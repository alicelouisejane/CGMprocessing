#' @title cleanCGM
#'
#' @description This function cleans and standardizes raw Continuous glucose monitoring (CGM) devices used in diabetes management. This function is optimised for raw Dexcom or libre data however has some functionality to handle pre-aggregated trial data bases.
#' Remember to always check your data structures and follow the guide in the README for more help optimizing code for your specific needs.
#' @returns Returns individual files (or aggregated depending on inital structure) of CGM data that have variable names standardized, missing glucose removed, high and low limits re coded to specific sensor limits. Option of "other" in
#' argument device is specific to data that is pre-aggregated into one sheet (ie multiple device uploads + multiple people in 1 file) and may use CGM with calibrations (for eg JAEB data).
#' detailed output of percentage wear and sensor drop out and optional CGM summary graphs created.
#' Cleaned files can then be run through the analyseCGM and/or exercise_split functions.
#'
#' @param inputdirectory path to folder containing raw files. If data is pre-aggregated then use the full file path including file name of this file. Preferred csv format but can read in others.
#'
#' @param outputdirectory path to folder where cleaned output files will be stored
#'
#' @param cgmdictionaryfile Optional. A string indicating the full path to a custom 'cgmvariable_dictionary.xlsx' file. See README on how to construct.
#' If NULL, the default file included with the package is used.
#'
#' @param device Use "other" if CGM required calibrations. Options are can be any dexcom (g4,g6etc), libre (1,2,pro), or "other." Important to specify what device is being used for the alignment of standardized variable names and to ensure sensor limits, ensure device type is also in the cgm dictionary next to variables.
#'  "Other" is specified if dealing with data that requires calibration or is in an alternative layout. Gap testing and standard percentage wear and droupout measures will not be created with this but number of days worn will be reported. Edit code as necessary if sensor is other and calibration is false
#'
#' @param combined TRUE/FALSE. Default is FALSE. This aims to handle pre aggregated data where more than one individual/visit etc. is in the file, most likely from external clinical study databases. Before running this code please combine patient ID, visit name and device id into one Id type variable. Still specify all variable names in the CGM dictionary.
#'
#' @param calibration only works for device = "other" TRUE/FALSE Default is FALSE. The majority of CGM data will not require a calibration, this is sensor dependant. For guardian sensors, dexcom g4 (and older generations) and medtronic ipro2 blood calibrations were required. The layout of the data may effect how this argument works please ensure calibration blood glucoses are labelled in a recordtype variable in the raw data (ie. in JAEB data)
#'
#' @param removerow TRUE/FALSE Default is TRUE. used when raw data has additional first row before variables. Usually encountered in freestyle libre. If sure this doesn't exist use F. Default to T otherwise which will check if row exists and act accordingly
#'
#' @param nrow used in conjunction with removerow=T. Number of rows to removed before variable names. Requires inspection of data files
#'
#' @param expectedwear Default is full, can acccept a numeric. This indicates if a participant was expected to wear the CGM for the lifetime of the sensor or for a particular number of days in a study. Enter the number of days expected as a numeric if so.
#'
#' @param saveplot TRUE/FALSE Default is TRUE. Save the overall CGM plot over the total number of days of wear. Will not generate if device type is "other"
#'
#' @param impute - for development. needs fixing and implementing correctly
#'
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate summarise n lead across contains filter select group_by inner_join slice ungroup arrange bind_rows rename
#' @import dplyr
#' @import tidyr
#' @import utils
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @import ggplot2
#' @import hms
#' @importFrom tools file_path_sans_ext
#' @importFrom anytime anytime
#' @import stats
#' @importFrom here here
#' @import stringr
#' @import data.table
#'
#' @author Alice Carr
#'
#' @export
#'
#' @seealso
#' analyseCGM and exercise_split
#'

cleanCGM <- function(inputdirectory,
                     outputdirectory,
                     cgmdictionaryfile=NULL,
                     device = "other",
                     combined = F,
                     calibration = F,
                     removerow = F,
                     nrow = 3,
                     expectedwear = "full",
                     impute = F,
                     saveplot = F) {

    # output directory is created and lists initialised
  base::dir.create(file.path(paste0(outputdirectory,"data-clean/")), showWarnings = FALSE)
  base::dir.create(file.path(paste0(outputdirectory,"additional/")), showWarnings = FALSE)

  if( saveplot==T){
    base::dir.create(file.path(paste0(outputdirectory,"graphs/")), showWarnings = FALSE)
  }

  gaptestoutput <- list()
  data_collected_output <- list()

  if (device != "other" & combined == T) {
    stop(print("Only use combined=TRUE with device=other. Separate raw download files are expected for use with device options dexcom or libre. See README for help"))
  } else if (combined == F) {

    if (is.null(cgmdictionaryfile)) {
      cgmdictionaryfile <- system.file("extdata", "cgmvariable_dictionary.xlsx", package = "CGMprocessing")
    }
    cgm_dict <- rio::import(cgmdictionaryfile, guess_max = 10000000)

    # Read in data: anticipated structure is a single file containing raw CGM downloads per individual
    files <- base::list.files(path = inputdirectory, full.names = TRUE)

  } else if (device == "other" & combined == T) {
    if (is.null(cgmdictionaryfile)) {
      cgmdictionaryfile <- system.file("extdata", "cgmvariable_dictionary.xlsx", package = "CGMprocessing")
    }
    cgm_dict <- rio::import(cgmdictionaryfile, guess_max = 10000000)

    # Read in data: anticipated structure here is a preproccessed dataframe of combined CGM from a study
    files <- inputdirectory
  }

  # Step 1: clean the CGM data
  for (f in 1:base::length(files)) {
    if (combined == F) {
      # id from filename (used only in dexcom and libre if device is other then this is irrelavent)
      Id <- tools::file_path_sans_ext(basename(files[f]))
      # Id <- gsub("^([^_]+_[^_]+).*", "\\1", Id)
      print(Id)
      table <- base::suppressWarnings(rio::import(files[f], guess_max = 10000000))
    } else if (device == "other" & combined == T) {
      table <- base::suppressWarnings(rio::import(files))
    }


    # if dealing with some libre raw data there may be additional rows added,
    # if youre certain there arent then use FALSE otherwise TRUE can handle both ways
    if (removerow == T) { # this was causing some issues in the previous if call: & grepl("V[0-9]",names(table)[1])
      names(table) <- table[nrow, ]
      table <- table[-(1:nrow), ]
    } else if (removerow == T | !grepl("V[0-9]", names(table)[1])) {
      table <- table
    } else if (removerow == F) {
      table <- table
    }

    # indicates what device we are using the data from, possibly important to keep track of
    cgm_dict <- dplyr::filter(cgm_dict, type==device)
    device_vars <- cgm_dict[cgm_dict$old_vars %in% names(table), ]

    cgm_dict <- device_vars

    # rename the variables to standardised variables names
    colnames(table) <- dplyr::recode(
      colnames(table),
      !!!stats::setNames(as.character(cgm_dict$new_vars), cgm_dict$old_vars)
    )

    # try to anticipate problematic dates
    if (is.character(table$timestamp)) {
      table$timestamp <- stringr::str_replace_all(table$timestamp, "T", " ")
      table$timestamp <- as.POSIXct(lubridate::parse_date_time(table$timestamp, orders = c("ymd HMS", "dmy HMS", "dmy HM", "mdy HMS", "mdy HM")), tz = "UTC")
      table$timestamp <- anytime::anytime(table$timestamp, tz = "UTC")
    } else if (!is.character(table$timestamp)) {
      table$timestamp <- anytime::anytime(table$timestamp, tz = "UTC")
    }


    # this get rid of the first lines in dexcom as all these rows miss a timestamp
    # but also gets rid of any problematic missing rows
    table <- dplyr::filter(table, !is.na(timestamp))

    # keep only variables of interest
    vars_to_keep <- dplyr::intersect(names(table), unique(cgm_dict$new_vars))

    table <- dplyr::select(table, c(all_of(vars_to_keep)))


    if (combined == F) {
      # order by timestamp
      table$subjectid <- Id
      table <- tidyr::unite(table,id,subjectid, deviceid)
      if (length(unique(table$id)) > 1) {
        table <- table[order(table$id, table$timestamp), ]
        print(paste("There is likely more than one Device ID in this individuals file. Check this ID if you are not expecting this:", Id))
      } else if (length(unique(table$id)) == 1) {
        table <- table[base::order(table$timestamp), ]
      }
    } else if (combined == T) {
      table <- table[order(table$id, table$timestamp), ]
      Id <- "combined_data"
    }


    # find what the interval in the data is ie. 5min for dexcom 15 min for libre
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp) / 60))



    # high and low limits from :
    # https://uk.provider.dexcom.com/sites/g/files/rrchkb126/files/document/2021-09/LBL017451%2BUsing%2BYour%2BG6%2C%2BG6%2C%2BUK%2C%2BEN%2C%2Bmmol_0.pdf
    # suggests libre 2 has different limits to libre 1 but this is not the case whan i look at my own data
    # https://www.freestyle.abbott/us-en/support/faq.html?page=device/freestyle-libre-2-system/faq/topic/reader

    if (grepl("dexcom", device)) {
      # change instances of low/ high to sensor limits
      table$sensorglucose <- as.character(table$sensorglucose)
      # DO NOT INCLUDE ANY OTHER RECODS OTHER THAN CGM ie. NOT calibration value for dexcom g5 - g4 is different and requires manual calibrations as below
      table <- dplyr::filter(table, eventtype == "EGV")
      table <- dplyr::select(table, -c(eventtype))
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("low", sensorglucose, ignore.case = TRUE) ~ "2.2",
          grepl("high", sensorglucose, ignore.case = TRUE) ~ "22.2",
          TRUE ~ table$sensorglucose
        ))
      )
      # for plotting:
      sensormin <- 2.2
      sensormax <- 24
    } else if (grepl("libre", device)) {
      # DO NOT INCLUDE ANY OTHER RECODS OTHER THAN CGM ie. NOT scanglucose
      table <- dplyr::filter(table, eventtype == 0)
      table <- dplyr::select(table, -c(eventtype))
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("lo", sensorglucose, ignore.case = TRUE) ~ "2.2",
          grepl("hi", sensorglucose, ignore.case = TRUE) ~ "27.8",
          TRUE ~ table$sensorglucose
        ))
      )
      # for plotting:
      sensormin <- 2.2
      sensormax <- 28
    } else if (device == "other") {
      sensormin <- 2.2
      sensormax <- 28
    }


    # make sure glucose is numeric
    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))
    # convert to mmol/l - if tests if glucose is mg/dl as would have higher max value than what would be max in mmol/l
      table$sensorglucose <- ifelse(table$sensorglucose>30,round(table$sensorglucose / 18, digits = 2),table$sensorglucose)

    if (unique(device_vars$type) == "other" | unique(device_vars$type == "dexcomg4")) {
      # if device type is "other" then calibration is likely necessary this is the loop it will be handled in
      # first identify record types that are calibration and remove them from the main table

      if (calibration == T) {
        calibration <- dplyr::filter(table, grepl("calib", table$recordtype, ignore.case = T)) %>%
          dplyr::rename("timestampfp" = "timestamp") %>%
          dplyr::rename("fingerprickglucose" = "sensorglucose") %>%
          data.table::as.data.table() %>%
          dplyr::mutate(timestampfp_copy = timestampfp)

        table_cal <- dplyr::filter(table, !grepl("calib", table$recordtype, ignore.case = T)) %>%
          data.table::as.data.table() %>%
          dplyr::mutate(date = as.Date(timestamp))

        # Set keys for data.tables
        data.table::setkey(calibration, id, timestampfp)
        data.table::setkey(table_cal, id, timestamp)

        # Perform a rolling join to merge the data.tables based on the closest timestamp
        calibration_merged <- table_cal[calibration, roll = "nearest"] %>%
          dplyr::mutate(diff = as.numeric(abs(timestamp - timestampfp_copy))) %>%
          dplyr::mutate(nocalibration = ifelse(diff > 900, 1, NA)) %>% # if the matching glucose is > 15 mins then ensure to mark
          dplyr::filter(is.na(nocalibration)) %>% # remove these
          dplyr::ungroup() %>%
          dplyr::mutate(date = as.Date(timestamp)) %>%
          dplyr::group_by(id, date) %>%
          dplyr::mutate(MD = mean(abs((fingerprickglucose - sensorglucose) / fingerprickglucose) * 100)) %>%
          dplyr::mutate(remove = ifelse(floor(MD) > 20, 1, NA)) %>%
          dplyr::mutate(calibrationperformed = 1) %>%
          dplyr::select(id, remove, date, MD, calibrationperformed) %>%
          dplyr::group_by(id, date) %>%
          dplyr::mutate(num_calibrations_perday = sum(calibrationperformed, na.rm = T)) %>%
          unique()

        datesincgm <- table_cal %>%
          dplyr::mutate(date = as.Date(timestamp)) %>%
          dplyr::select(id, date) %>%
          unique() %>%
          base::merge(calibration_merged, by = c("id", "date"), all = T) %>%
          dplyr::mutate(remove = ifelse(is.na(MD), 1, remove)) %>% # if missing MD this means calibration were not perfomed on this day. usually calibrations are done everyday in those sensor that required calibration. these dates should also be removed from CGM
          dplyr::group_by(id) %>%
          dplyr::mutate(num_days_wear = dplyr::n()) %>%
          dplyr::mutate(num_days_calibration = sum(calibrationperformed, na.rm = T)) %>%
          dplyr::mutate(num_days_remove_calibration = sum(remove, na.rm = T)) %>%
          dplyr::mutate(average_calibrations_perday_overtimeofwear = mean(num_calibrations_perday, na.rm = T))

        remove <- dplyr::select(datesincgm, id, remove, date) %>%
          dplyr::filter(remove == 1)

        # remove bad calibrated days/ days where no calibration was performed
        table <- table_cal %>%
          base::merge(remove, by = c("id", "date"), all = T) %>%
          dplyr::filter(is.na(remove)) %>%
          dplyr::select(-remove)

        table <- table[base::order(table$id, table$timestamp), ]
        # assessing sensor drop out and not taking into account days removed because of calibration
        gaptest <- table %>%
          dplyr::group_by(id) %>%
          dplyr::mutate(diff = as.numeric(difftime(timestamp, dplyr::lead(timestamp), units = "mins"))) %>%
          dplyr::mutate(gap = ifelse(abs(diff) > interval + 1, 1, 0)) %>%
          dplyr::filter(gap == 1) %>%
          dplyr::mutate(gapcount = sum(gap)) %>%
          dplyr::mutate(gaptime = paste(timestamp, "length:", abs(diff), "mins")) %>%
          dplyr::select(gaptime, gapcount, timestamp, diff,sensorglucose) %>%
          dplyr::filter(diff / 60 > 23.5) # focuses only on gaps that are from either the same time of wear or removes where a gap now exisits becuase of calibration removal

        # store this as a dataframe
        gaptestoutput[[f]] <- table %>%
          dplyr::group_by(id) %>%
          dplyr::mutate(diff = as.numeric(difftime(timestamp, lead(timestamp), units = "mins"))) %>%
          dplyr::mutate(gap = ifelse(abs(diff) > interval + 1, 1, 0)) %>%
          dplyr::filter(gap == 1) %>%
        dplyr::select(id, timestamp, diff)

        # you can edit this code further to include measures for percentage expected wear etc depending on your needs
        # as this data is expected to be coalesed there with be multiple sensor uploads from the same person
        # unsure on what the expected time of wear should therefore be but if you know this then edit as neccessary follwing the loops that are below

        # summarise data collected -  different for when processing raw data
        # and is aimed at when all data was put into 1 dataframe like in JAEB and not separate files
        # and if there were calibrations performed
        data_collected <- datesincgm %>%
          dplyr::select(id, num_days_wear, num_days_calibration, num_days_remove_calibration, average_calibrations_perday_overtimeofwear) %>%
          unique()

        data_collected_output[[f]] <- data_collected
      } else if (calibration == F) {
        table <- table

        gaptest <- table %>%
          dplyr::group_by(id) %>%
          dplyr::mutate(diff = as.numeric(difftime(timestamp, lead(timestamp), units = "mins"))) %>%
          dplyr::mutate(gap = ifelse(abs(diff) > interval + 1, 1, 0)) %>%
          dplyr::filter(gap == 1) %>%
          dplyr::mutate(gapcount = sum(gap)) %>%
          dplyr::mutate(gaptime = paste(timestamp, "length:", abs(diff), "mins")) %>%
          dplyr::select(id, gaptime, gapcount, timestamp, diff,sensorglucose)

        # store this as a dataframe
        gaptestoutput[[f]] <- gaptest
      }
    } else if (device != "other") {
      gaptest <- table %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(diff = as.numeric(difftime(timestamp, lead(timestamp), units = "mins"))) %>%
        dplyr::mutate(gap = ifelse(abs(diff) > interval + 1, 1, 0)) %>%
        dplyr::filter(gap == 1) %>%
        dplyr::mutate(gapcount = sum(gap)) %>%
        dplyr::mutate(gaptime = paste(timestamp, "length:", abs(diff), "mins")) %>%
        dplyr::select(id, gaptime, gapcount, timestamp, diff,sensorglucose)

      gaptestoutput[[f]] <- gaptest
    }


    if (!is.na(unique(cgm_dict$expecteddaysofwear)) | !is.na(expectedwear)) {
      if (expectedwear == "full") {
        expectedtime <- 24 * unique(cgm_dict$expecteddaysofwear) # 24hr * number of days from file
        expectedtime_mins <- 60 * 24 * unique(cgm_dict$expecteddaysofwear)
      } else if (is.numeric(expectedwear)) {
        expectedtime <- 24 * expectedwear
        expectedtime_mins <- 60 * 24 * expectedwear
      }

      if (nrow(gaptest) > 0) {

        # interpolate gaps <= 20 min
        # Create empty data frame to store interpolated rows
        interpolated_df <- gaptest

        # Calculate number of rows to insert
        #interval is the time one row is equivelent to in mins
        interpolated_df$num_rows <- floor(round(abs(interpolated_df$diff),digits = 0)/interval)-1

        #keep only the timestamps where the gap was <20 min
        interpolated_df<-dplyr::filter(interpolated_df,abs(diff)<=20)
        interpolated_rows<-list()
        # Iterate over rows in the input data frame
        for (i in seq_len(nrow(interpolated_df))) {
          # Generate interpolated timestamps

          # Create data frame with interpolated timestamps and sensor values
          interpolated_rows[[i]] <- data.frame(
            timestamp = seq(interpolated_df$timestamp[i], by = "5 mins", length.out = interpolated_df$num_rows[i] + 1)[-1],
            sensorglucose = interpolated_df$sensorglucose[i],
            id=interpolated_df$id[i],
            num_gaps_interpolated=nrow(interpolated_df),
            minutes_interpolated=sum(interpolated_df$num_rows)*5
          )
        }

        table_interpolated<-dplyr::bind_rows(interpolated_rows)

        percentageexpectedwear <- table

        percentage_dropout <- gaptest %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(totallosttime = sum(abs(diff)))

        percentageexpectedwear <- table %>%
          dplyr::group_by(id) %>%
          dplyr::mutate(percentage_expectedwear_overstudy = as.numeric((round(difftime(max(timestamp), min(timestamp), units = "hours")) / expectedtime) * 100)) %>%
          select(id, percentage_expectedwear_overstudy) %>%
          unique() %>%
          base::merge(percentage_dropout, by = "id", all = T) %>%
          mutate(totallosttime = ifelse(is.na(totallosttime), 0, totallosttime)) %>%
          mutate(percentage_datacollected_overstudy = ((expectedtime_mins - totallosttime) / expectedtime_mins) * 100) %>%
          mutate(percentage_dropout_overstudy = (totallosttime / expectedtime_mins) * 100) %>%
          merge(unique(select(table_interpolated,id,num_gaps_interpolated,minutes_interpolated)))

        data_collected_output[[f]] <- percentageexpectedwear
      } else if (nrow(gaptest) == 0) { # there were no gaps in wear
        percentageexpectedwear <- table %>%
          dplyr::group_by(id) %>%
          dplyr::mutate(percentage_expectedwear_overstudy = as.numeric((round(difftime(max(timestamp), min(timestamp), units = "hours")) / expectedtime) * 100)) %>%
          select(id, percentage_expectedwear_overstudy) %>%
          unique() %>%
          mutate(totallosttime = 0) %>%
          mutate(percentage_datacollected_overstudy = 100) %>%
          mutate(percentage_dropout_overstudy = 0) %>%
          mutate(num_gaps_interpolated=0) %>%
          mutate(minutes_interpolated=0)

        data_collected_output[[f]] <- percentageexpectedwear
      }
    } else if (is.na(unique(cgm_dict$expectedwear)) & expectedwear == "full") {
      warning("Missing value for expected wear in CGM dictionary file. Unable to generated data collected output. Please update dictionary accordingly or use expectedwear function argument.")
      data_collected_output[[f]] <- NULL
    }

      table<-merge(table,select(table_interpolated,id,timestamp,sensorglucose),all=T) %>%
        dplyr::group_by(id) %>%
        dplyr::arrange(timestamp)

    table <- dplyr::filter(table, !is.na(table$sensorglucose))


    if (combined == F) {

      #for IQR summary ribbons round time up to the nearest 5 min to make a neater summary line
    summary_table<-table %>%
      dplyr::mutate(date = base::as.Date(timestamp)) %>%
      dplyr::mutate(time = hms::as_hms(timestamp)) %>%
      dplyr::mutate(test=hms::round_hms(time, secs = interval*60))

      graph1 <- table %>%
        dplyr::mutate(date = base::as.Date(timestamp)) %>%
        dplyr::mutate(time = hms::as_hms(timestamp)) %>%
        ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = as.numeric(sensorglucose))) +
        ggplot2::geom_path(ggplot2::aes(group = as.factor(date), colour = as.factor(date)), colour = "grey") +
        ggplot2::labs(x = "Time", y = "Glucose", title = paste("Summary of CGM wear over:",length(unique(base::as.Date(table$timestamp))),"days; total time:", as.numeric(round(difftime(max(table$timestamp), min(table$timestamp), units = "hours"))), "hours", "\n Raw data collected:", round(mean(percentageexpectedwear$percentage_datacollected_overstudy, na.rm = T)), "%")) +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
        # median hi low and IQR could be the same as each other...
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "10-90th Centile"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.1, 0.9))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "25-75th Centile"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.25, 0.75))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "Median"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.5, 0.5))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose)), fun = "median", geom = "line", alpha = 0.5, colour = "orange",linewidth=1, show.legend = F) +
        ggplot2::theme(
          legend.position = c(0.85, 0.9), # Adjust the position of the legend box (top-right corner)
          legend.background = ggplot2::element_rect(fill = "white", color = "black"), # Customize the legend box
          legend.key.size = ggplot2::unit(0.5, "cm"), # Adjust the size of the legend keys
          legend.text = ggplot2::element_text(size = 10), # Adjust the size of the legend text
          legend.title = ggplot2::element_text(size = 12, face = "bold") # Adjust the size and style of the legend title
        ) +
        ggplot2::scale_fill_manual("Key", values = c("lightblue", "darkblue","orange"),aesthetics = c("fill")) +
        ggplot2::scale_y_continuous(limits = c(2, (sensormax)), breaks = c(seq(2, sensormax, 2)))

      graphoutput_title <- cowplot::ggdraw(cowplot::plot_grid(
        NULL,
        graph1,
        ncol = 1,
        rel_heights = c(0.07, 0.93)
      )) +
        cowplot::draw_label(paste("Patient ID:", Id), x = 0.5, y = 0.95, hjust = 0.5, fontface = "bold", size = 14)

      if (saveplot == T) {

        # save the plot, all patients
        ggplot2::ggsave(paste0(outputdirectory,"graphs/", Id, "_summaryCGM.pdf"), graphoutput_title, width = 6, height = 6)
      }
    } else if (combined == T) {
      data_collected_output_final <- dplyr::bind_rows(data_collected_output[!sapply(data_collected_output, is.null)])

      summary_table<-table %>%
        dplyr::mutate(date = as.Date(timestamp)) %>%
        dplyr::mutate(time = hms::as_hms(timestamp)) %>%
        dplyr::mutate(test=hms::round_hms(time, secs = interval*60))

      graph1 <- table %>%
        dplyr::mutate(date = as.Date(timestamp)) %>%
        dplyr::mutate(time = hms::as_hms(timestamp)) %>%
        ggplot2::ggplot(ggplot2::aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = as.numeric(sensorglucose))) +
        ggplot2::geom_path(ggplot2::aes(group = interaction(as.factor(date), id), colour = as.factor(date)), colour = "grey") +
        ggplot2::labs(x = "Time", y = "Glucose", title = paste("Summary of CGM wear over study, N=",length(unique(table$id)), "\n Mean raw data collected over expected study time:", round(mean(data_collected_output_final$percentage_datacollected_overstudy, na.rm = T)), "%")) +
        ggplot2::theme_minimal() +
        ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
        # median hi low and IQR could be the same as each other...
        # median hi low and IQR could be the same as each other...
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "10-90th Centile"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.1, 0.9))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "25-75th Centile"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.25, 0.75))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "Median"),colour="transparent", fun.data = function(x) {
          y <- stats::quantile(x, c(0.5, 0.5))
          names(y) <- c("ymin", "ymax")
          y
        }, geom = "ribbon", alpha = 0.5, show.legend = T) +
        ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose)), fun = "median", geom = "line", alpha = 0.5, colour = "orange",linewidth=1, show.legend = F) +
        ggplot2::theme(
          legend.position = c(0.85, 0.9), # Adjust the position of the legend box (top-right corner)
          legend.background = ggplot2::element_rect(fill = "white", color = "black"), # Customize the legend box
          legend.key.size = ggplot2::unit(0.5, "cm"), # Adjust the size of the legend keys
          legend.text = ggplot2::element_text(size = 10), # Adjust the size of the legend text
          legend.title = ggplot2::element_text(size = 12, face = "bold") # Adjust the size and style of the legend title
        ) +
        ggplot2::scale_fill_manual("Key", values = c("lightblue", "darkblue","orange"),aesthetics = c("fill")) +
        ggplot2::scale_y_continuous(limits = c(2, (sensormax)), breaks = c(seq(2, sensormax, 2)))


      graph_list <- list()
      for (i in unique(table$id)) {
        data <- table %>%
          filter(id == i) %>%
          dplyr::mutate(date = as.Date(timestamp)) %>%
          dplyr::mutate(time = hms::as_hms(timestamp))

        summary_table<-data %>%
          dplyr::mutate(date = as.Date(timestamp)) %>%
          dplyr::mutate(time = hms::as_hms(timestamp)) %>%
          dplyr::mutate(test=hms::round_hms(time, secs = interval*60))

        graph_list[[i]] <- ggplot2::ggplot(data = data, ggplot2::aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = as.numeric(sensorglucose))) +
          ggplot2::geom_path(ggplot2::aes(group = interaction(as.factor(date), id), colour = as.factor(date)), colour = "grey") +
          ggplot2::labs(x = "Time", y = "Glucose", title = paste("Patient ID:", i, "\n Summary of CGM wear over:",length(unique(data$date)),"days; total time:", as.numeric(round(difftime(max(data$timestamp), min(data$timestamp), units = "hours"))), "hours", "\n Raw data collected over expected study time:", round(data_collected_output_final$percentage_datacollected_overstudy[data_collected_output_final$id == i]), "%")) +
          ggplot2::theme_minimal() +
          ggplot2::scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
          # median hi low and IQR could be the same as each other...
          ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "10-90th Centile"),colour="transparent", fun.data = function(x) {
            y <- stats::quantile(x, c(0.1, 0.9))
            names(y) <- c("ymin", "ymax")
            y
          }, geom = "ribbon", alpha = 0.5, show.legend = T) +
          ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "25-75th Centile"),colour="transparent", fun.data = function(x) {
            y <- stats::quantile(x, c(0.25, 0.75))
            names(y) <- c("ymin", "ymax")
            y
          }, geom = "ribbon", alpha = 0.5, show.legend = T) +
          ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose),fill = "Median"),colour="transparent", fun.data = function(x) {
            y <- stats::quantile(x, c(0.5, 0.5))
            names(y) <- c("ymin", "ymax")
            y
          }, geom = "ribbon", alpha = 0.5, show.legend = T) +
          ggplot2::stat_summary(data = summary_table,ggplot2::aes(x = as.POSIXct(test, format = "%H:%M:%S"), y = as.numeric(sensorglucose)), fun = "median", geom = "line", alpha = 0.5, colour = "orange",linewidth=1, show.legend = F) +
          ggplot2::theme(
            legend.position = c(0.85, 0.9), # Adjust the position of the legend box (top-right corner)
            legend.background = ggplot2::element_rect(fill = "white", color = "black"), # Customize the legend box
            legend.key.size = ggplot2::unit(0.5, "cm"), # Adjust the size of the legend keys
            legend.text = ggplot2::element_text(size = 10), # Adjust the size of the legend text
            legend.title = ggplot2::element_text(size = 12, face = "bold") # Adjust the size and style of the legend title
          ) +
          ggplot2::scale_fill_manual("Key", values = c("lightblue", "darkblue","orange"),aesthetics = c("fill")) +
          ggplot2::scale_y_continuous(limits = c(2, (sensormax)), breaks = c(seq(2, sensormax, 2)))

        if (saveplot == T) {
          ggplot2::ggsave(paste0(outputdirectory,"graphs/", i, "_summaryCGM.pdf"), graph_list[[i]], width = 6, height = 6)
        }
      }

      if (saveplot == T) {
        # save the plot, all patients
        ggplot2::ggsave(paste0(outputdirectory,"graphs/summaryCGM_allstudy.pdf"), graph1, width = 6, height = 6)
      }
    }

    # output
    table$date <- as.Date(table$timestamp)
    table <- dplyr::select(table, c(id, date, timestamp, sensorglucose))
    rio::export(table, file = base::paste0(outputdirectory,"data-clean/", Id, "_cleaned.csv"))
  }

  gaptestfinaloutput <- dplyr::bind_rows(gaptestoutput[!sapply(gaptestoutput, is.null)])

  rio::export(gaptestfinaloutput, file=paste0(outputdirectory,"additional/gap_info.csv"))

  data_collected_output_final <- dplyr::bind_rows(data_collected_output[!sapply(data_collected_output, is.null)])
  rio::export(data_collected_output_final, file=paste0(outputdirectory,"additional/percentage_data_collected_info.csv"))
}
