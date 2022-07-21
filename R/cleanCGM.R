#' Clean raw (or in house preprocessed) CGM files beofre analysis
#'
#' @param inputdirectory path to folder containing raw (or in house preprocessed) files. Prefered csv format but could read in others.

#' @param outputdirectory path to folder where cleaned output files will be stored

#' @param calibrationcheck Default TRUE. This calibration was sensor dependant (Dexcom G4) againsted logged fingerstick readings
#'  and nearest 15 min later sensor reading. Calibration excluded 1) whole traces if 2 blood glucose calibrations were not
#'  completed at the start of the sensor wear, 2) a day of wear if the MARD of the sensor glucose and blood glucose
#'   calibration on that day is >20% or if <2 blood glucose calibrations were completes on that day. This can be set to false for CGM
#'   without fingerstick calibration. Check if layout of input files is correct if using this calibration as TRUE

#' @param sensortype Type of sensor used options are dexcom, libre or other. Default is other due to the nature of the files
#'  having to be preprocessed due start date errors. Should set to dexcom or libre the raw csv files should be able to be read in.

#' @param select7days Quirk of inhouse data was some data collection meant there were longer than 7 days of collection stored on the system
#' we therefore use this to take the first 7 days of data if data was >8 days collected (accuracy of sensor decreases after 8 days)
#'
#' @param calibrationoutput if calibration check is TRUE ensure to have output directory for calibration files. This will output the
#' calibration table of fingerstick matched to nearest 15 min later sensor glucose with the correlation
#' (checking there were 2 fingersticks per day) and the MARD between the sensor and fingerstick
#'
#' @importFrom utils write.csv
#' @importFrom rio import export
#' @importFrom dplyr mutate across contains if_else filter select ifelse

#' @author Alice Carr
#' @return
#' @export
#'
#' @examples
#' cleanCGM("CGMprocessing/data-preprocessed/","CGMprocessing/data-clean",T,"other",T,"CGMprocessing/calibration")
#'
#'Notes: add a option for expected day wear for the sensor / depending on what sensor selected, double check the percentage wear calcs

inputdirectory<-"EXTOD/data-preprocessed"
cleanCGM <- function(inputdirectory,
                     outputdirectory = tempdir(),
                     calibrationcheck = TRUE, sensortype = "other", select7days = T, calibrationoutput = tempdir()) {

  #Read in file list. Create output

  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  base::dir.create(outputdirectory, showWarnings = FALSE)
  dateparseorder <- c(
    "mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "dmy HM", "dmy HMS",
    "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS", "ymd HM", "ymd HMS",
    "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS"
  )
  #cgm variable dictionary
  cgm_dict<-rio::import("cgmvariable_dictionary.xlsx")
  # Read in data, depending on CGM type.
  for (f in 1:base::length(files)) {

    #id from filename
    Id <- base::unlist(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_"))[1]

    # other means ive ran it though split files function as there were dates in there that were not right start date.
    #This is could be included in this function if a persistant error
    if (sensortype == "dexcom") {
      table <-  rio::import(files[f], guess_max = 10000000)
      names(table)<- tolower(names(table))
      colnames(table) <- dplyr::recode(
        colnames(table),
          !!!setNames(as.character(cgm_dict$new_vars), cgm_dict$old_vars)
      )
      vars_to_keep <- intersect(names(table), unique(cgm_dict$new_vars))
      table<-dplyr::select(table,all_of(vars_to_keep))
      #chnage sensor id to be patient id
      table$id <- Id

      #change instances of low/ high to sensor limits
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          sensorglucose == "Low" ~ "2.2",
          sensorglucose == "High" ~ "22.2",
          TRUE ~ table$sensorglucose
        ))
      )

      # date time conversion
      table$timestampfp <- base::as.POSIXct(lubridate::parse_date_time(
        table$timestampfp,
        dateparseorder
      ), tz = "UTC")


      table$fingerprickglucose <-
        base::suppressWarnings(base::round(base::as.numeric(table$fingerprickglucose), digits = 2))

    } else if (sensortype == "libre") {
      table <-  rio::import(files[f], skip =2 ,guess_max = 10000000)
      names(table)<- tolower(names(table))
      colnames(table) <- dplyr::recode(
        colnames(table),
        !!!setNames(as.character(cgm_dict$new_vars), cgm_dict$old_vars)
      )
      table$id <- Id
      vars_to_keep <- intersect(names(table), unique(cgm_dict$new_vars))
      table<-dplyr::select(table,all_of(vars_to_keep))
      #keep commented out until gap interpolation is solved
      #table$sensorglucose <- ifelse(table$`recordtype` == 1, table$`scanglucose`, table$`sensorglucose`)
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          sensorglucose == "LO" ~ "2.2",
          sensorglucose == "HI" ~ "27.8",
          TRUE ~ table$sensorglucose
        ))
      )


      table<- table %>% arrange((timestamp)) #order on timestamp

      #adds dummy 5 min data by adding 2 rows after every original row that is the same as the original row
      # FOR DEVELOPMENT- here we could look at interpolation of data only interpolating up to 20mins of gap perhaps
      table<-slice(table,rep(1:n(), each = 3))

      # sensor type other means ive ran it though split files function as there were dates in there that were not right start date.
      #This need probably incuding in this whole function
    } else if (sensortype == "other") {
      table <- read.csv(files[f])
      table <- table[, c("id", "timestampfp", "fingerprickglucose", "timestamp", "sensorglucose")]
      table$timestampfp <- base::as.POSIXct(lubridate::parse_date_time(
        table$timestampfp,
        dateparseorder
      ), tz = "UTC")


      table$fingerprickglucose <-
        base::suppressWarnings(base::round(base::as.numeric(table$fingerprickglucose), digits = 2))
      table$id <- Id
    }


    # Make sensor glucose numeric, sort table by timestamp, remove duplicate rows.
    # If necessary, remove rows with no data.
    table <- table[-c(max(base::which(!is.na(table$sensorglucose))) + 1:nrow(table)), ]
    table$timestamp <- base::as.POSIXct(lubridate::parse_date_time(
      table$timestamp,
      dateparseorder
    ), tz = "UTC")

    table$timestamp <- base::as.POSIXct(lubridate::parse_date_time(
      table$timestamp,
      dateparseorder
    ), tz = "UTC")

    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))
    table <- table[base::order(table$timestamp), ]

    # ***Make record start time
    recordstart <- min(table$timestamp)
    recordstart <- base::as.POSIXct(recordstart, tz = "UCT", format = "%m/%d/%Y %H:%M:%OS")

    removaltime <- base::as.POSIXlt(max(table$timestamp), tz = "UCT", format = "%m/%d/%Y %H:%M:%OS")

    # Set interval based on mode of timestamp diff. this should always be 5 mins / 300s
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))

    # prints dates in each file for debugging
    printdates <- as.Date(table$timestamp)
    print(base::paste(files[f], "=", unique(printdates), sep = ""))


    ################ Calculate percentage expected wear##############################################

    # percenageexpectedwear we expect them to have 7 days of data if a CGM
    # for those that dont then this is possibly an error in data collection...
    # take the first 7 days of data
    # Total time units in secs
    time <- table
    totaltime <- difftime(max(time$timestamp), min(time$timestamp), units = "secs")
    totaltime <- as.numeric(totaltime)
    totaltimedays <- difftime(max(time$timestamp), min(time$timestamp), units = "days")

    #percentage expected wear without cutting at 7 days
    percenageexpectedwear <- ((totaltime / 86400) / 7) * 100

    # Selects the first 7 days of wear
    if (select7days == T) {
      if (totaltimedays > 8) {
        table <- time %>% dplyr::filter(dplyr::between(as.Date(timestamp), as.Date(recordstart), as.Date(recordstart) + lubridate::days(7)))
        #percentage expected wear cutting at 7 days (if select 7 days )
        totaltime7datcut <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")
        totaltime7datcut <- as.numeric(totaltime7datcut)
        totaltime7datcutdays <- difftime(max(table$timestamp), min(table$timestamp), units = "days")
        percenageexpectedwear_7daycut <- ((totaltime7datcut / 86400) / 7) * 100

         } else {
        table <- time
        percenageexpectedwear_7daycut <- NA
      }
    }

    ################################# calibration step ################################

    # for percentage of data kept post calibration work out sum of table before
    totaltimebefore <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")


    # Dexcom G4 sensor this is always true
    if (calibrationcheck == TRUE) {
      # split into two dataframes for easier analysis
      df1 <- table[c("id", "fingerprickglucose", "timestampfp")]
      df1 <- df1[!is.na(df1$fingerprickglucose), ]
      df2 <- table[c("id", "sensorglucose", "timestamp")]
      df2 <- df2[!is.na(df2$sensorglucose), ]
      # find closest times of finger prick to the cgm and create a data frame of these called calibration
      # diff is in seconds
      calibration <- df1 %>%
        dplyr::group_by(id, timestampfp) %>%
        dplyr::inner_join(df2, by = "id") %>%
        dplyr::mutate(diff = abs(timestampfp + 10 * 60 - timestamp)) %>%
        # 10*60 is the time delay as finger stick glucose is ~15mins delayed from interstital

        dplyr::arrange(diff) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      calibration <- calibration[!is.na(calibration$`fingerprickglucose`), ]

      # make a date only column
      calibration$Date <- as.Date(calibration$timestampfp)

      # find correlation based on the date between the finger stick and cgm values
      # returns a data frame called correlation which has the correlations for each day
      # check element 3 of this dataframe, if missing then there are no start claibrations after the set up and therefore these days
      # would be removed
      if (!is.na(calibration$Date[3] == calibration$Date[1])) {
        calibrationgo <- "ok"
      } else {
        print(base::paste("File '", files[f], "' does not have 2 start calibrations", sep = ""))
        next
      }


      calibration$diff2 <- NA
      for (i in 2:nrow(calibration)) {
        calibration$diff2[i] <- difftime(calibration$timestampfp[i], calibration$timestampfp[i + 1], units = "hours")
      }

      require(plyr)

      func <- function(calibration) {
        if (dim(calibration)[1] > 1) {
          model <- lm(`fingerprickglucose` ~ `sensorglucose` + 0, calibration)
          coef <- model$coefficients
          coeffequal1 <- ifelse(coef > 0.9 & coef < 1.1, 1, 0)
        }
        else {
          coeffequal1 <- 0
          coef <- 0
        }
        return(data.frame(coeffequal1, coef))
      }

      correlation <- ddply(calibration, .(Date), func)

      # find mean absolute percentage difference between the finger stick and cgm values
      # returns a data frame called MD which has the MAD for each day
      func <- function(calibration) {
        return(data.frame(MD = mean(abs((calibration$`fingerprickglucose` - calibration$`sensorglucose`) / calibration$`fingerprickglucose`) * 100)))
      }
      MD <- ddply(calibration, .(Date), func)

      quality <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), list(correlation, MD))

      ########################################## can change calibration value#########################################
      quality$remove <- ifelse(quality$coef == 0 | quality$MD > 20, 1, 0)

      # dates that werent included as not first 7 days data removed from calibration summary table
      if (select7days == T) {
        if (totaltimedays > 8) {
          quality <- quality %>% dplyr::filter(dplyr::between(as.Date(Date),as.Date(recordstart), as.Date(recordstart) + lubridate::days(7)))
        } else {
          quality <- quality
        }
      }

      # Inclusion critera:
      # If MAD >20 remove all from this date or they did not have 2 SMBG on this date (ie the coeefcient would be 0)

      # Find dates that dont fit this inclusion criteria and make df of these called remove
      remove <- quality %>%
        dplyr::filter(remove == 1) %>%
        dplyr::select(Date)
      remove <- unique(remove)
      print(remove$Date)

      table$Date <- as.Date(table$timestamp)

      # Remove the dates that dont meet inclusion critera
      table <- table[!(table$Date %in% remove$Date), ]
      # table$Date<-NULL

      # not enough data
      if (base::nrow(table) == 0) {
        print(base::paste(files[f], "not enought data"))
        next
      }
    }

    if (base::length(table$timestamp) == 0) {
      print(base::paste(files[f], "not enought data"))
      next
    }

    # data left post calibrtion
    totaltimeafter <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")

    totaltimebefore <- as.numeric(totaltimebefore)
    totaltimeafter <- as.numeric(totaltimeafter)

    percent_cgm_wear <- (totaltimeafter / totaltimebefore) * 100

    #amount of data left post calibration
    table$percent_cgm_wear <- percent_cgm_wear
    #amount of data collected out of 7 days
    table$percentage_expected_wear <- percenageexpectedwear

    if (select7days == T) {
      if (totaltimedays > 8) {
    table$percenageexpectedwear_7daycut <- percenageexpectedwear_7daycut
    table$totaltime7datcutdays <- as.numeric(totaltime7datcutdays)
      } else {
        table$percenageexpectedwear_7daycut <- NA
        table$totaltime7datcutdays <- NA
      }
    }

    # prints out percentage wear in command prompt
    print(base::paste(files[f], "=", percent_cgm_wear, sep = ""))
    table$Date <- as.Date(table$timestamp)
    tableout <- table[, c("id", "timestamp", "sensorglucose", "Date", "percent_cgm_wear", "percentage_expected_wear", "percenageexpectedwear_7daycut")]

    filename <- base::paste0(outputdirectory, "/", basename(files[f]))
    utils::write.csv(tableout, file = filename, row.names = FALSE)
    if (calibrationcheck == T) {
      base::dir.create(calibrationoutput, showWarnings = FALSE)
      filenamecalibration <- base::paste(calibrationoutput, base::unlist(tools::file_path_sans_ext(basename(files[f]))), "_calibration.csv", sep = "")
      utils::write.csv(quality, filenamecalibration, row.names = FALSE)
    }
  }
  closeAllConnections()
}
