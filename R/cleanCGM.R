#' Title
#'
#' @param inputdirectory
#' @param outputdirectory
#' @param calibrationcheck
#' @param sensortype
#' @param select7days
#'
#' @return
#' @export
#'
#' @examples
#'
cleanCGM <- function(inputdirectory,
                     outputdirectory = tempdir(),
                     calibrationcheck = TRUE, sensortype = "other", select7days = T) {

  # Set system locale to read all characters. Read in file list. Creat output
  require(dplyr)
  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  base::dir.create(outputdirectory, showWarnings = FALSE)
  dateparseorder <- c(
    "mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "dmy HM", "dmy HMS",
    "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS", "ymd HM", "ymd HMS",
    "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS"
  )
  # Read in data, check CGM type.

  for (f in 1:base::length(files)) {
    Id <- base::unlist(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_"))[1]
    table <- read.csv(files[f])


    # other means ive ran it though split files function as there were dates in there that were not right start date. This need probably incuding in this whole function
    # make this edit at some point
    if (sensortype == "dexcom") {
      table <- table[, c("Id", "DisplayTime", "Value", "DisplayTime3", "Value4")]
      base::colnames(table) <- c("id", "timestampfp", "fingerprickglucose", "timestamp", "sensorglucose")
      table$id <- Id
      require(dplyr)
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          sensorglucose == "Low" ~ "2.2",
          sensorglucose == "High" ~ "22.2",
          TRUE ~ table$sensorglucose
        ))
      )
      table$timestampfp <- base::as.POSIXct(lubridate::parse_date_time(
        table$timestampfp,
        dateparseorder
      ), tz = "UTC")


      table$fingerprickglucose <-
        base::suppressWarnings(base::round(base::as.numeric(table$fingerprickglucose), digits = 2))
    } else if (sensortype == "libre") {
      table <- table[, c(
        "Serial Number",
        "Meter Timestamp",
        "Record Type",
        "Historic Glucose(mmol/L)",
        "Scan Glucose(mmol/L)"
      )]

      table$id <- Id
      table$sensorglucose <- ifelse(table$`Record Type` == 1, table$`Scan Glucose(mmol/L)`, table$`Historic Glucose(mmol/L)`)
      base::colnames(table) <- c("id", "timestamp", "recordtype", "sensorglucose", "scanglucose")
      table <- table[, c("id", "timestamp", "sensorglucose")]
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          sensorglucose == "LO" ~ "2.2",
          sensorglucose == "HI" ~ "27.8",
          TRUE ~ table$sensorglucose
        ))
      )

      insert.df1 <- transform(orig.df, timestamp = timestamp + 300)
      out.df1 <- rbind(orig.df, insert.df)

      insert.df2 <- transform(orig.df, timestamp = timestamp + 300)
      out.df2 <- rbind(out.df1, insert.df2)

      # other means ive ran it though split files function as there were dates in there that were not right start date. This need probably incuding in this whole function
    } else if (sensortype == "other") {
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


    # Set interval based on mode of timestamp diff.
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))


    # split time into windows
    table$h <- substring(table$timestamp, 12)
    table$h <- lubridate::parse_date_time(table$h, "HMS")

    a <- table$h >= lubridate::parse_date_time("00:00:00", "HMS") & table$h < lubridate::parse_date_time("06:00:00", "HMS")
    b <- table$h >= lubridate::parse_date_time("06:00:00", "HMS") & table$h < lubridate::parse_date_time("12:00:00", "HMS")
    c <- table$h >= lubridate::parse_date_time("12:00:00", "HMS") & table$h < lubridate::parse_date_time("18:00:00", "HMS")
    d <- table$h >= lubridate::parse_date_time("18:00:00", "HMS") & table$h < lubridate::parse_date_time("24:00:00", "HMS")

    table <- cbind(table, a, b, c, d)

    table$group <- factor(1 * a + 2 * b + 3 * c + 4 * d)
    table$group <- as.character(table$group)
    table <- table %>% dplyr::mutate(group = dplyr::case_when(
      group == "1" ~ "0000-0600",
      group == "2" ~ "0600-1200",
      group == "3" ~ "1200-1800",
      group == "4" ~ "1800-0000",
      TRUE ~ table$group
    ))

    table$h <- NULL
    table$a <- NULL
    table$b <- NULL
    table$c <- NULL
    table$d <- NULL


    printdates <- as.Date(table$timestamp)
    print(base::paste(files[f], "=", unique(printdates), sep = ""))


    ################ Calculate percentage expected wear##############################################

    # percenageexpectedwear we expect them to have 7 days of data, for those that dont then this is possibly an error in data collection...
    # take the first 7 days of data
    # Total time units in secs
    time <- table
    totaltime <- difftime(max(time$timestamp), min(time$timestamp), units = "secs")
    totaltime <- as.numeric(totaltime)
    totaltimedays <- difftime(max(time$timestamp), min(time$timestamp), units = "days")
    percenageexpectedwear <- ((totaltime / 86400) / 7) * 100


    if (select7days == T) {
      if (totaltimedays > 8) {
        table <- time %>% filter(dplyr::between(as.Date(timestamp), as.Date(recordstart), as.Date(recordstart) + lubridate::days(7)))
      } else {
        table <- time
      }
    }
    totaltime7datcut <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")
    totaltime7datcut <- as.numeric(totaltime7datcut)
    totaltime7datcutdays <- difftime(max(table$timestamp), min(table$timestamp), units = "days")
    percenageexpectedwear_7daycut <- ((totaltime7datcut / 86400) / 7) * 100


    # for percentage wear work out sum of table before
    totaltimebefore <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")


    ################################# calibration step ################################

    # Dexcom G4 sensor this is always true
    if (calibrationcheck == TRUE) {
      # split into two dataframes for easier analysis
      df1 <- table[c("id", "fingerprickglucose", "timestampfp")]
      df1 <- df1[!is.na(df1$fingerprickglucose), ]
      df2 <- table[c("id", "sensorglucose", "timestamp", "group")]
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

      ########################################## Changed clalibration value#########################################
      quality$remove <- ifelse(quality$coef == 0 | quality$MD > 20, 1, 0)

      # dates that werent included as not first 7 days data removed from calibration summary table
      if (select7days == T) {
        if (totaltimedays > 8) {
          quality <- quality %>% filter(dplyr::between(Date, Date, Date + lubridate::days(7)))
        } else {
          quality <- quality
        }
      }

      # Inclusion critera:
      # If MAD >20 remove all from this date or they did not have 2 SMBG on this date (ie the coeefcient would be 0)
      # Mean abs difference needs to be <28% (Angus paper)

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

    # percentage cgm wear number of days wanted / number days collected
    totaltimeafter <- difftime(max(table$timestamp), min(table$timestamp), units = "secs")

    totaltimebefore <- as.numeric(totaltimebefore)
    totaltimeafter <- as.numeric(totaltimeafter)

    percent_cgm_wear <- (totaltimeafter / totaltimebefore) * 100
    table$percent_cgm_wear <- percent_cgm_wear
    table$percentage_expected_wear <- percenageexpectedwear
    table$percenageexpectedwear_7daycut <- percenageexpectedwear_7daycut
    table$totaltime7datcutdays <- as.numeric(totaltime7datcutdays)

    # prints out percentage wear in command prompt
    print(base::paste(files[f], "=", percent_cgm_wear, sep = ""))
    table$Date <- as.Date(table$timestamp)
    tableout <- table[, c("id", "timestamp", "sensorglucose", "group", "Date", "percent_cgm_wear", "percentage_expected_wear", "percenageexpectedwear_7daycut")]

    filename <- base::paste0(outputdirectory, "/", basename(files[f]))
    utils::write.csv(tableout, file = filename, row.names = FALSE)
    if (calibrationcheck == T) {
      filenamecalibration <- base::paste("calibration", "/", base::unlist(tools::file_path_sans_ext(basename(files[f]))), "_calibration.csv", sep = "")
      # utils::write.csv(quality,file = filenamecalibration,row.names = FALSE)
      utils::write.csv(quality, filenamecalibration, row.names = FALSE)
    }
  }
  closeAllConnections()
}
