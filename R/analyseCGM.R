#'@title analyseCGM
#'
#'@description This function function written to create consensus glycemic metrics based off definitions outlined International Consensus on Use of Continuous Glucose Monitoring. Ths function takes files of a certain structure as outlined in the README.
#'@returns A .csv file called by default CGMupload.csv containing CGM variables per each individual (file) on each row
#'
#'@param hourspostexercise only use if exercise=T. Accepts a numeric for the specific restricted timeperiod post exercise (usually 6 hours)
#'
#'@param exercise Default is FALSE. In house development of time windowed exercise files.
#' Format of these files is slightly different to the output files from [CGMprocessing::cleanCGM()] function
#'
#'@param combined TRUE/FALSE. Default is FALSE. This aims to handle pre aggregated data where more than one individual/visit etc. is in the file, most likely from external clinical study databases.
#'
#'@param analysesensorlifetime TRUE/FALSE. Default is TRUE. Used only with combined= TRUE. If you are expecting to analyse continuous data that is > sensor lifetime then specify to FALSE. This is to ensure multiple sensors from one person are not aggregated by accident.
#'
#'@param libre For calculation of the correct interval (libre 15 min or 900s, CGM 5 min or 300s).
#' Default is FALSE. Currently libre files have "dummy" coded 5 minute data with carry forward method.
#' ie. Every row there is an addition of 2 rows that are same as the original row.
#'
#'@param inputdirectory path to folder containing files created by [CGMprocessing::cleanCGM()] function
#'
#'@param outputdirectory path to folder where output csv file will be uploaded
#'
#'@param outputname name of output file. Default "CGMupload".
#'
#'@param aboveexcursionlength numeric for the time (in minutes) defined as an hyperglycemic exercusion
#'  default is 15 minutes (https://care.diabetesjournals.org/content/40/12/1631)
#'
#'@param belowexcursionlength numeric for the time (in minutes) defined as an hypoglycemic exercusion
#'  default is 15 minutes (https://care.diabetesjournals.org/content/40/12/1631)
#'
#'@param magedef Defining the threshold used in MAGE calculation.  MAGE is an arithmetic average of either the upward or downward
#' of all glycemic excursions exceeding the threshold (standard deviation of blood glucose obtained from all blood glucose
#' concentrations within 24-hour period). Default is 1 standarddevation ("1sd"), options are 1.5 SD ("1.5sd") , 2 SD ("2sd")
#' or other can be specifed as a numeric
#'
#'@param congan Specificing the n number of hours in CONGA(n). Default is the numeric 1. CONGA(n) represents the SD
#' of all valid differences between a current observation and an observation (n) hours earlier
#'
#'@param format changes format to CGM variables as x or y in table. Default is "rows" making each ID a row
#'
#'@param printname Prints name of the file being processed. Default is TRUE.
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate across contains filter select group_by inner_join slice ungroup arrange if_else lag lead
#' @importFrom tools file_path_sans_ext
#' @importFrom janitor clean_names
#' @importFrom anytime anytime
#' @importFrom here here
#' @import tidyr
#' @import lubridate
#' @importFrom berryFunctions insertRows
#' @import pracma
#' @import stats
#' @import pastecs
#' @import zoo
#' @import purrr
#' @import utils
#'
#' @author Alice Carr
#'
#' @export
#'
#' @seealso
#' cleanCGM and exercise_split
#'
#'


analyseCGM <- function(exercise = F,
                       hourspostexercise=NULL,
                       combined=F,
                       analysesensorlifetime=T,
                       libre=T,
                       inputdirectory,
                       outputdirectory,
                       outputname="CGMupload",
                       belowexcursionlength = 15,
                       aboveexcursionlength = 15,
                       magedef = "1sd",
                       congan = 1,
                       format = "rows",
                       printname = T) {

  base::dir.create(outputdirectory, showWarnings = FALSE)

  if(combined==F){
  # define lists
  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  if(exercise==T){
  cgmupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(files)))
  base::colnames(cgmupload) <- base::rep("Record", base::length(files))
  }else if(exercise==F){
    #adding this in as i now have metrics for different parts of the day : all, day time and nighttime
    #wouldnt use this with exercicse =T
    cgmupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(files)*3))
    base::colnames(cgmupload) <- base::rep("Record", base::length(files)*3)
  }
  }else if(combined==T){
    table_test<-rio::import(inputdirectory)
    files<-split(table_test,table_test$id)
    if(exercise==T){
    cgmupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(unique(table_test$id))))
    base::colnames(cgmupload) <- base::rep("Record", base::length(unique(table_test$id)))
    }else if(exercise==F){
      #adding this in as i now have metrics for different parts of the day : all, day time and nighttime
      #wouldnt use this with exercicse =T
      cgmupload <- base::as.data.frame(base::matrix(nrow = 0, ncol = base::length(unique(table_test$id))*3))
      base::colnames(cgmupload) <- base::rep("Record", base::length(unique(table_test$id))*3)

    }
  }



  for (file in 1:base::length(files)) {
    if(combined==F & analysesensorlifetime==T){
    table <-  base::suppressWarnings(rio::import(files[file], guess_max = 10000000))
    Id <- unique(table$id)
    #Id <- base::unlist(tools::file_path_sans_ext(basename(files[file])), "_")

    names(table) <- tolower(names(table))
    table <- unique(table)
    if (printname == T) {
      print(basename(files[file]))
    }
    }else if(combined==F & analysesensorlifetime==F){
      table <-  base::suppressWarnings(rio::import(files[file], guess_max = 10000000))
      #Id <- unique(table$id)
      table$id<-sub("_[^_]*$", "", table$id) # if expecting > sensor lifetime then get rid of device id in the underscore
      Id <-unique(table$id)
      names(table) <- tolower(names(table))
     # cgmupload["subject_id", f] <- Id
      table <- unique(table)
      if (printname == T) {
        print(basename(files[file]))
      }
    }else if(combined==T & analysesensorlifetime==T){
      table <-  files[[file]]
      Id <- unique(table$id)
      names(table) <- tolower(names(table))
      #cgmupload["subject_id", f] <- Id
      table <- unique(table)
      if (printname == T) {
        print(Id)
      }
    }else if(combined==T & analysesensorlifetime==F){
      table <-  files[[file]]
      table$id<- sub("_[^_]*$", "", table$id) # if expecting > sensor lifetime then get rid of device id in the underscore
      Id <- unique(table$id)
      names(table) <- tolower(names(table))
      #cgmupload["subject_id", f] <- Id
      table <- unique(table)
      print(Id)
    }


# more recent times there is an issue with date time change to remove parse date time
    table$timestamp <- base::as.POSIXct(table$timestamp)

    table<- table %>% dplyr::arrange(timestamp) #important to order on timestamp for many functions below to work


    if (is.null(length(table$sensorglucose)) | length(table$sensorglucose) == 1 | length(table$sensorglucose) == 0) {
      print(base::paste(files[file], "not enought data"))
      next
    }

    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))

    #define interval of readings
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)))
    interval <- base::abs(interval)
    if(libre==T & interval==900){
      interval<- interval/3
      # all sensor readings ensure there are "5min readings" for the analyseCGM function.

      #Pseudo code it this way mainly for the hypo definition which relies on 15mins
        table<-slice(table,rep(1:n(), each = 3))

    }else if(libre==T & interval!=900){
      table<-table
    }

    # if there is no readings or 1 reading not enough data
    if (is.null(length(table$sensorglucose)) | length(table$sensorglucose) == 1 | length(table$sensorglucose) == 0) {
      print(base::paste(files[file], "not enought data"))
      next
    }


    # metrics for 24hours overnight and day

  if(exercise==T){
    table_list<-list(table)
  }else if(exercise==F){
    #all time
    table_24<-table

    #day time 6am to midnight
    table_day<-dplyr::filter(table,lubridate::hour(table$timestamp) >= 6 & hour(table$timestamp) < 24) %>%
      dplyr::arrange(timestamp)

    #sleep midnight to 6am
    table_night<-dplyr::filter(table,lubridate::hour(table$timestamp) <6) %>%
      dplyr::arrange(timestamp)

    table_list<-list(table_24,table_day,table_night)
    time_of_day_list<-c("all time","day time","night time")
    # Identify positions of empty data frames
    empty_positions <- sapply(table_list, function(df) nrow(df) == 0)

    # Remove elements from both lists
    table_list <- table_list[!empty_positions]
    time_of_day_list <- time_of_day_list[!empty_positions]
  }


    for(f in seq_along(table_list)){
      table<- table_list[[f]]
      if (exercise == F) {
      table$timeofday<-time_of_day_list[f]
      }

      # specific TIR for exercise from lancet guidelines, input file must be set up specifically
      if (exercise == T) {
        f<-file
        cgmupload["subject_id", f] <- Id
        # files not set up correctly
        if (is.null(hourspostexercise) | !("diff_disc" %in% names(table))) {
          stop("Input files not set up correctly for this step- refer to README. Ensure you also use hourspostexercise argument to specify time of interest post-exercise.")
        }
        hours_post_exercise=hourspostexercise
        table$date<-as.Date(table$timestamp)
        cgmupload["exercise", f]<-"TRUE"
        cgmupload["hourspostexercise", f]<-hours_post_exercise
        #time in range for exercise 7-15 mmol/L from Lancet guidelines
        exercisetime<- nrow(table[table$diff_disc==0,]) * interval
        if(exercisetime>0){
          BGinrangeexercise <- base::as.numeric(table$sensorglucose[base::which(table$diff_disc==0)], length = 1)
          BGinrangeexercise <- ifelse(BGinrangeexercise %in% seq(7, 15, 0.01), 1, 0)
          cgmupload["min_spent_7_15_exercise", f] <-base::round(base::sum(BGinrangeexercise) * (interval / 60), digits = 2)
          cgmupload["percent_time_7_15_exercise", f] <- base::round(((base::sum(BGinrangeexercise) * (interval / 60)) * 60 / exercisetime) * 100, digits = 2)
          #recommended post exercise 5-12
          postexercisetime<- nrow(table[table$diff_disc>0 & table$diff_disc<=hours_post_exercise,]) * 300
          BGinrangepostexercise <- base::as.numeric(table$sensorglucose[base::which(table$diff_disc!=0)], length = 1)
          BGinrangepostexercise <- ifelse(BGinrangepostexercise %in% seq(7, 15, 0.01), 1, 0)
          cgmupload["min_spent_5_12_exercise", f] <-base::round(base::sum(BGinrangepostexercise) * (interval / 60), digits = 2)
          cgmupload["percent_time_5_12_exercise", file] <- base::round(((base::sum(BGinrangepostexercise) * (interval / 60)) * 60 / postexercisetime) * 100, digits = 2)
        }

        # ensure now the table is only timevalues after exercise ie. diff disc !=0
        table<-filter(table,diff_disc!=0)
      }



      if(exercise==F){
      f<-(seq(0,ncol(cgmupload),3)[file])+f
      cgmupload["time_of_day",f]<- unique(table$timeofday)
      cgmupload["subject_id", f] <- Id
      }

    # Total time in the dataset is the whole length of the dataset x 300 as each row represetns 300 seconds (5mins)
    #table will have been interpoated in preprocessing, the number of minutes/cgm points interpolated can be derived from that output file
    totaltime <- nrow(table) * interval


    # Beginning of generation of table
    cgmupload["totaltime_mins", f] <- as.numeric(totaltime) / 60
    cgmupload["start_cgm_analysis", f] <- base::as.character(min(table$timestamp, na.rm = T))
    cgmupload["end_cgm_analysis", f] <- base::as.character(max(table$timestamp, na.rm = T))

    if(libre==T){
    cgmupload["interval", f] <- interval*3
    table$id<-Id
    }else if(libre==F){
    cgmupload["interval", f] <- interval
    table$id<-Id
    }

    # this should actually be total time /24*3600 for more accurate day count
    cgmupload["num_days_cgmwear", f] <- as.numeric(round(difftime(max(table$timestamp),min(table$timestamp),units ="days")))

    # this is more the true amount of time as it goes off specific hours
    cgmupload["totaltime_hours", f] <- base::round(unlist(totaltime) / 3600)

    # number of readings- wouldnt include interpolated, if we were to include interpolation it should be in this function after this call
    cgmupload["total_sensor_readings", f] <- base::as.numeric(base::nrow(unique(table[!is.na(table$sensorglucose),])))


    # Average sensor glucose
    cgmupload["average_sensor", f] <- base::round(base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))], na.rm = T), digits = 2)

    # Hba1C equations uses glucose in mg/dl so convert it
    glucosemgdl <- (table$sensorglucose[base::which(!is.na(table$sensorglucose))]) * 18
    cgmupload["estimated_a1c%", f] <- base::round((46.7 + (base::mean(glucosemgdl[base::which(!is.na(glucosemgdl))]))) / 28.7, digits = 1)
    cgmupload["estimated_a1cmmolmol", f] <- 10.929 * (base::round((46.7 + (base::mean(glucosemgdl[base::which(!is.na(glucosemgdl))]))) / 28.7, digits = 1)- 2.15)

    # the gmi is the Glucose Management Indicator inndicates the average A1C level that would be expected based on mean glucose measured
    # gmi and estimated Hba1C should therefore be similar...
    cgmupload["gmimmol/mol", f] <- base::round(12.71 + (4.70587 * base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))])), digits = 1)
    cgmupload["gmi%", f] <- base::round(3.31 + (0.02392 * base::mean((table$sensorglucose[base::which(!is.na(table$sensorglucose))]) * 18)), digits = 1)

    # lower quartile sensor glucose
    cgmupload["q1_sensor", f] <- base::round(base::as.numeric(base::summary(table$sensorglucose[base::which(!is.na(table$sensorglucose))])[2]), digits = 2)

    # median sensor glucose
    cgmupload["median_sensor", f] <- base::round(base::as.numeric(base::summary(table$sensorglucose[base::which(!is.na(table$sensorglucose))])[3]), digits = 2)

    # upper quartile sensor
    cgmupload["q3_sensor", f] <- base::round(base::as.numeric(base::summary(table$sensorglucose[base::which(!is.na(table$sensorglucose))])[5]), digits = 2)

    # standard deviation
    cgmupload["standard_deviation", f] <- base::round(stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    # CV is the coeffcient of variarion
    # Note: SD is highly influenced by the mean glucose – someone with a higher mean glucose will have a higher SD.
    # the CV divides the SD/mean x100. This division helps “correct” and normalize glucose variability, allowing us to set a single variability goal that applies to people with different mean glucose levels.
    cgmupload["cv", f] <- base::round((stats::sd(table$sensorglucose[base::which(!is.na(table$sensorglucose))])) / base::mean(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    # minimum sensor reading
    cgmupload["min_sensor", f] <- base::round(base::min(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    # maximum reading
    cgmupload["max_sensor", f] <- base::round(base::max(table$sensorglucose[base::which(!is.na(table$sensorglucose))]), digits = 2)

    ########### Over 10mmol calcs##########
    # create a new df so not to mess up table for the other metrics
    table10 <- table


    # put breaks (rows of NA) where there are non-consectuive timestamps (ie. after calibration when some dates have been removed)
    # this prevents excursions that were on one day running to another non-consective day if that day started >=10
    # this also prevents interpolation over 20 min- would treat episodes >20 min as separate even if the rows are next to each other
    # table10$consecutive <- c(FALSE,diff(as.Date(table10$date))>1)

    table10$consecutive <- c(FALSE, as.numeric(diff(table10$timestamp),units="secs") > 1200)

    insertpostions10 <- which(table10$consecutive %in% TRUE)
    if (length(insertpostions10) != 0) {
      insertpostions10[1]<-insertpostions10-1
      #table10 <- table10 %>% tibble::add_row(, .before = insertpostions10[1])
    } else {
      table10 <- table10
    }
    # missing variables means that they all will be given value of NA which is what we want
    if (length(insertpostions10) >= 1) {
      table10 <- berryFunctions::insertRows(table10, r = c(insertpostions10), new = NA, rcurrent = T)
    }

    # make a new column for dealing with sensor glucose in this new df. Probs uneccesary but was helpful to debug
    table10$BGover10 <- table10$sensorglucose
    # replace where NA is with 0 anything <10 with a 0 and anything else (>=10) with a 1
    table10 <- table10 %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(BGover10 = dplyr::case_when(
        is.na(sensorglucose) ~ 0,
        sensorglucose <= 10 ~ 0,
        TRUE ~ 1
      ))


    # perform run length encoding to find "true" excursions that are >excursion length defined in function (15mins usually)
    #padd with 0s to ensure this works if ending in 1
    BGover10 <- as.numeric(table10$BGover10,length=1)
    BG10.rle <- base::rle(BGover10)
    over10loc <- base::matrix(nrow = 0, ncol = 3)
    over10loc$values <- BG10.rle$values
    over10loc$position <- base::cumsum(BG10.rle$lengths) + 1
    over10loc$lengths <- BG10.rle$lengths
    over10loc$lengthstrue <- BG10.rle$lengths - 1
    over10loc <- base::as.data.frame(over10loc)
    over10loc$true <- ifelse(over10loc$lengths >= (((aboveexcursionlength * 60) / interval) + 1) & over10loc$values == 1, T, F)

    # number of true excursions >10 >15minsif gaps are >20 mins between consecutive excursiosn
    cgmupload["excursions_over_10", f] <- base::length(base::which(over10loc$true == T))

    # from the run length encoding find the positions where the true excursion started
    # and replace these values with a 1 in the new column which was replaced with the original sensor glucose values
    positions10 <- dplyr::lag(over10loc$position)[which(over10loc$true == T)]
    if (over10loc$values[1] == 1 & over10loc$true[1] == T) {
      positions10[1] <- 1
    }
    table10$BGover10 <- table10$sensorglucose
    table10$BGover10[c(positions10)] <- 1

    # function fills in after these 1s with a 1 if it is >=10 to give us chuncks of 1s where the excursions happen (all are >35mins becuase of previous steps)
    fill_in_10 <- function(prev, new) {
      if_else(new != 1 & prev == 1 & new > 10, 1, new)
    }

    table10 <- table10 %>%
      mutate(BGover10 = purrr::accumulate(BGover10, fill_in_10))

    # replace anything NA with a 0 is like this again as we did table10$BGover10<-table10$sensorglucose
    # (ie. where the gaps where because we only want the time now spent and we already have highlighted the sections with 1s)
    # replace anything with a 1 with a 1 and anything else (which will be <10) with a 0
    table10$BGover10 <- dplyr::case_when(
      is.na(table10$BGover10) ~ 0,
      table10$BGover10 == 1 ~ 1,
      TRUE ~ 0
    )


    # count the time spend over 10
    # make a new array of the 0 and 1 BGover10 and then pad either end with 0s so this will always work if we end or start in a 1
    mins10 <- diff(c(0, as.numeric(table10$BGover10), 0))
    # timestamp also as a separate array
    time10 <- as.array(table10$timestamp)
    # pick up the time where 1 sequence starts, and 0 starts as the end. Here the mins10 array is
    # one element longer than time but since the last element for mins10 == 1 will always be false because padded by 0, it won't affect the result.
    results10 <- data.frame(start = time10[mins10 == 1], end = time10[(mins10 == -1)[-1]]) %>%
      dplyr::summarise(sum(difftime(end, start, units = "secs")))

    # final variables for mins > 10 and percentage time >10 from above code
    cgmupload["min_spent_excursion_over_10", f] <- base::round(base::sum(as.numeric(results10)) / 60)
    cgmupload["percent_time_excursion_over_10", f] <- base::round((base::sum(as.numeric(results10)) / totaltime) * 100, digits = 2)


    ############### Over 13.9mmol calcs#############################################
    # create a new df so not to mess up table for the other metrics
    table13 <- table
    # put breaks (rows of NA) where there are non-consectuive timestamps (ie. after calibration when some dates have been removed)
    # this prevents excursions that were on one day running to another non-consective day if that day started >=13
    # table13$consecutive <- c(FALSE,diff(as.Date(table13$date))>1)

    table13$consecutive <- c(FALSE, as.numeric(diff(table13$timestamp),units="secs") > 1200)
    insertpostions13 <- which(table13$consecutive %in% TRUE)

    if (length(insertpostions13) != 0) {
      insertpostions13[1]<-insertpostions13-1
     # table13 <- table13 %>% tibble::add_row(, .before = insertpostions13[1])
    } else {
      table13 <- table13
    }
    # missing variables means that they all will be given value of NA which is what we want
    if (length(insertpostions13) >= 1) {
      table13 <- berryFunctions::insertRows(table13, r = c(insertpostions13), new = NA, rcurrent = T)
    }

    # make a new column for dealing with sensor glucose in this new df. Probs uneccesary but was helpful to debug
    table13$BGover13 <- table13$sensorglucose
    # replace where NA is with 0 anything <13 with a 0 and anything else (>=13) with a 1
    table13 <- table13 %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(BGover13 = dplyr::case_when(
        is.na(sensorglucose) ~ 0,
        sensorglucose <= 13.9 ~ 0,
        TRUE ~ 1
      ))

    # perform run length encoding to find "true" excursions that are >15mins
    BGover13 <- base::as.numeric(table13$BGover13, length = 1)
    BG13.rle <- base::rle(BGover13)
    over13loc <- base::matrix(nrow = 0, ncol = 3)
    over13loc$values <- BG13.rle$values
    over13loc$position <- cumsum(BG13.rle$lengths) + 1
    over13loc$lengths <- BG13.rle$lengths
    over13loc$lengthstrue <- BG13.rle$lengths - 1
    over13loc <- as.data.frame(over13loc)
    over13loc$true <- ifelse(over13loc$lengths >= (((aboveexcursionlength * 60) / interval) + 1) & over13loc$values == 1, T, F)

    # number of true excursions >13.9 >15mins
    cgmupload["excursions_over_13", f] <- base::length(base::which(over13loc$true == T))

    # from the run length encoding find the positions where the true excursion started
    # and replace these values with a 1 in the new column which was replaced with the original sensor glucose values
    positions13 <- dplyr::lag(over13loc$position)[which(over13loc$true == T)]
    if (over13loc$values[1] == 1 & over13loc$true[1] == T) {
      positions13[1] <- 1
    }
    table13$BGover13 <- table13$sensorglucose
    table13$BGover13[c(positions13)] <- 1

    # function fills in after these 1s with a 1 if it is >13.9 to give us chuncks of 1s where the excursions happen (all are >35mins becuase of previous steps)
    fill_in_13 <- function(prev, new) {
      if_else(new != 1 & prev == 1 & new > 13.9, 1, new)
    }

    table13 <- table13 %>%
      mutate(BGover13 = purrr::accumulate(BGover13, fill_in_13))

    # replace anything NA with a 0 is like this again as we did table13$BGover13<-table13$sensorglucose
    # (ie. where the gaps where because we only want the time now spent and we already have highlighted the sections with 1s)
    # replace anything with a 1 with a 1 and anything else (which will be <13) with a 0
    table13$BGover13 <- dplyr::case_when(
      is.na(table13$BGover13) ~ 0,
      table13$BGover13 == 1 ~ 1,
      TRUE ~ 0
    )

    # count the time spend under 13.9
    # make a new array of the 0 and 1 BGover13 and then pad either end with 0s so this will always work if we end or start in a 1
    mins13 <- diff(c(0, as.numeric(table13$BGover13), 0))
    # timestamp also as a separate array
    time13 <- as.array(table13$timestamp)
    # pick up the time where 1 sequence starts, and 0 starts as the end. Here the mins13 array is
    # one element longer than time but since the last element for mins13 == 1 will always be false because padded by 0, it won't affect the result.
    results13 <- data.frame(start = time13[mins13 == 1], end = time13[(mins13 == -1)[-1]]) %>%
      dplyr::summarise(sum(difftime(end, start, units = "secs")))

    cgmupload["min_spent_excursion_over_13", f] <- base::round(base::sum(as.numeric(results13) / 60))
    cgmupload["percent_time_excursion_over_13", f] <- base::round((base::sum(as.numeric(results13)) / totaltime) * 100, digits = 2)


    ############### Over 16mmol calcs - not required for international consesus CGM results#############################################
    # create a new df so not to mess up table for the other metrics
    table16 <- table
    # put breaks (rows of NA) where there are non-consectuive timestamps (ie. after calibration when some dates have been removed)
    # this prevents excursions that were on one day running to another non-consective day if that day started >=13
    # table16$consecutive <- c(FALSE,diff(as.Date(table16$date))>1)

    table16$consecutive <- c(FALSE, as.numeric(diff(table16$timestamp),units="secs") > 1200)

    insertpostions16 <- which(table16$consecutive %in% TRUE)

    if (length(insertpostions16) != 0) {
      insertpostions16[1]<-insertpostions16-1
      #table16 <- table16 %>% tibble::add_row(, .before = insertpostions16[1])
    } else {
      table16 <- table16
    }
    # missing variables means that they all will be given value of NA which is what we want
    if (length(insertpostions16) >= 1) {
      table16 <- berryFunctions::insertRows(table16, r = c(insertpostions16), new = NA, rcurrent = T)
    }

    # make a new column for dealing with sensor glucose in this new df. Probs uneccesary but was helpful to debug
    table16$BGover16 <- table16$sensorglucose
    # replace where NA is with 0 anything <16 with a 0 and anything else (>=16) with a 1
    table16 <- table16 %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(BGover16 = dplyr::case_when(
        is.na(sensorglucose) ~ 0,
        sensorglucose <= 16 ~ 0,
        TRUE ~ 1
      ))

    # perform run length encoding to find "true" excursions that are >35mins
    BGover16 <- base::as.numeric(table16$BGover16, length = 1)
    BG16.rle <- base::rle(BGover16)
    over16loc <- base::matrix(nrow = 0, ncol = 3)
    over16loc$values <- BG16.rle$values
    over16loc$position <- cumsum(BG16.rle$lengths) + 1
    over16loc$lengths <- BG16.rle$lengths
    over16loc$lengthstrue <- BG16.rle$lengths - 1
    over16loc <- as.data.frame(over16loc)
    over16loc$true <- ifelse(over16loc$lengths >= (((aboveexcursionlength * 60) / interval) + 1) & over16loc$values == 1, T, F)
    # number of true excursions
    cgmupload["excursions_over_16", f] <- base::length(base::which(over16loc$true == T))

    # from the run length encoding find the positions where the true excursion started
    # and replace these values with a 1 in the new column which was replaced with the original sensor glucose values
    positions16 <- dplyr::lag(over16loc$position)[which(over16loc$true == T)]
    if (over16loc$values[1] == 1 & over16loc$true[1] == T) {
      positions16[1] <- 1
    }
    table16$BGover16 <- table16$sensorglucose
    table16$BGover16[c(positions16)] <- 1

    # function fills in after these 1s with a 1 if it is >16 to give us chuncks of 1s where the excursions happen
    fill_in_16 <- function(prev, new) {
      if_else(new != 1 & prev == 1 & new > 16, 1, new)
    }

    table16 <- table16 %>%
      mutate(BGover16 = purrr::accumulate(BGover16, fill_in_16))

    # replace anything NA with a 0 is like this again as we did table16$BGover16<-table16$sensorglucose
    # (ie. where the gaps where because we only want the time now spent and we already have highlighted the sections with 1s)
    # replace anything with a 1 with a 1 and anything else (which will be <16) with a 0
    table16$BGover16 <- dplyr::case_when(
      is.na(table16$BGover16) ~ 0,
      table16$BGover16 == 1 ~ 1,
      TRUE ~ 0
    )

    # count the time spend over 16
    # make a new array of the 0 and 1 BGover16 and then pad either end with 0s so this will always work if we end or start in a 1
    mins16 <- diff(c(0, as.numeric(table16$BGover16), 0))
    # timestamp also as a separate array
    time16 <- as.array(table16$timestamp)
    # pick up the time where 1 sequence starts, and 0 starts as the end. Here the mins13 array is
    # one element longer than time but since the last element for mins13 == 1 will always be false because padded by 0, it won't affect the result.
    results16 <- data.frame(start = time16[mins16 == 1], end = time16[(mins16 == -1)[-1]]) %>%
      dplyr::summarise(sum(difftime(end, start, units = "secs")))

    cgmupload["min_spent_excursion_over_16", f] <- base::round(base::sum(as.numeric(results16) / 60))
    cgmupload["percent_time_excursion_over_16", f] <- base::round((base::sum(as.numeric(results16)) / totaltime) * 100, digits = 2)


    ######### Hypoglycemia############
    # International consensus states:
    # Beginning of a CGM event -readings below the threshold for at least 15 min is considered an event. For example, at least 15 min <54 mg/dL (3.0 mmol/L) to define a clinically significant (level 2) hypoglycemic event.
    # End of a CGM event-readings for 15 min at ≥70 mg/dL (3.9 mmol/L).

    # create a new df so not to mess up table for the other metrics
    hypo <- table
    # put breaks (rows of NA) where there are non-consectuive timestamps (ie. after calibration when some dates have been removed)
    # this prevents excursions that were on one day running to another non-consective day if that day started >=13
    hypo$consecutive <- c(FALSE, as.numeric(diff(hypo$timestamp),units="secs") > 1200)
    insertpostionshypo <- which(hypo$consecutive %in% TRUE)

    # insertpostionshypo<-match(TRUE,hypo$consecutive)
    if (length(insertpostionshypo) != 0){
      insertpostionshypo[1]<-insertpostionshypo-1
     # hypo <- hypo %>% tibble::add_row(, .before = insertpostionshypo[1])
    } else {
      hypo <- hypo
    }
    # missing variables means that they all will be given value of NA which is what we want
    if (length(insertpostionshypo) >= 1) {
      hypo <- berryFunctions::insertRows(hypo, r = c(insertpostionshypo), new = NA, rcurrent = T)
    }

    # make a new column for dealing with sensor glucose in this new df. Probs uneccesary but was helpful to debug
    hypo$BGunder3 <- hypo$sensorglucose
    # replace where NA is with 0 anything <13 with a 0 and anything else (>=13) with a 1
    hypo <- hypo %>%
      dplyr::group_by(date) %>%
      dplyr::mutate(BGunder3 = dplyr::case_when(
        is.na(sensorglucose) ~ 0,
        sensorglucose >= 3 ~ 0,
        TRUE ~ 1
      ))

    # perform run length encoding to find "true" excursions that are >15mins
    BGunder3 <- base::as.numeric(hypo$BGunder3, length = 1)
    BGunder3 <- base::rle(BGunder3)

    # prolonged is >2 hours in hypo
    # International Guidelines state:
    # A second hypoglycemic event outcome of prolonged hypoglycemia is considered when CGM levels are <54 mg/dL (3.0 mmol/L) for consecutive 120 min or more
    cgmupload["hypo_under_3_prolonged", f] <- base::length(base::which(base::as.numeric(BGunder3$lengths[base::which(BGunder3$values == 1)]) >= ((120 * 60) / interval)))

    # for the other hypos decide which ones are >15mins
    under3loc <- base::matrix(nrow = 0, ncol = 3)
    under3loc$values <- BGunder3$values
    under3loc$position <- cumsum(BGunder3$lengths) + 1
    under3loc$lengths <- BGunder3$lengths
    under3loc$lengthstrue <- BGunder3$lengths - 1
    under3loc <- as.data.frame(under3loc)
    under3loc$true <- ifelse(under3loc$lengths >= (((belowexcursionlength * 60) / interval) + 1) & under3loc$values == 1, T, F)


    # from the run length encoding find the positions where the true excursion started
    # and replace these values with a 1 in the new column which was replaced with the original sensor glucose values
    positionshypo <- dplyr::lag(under3loc$position)[which(under3loc$true == T)]
    if (under3loc$values[1] == 1 & under3loc$true[1] == T) {
      positionshypo[1] <- 1
    }
    hypo$BGunder3 <- hypo$sensorglucose
    hypo$BGunder3[c(positionshypo)] <- 1

    # From where the true hypos are located by 1s fill in the next value if it is <3.9
    # Still in hypo until >3.9 for 15mins
    fill_in <- function(prev, new) {
      if_else(new != 1 & prev == 1 & new < 3.9, 1, new)
    }
    hypo <- hypo %>%
      mutate(BGunder3 = purrr::accumulate(BGunder3, fill_in))

    # Turn everything <3.9 into a 2 to work out if the >3.9 after the end of hypo has lasted for ≥15mins
    # if it hasnt then it is still classed as that hypo
    # DEBUG
    # hypo<-as.data.frame(c(2,2,1,1, 1, 1, 5, 5, 5, 2, 2, 5, 2, 5, 5, 2, 5, 5,5,5,5))
    # colnames(hypo)<-c("BGunder3")

    # this part deals with the scenario 1 0 2 to fill the 0 in and turns everything into 1s, 0s, 2s
    hypo <- hypo %>% dplyr::mutate(BGunder3 = dplyr::case_when(
      BGunder3 != 1 & BGunder3 < 3.9 ~ 2,
      dplyr::lag(BGunder3) == 1 & dplyr::lead(BGunder3) == 2 ~ 2,
      BGunder3 == 1 ~ 1,
      TRUE ~ 0
    ))

    hypo$BGunder3 <- ifelse(is.na(hypo$id), 2.1, hypo$BGunder3) #where there is gaps

    # if there are 2s that follow straight after a 1 then turn all these 2s into a 1 (2s after a 1 represent <3.9 a hypo does not end until we are clear of 3.9 for 15mins)
    fill_in_2 <- function(prev, new) {
      if_else(new == 2 & prev == 1, 1, new)
    }
    hypo <- hypo %>%
      mutate(BGunder3 = purrr::accumulate(BGunder3, fill_in_2))

    # Now dealing with other cases ie. 1 0 0 2 and 1 0 0 0 2, A hypo ends when we have > 4 zeros (since 1 zero represents 5 mins assuming there are no gaps)
    # if there are gaps then thats ok because we assume the glucose was the same in these gaps, so a hypo still classed as not ended until 4 0s in a row
    # we can get the true time in hypo still by subtracting the timestamps

    # check for debug
    # hypo<-c(2,2,1,1, 1, 1, 0, 0, 0, 2, 2, 0, 2, 0, 0, 2, 0, 0,0,0,0)

    # make an array and pad with 0s so this code will always work
    BGunder3 <- c(0, hypo$BGunder3, 0, 0, 0, 0)
    # true sequence starts at postion 2 since we padded with 0s and ends at length-4
    for (i in 2:length(BGunder3)) {
      if (BGunder3[i] == 0 && BGunder3[i - 1] == 1) {
        if (BGunder3[i + 1] != 0 | BGunder3[i + 2] != 0 | BGunder3[i + 3] != 0) {
          BGunder3[i + 1] <- 1
          BGunder3[i + 2] <- 1
          BGunder3[i + 3] <- 1
          BGunder3[i] <- 1
        } else if (BGunder3[i] != 0 && BGunder3[i - 1] == 1) {
          BGunder3[i] <- 1
        } else if (BGunder3[i] != 0 && BGunder3[i - 1] == 2) {
          BGunder3[i] <- BGunder3[i]
        }
      }
    }

    hypo$BGunder3 <- BGunder3[2:(length(BGunder3) - 4)]
    # make any 2s proceding a 1 into 1s
    fill_in_3 <- function(prev, new) {
      if_else(new == 2 & prev == 1, 1, new)
    }
    hypo <- hypo %>%
      mutate(BGunder3 = purrr::accumulate(BGunder3, fill_in_3))



    # all 2s at the end of a 1 sequence chnaged to 1s
    hypo <- hypo %>%
      dplyr::mutate(BGunder3 = dplyr::case_when(
        BGunder3 == 0 & dplyr::lag(BGunder3) == 1 & dplyr::lead(BGunder3) == 2 ~ 1,
        TRUE ~ BGunder3
      )) %>%
      mutate(BGunder3 = dplyr::case_when(
        BGunder3 == 2 & dplyr::lag(BGunder3) == 1 & dplyr::lead(BGunder3) == 0 ~ 1,
        TRUE ~ BGunder3
      ))

    # run for case of 1 0 0 2
    # all ends of hypo are now covered
    # true sequence starts at postion 2 since we padded with 0s and ends at length-3 so this code will always work even if there is a hypo at end of file
    BGunder3 <- c(0, hypo$BGunder3, 0, 0, 0)
    for (i in 2:length(BGunder3)) {
      if (BGunder3[i] == 0 && BGunder3[i - 1] == 1) {
        if (BGunder3[i + 1] != 0 | BGunder3[i + 2] != 0) {
          BGunder3[i + 1] <- 1
          BGunder3[i + 2] <- 1
          BGunder3[i] <- 1
        } else if (BGunder3[i] != 0) {
          BGunder3[i] <- BGunder3[i]
        }
      }
    }
    hypo$BGunder3 <- BGunder3[2:(length(BGunder3) - 3)]
    # change everything in 1s and 0s, the actual end at the end of the 15 mins that we were over 3.9 ie. 4 after the final 1
    hypo <- hypo %>% dplyr::mutate(BGunder3 = dplyr::case_when(
      BGunder3 == 1 ~ 1,
      TRUE ~ 0
    ))
    hypo$BGunder3 <- ifelse(is.na(hypo$id), 2.1, hypo$BGunder3)


    # final end -
    #this was added in as it was trying to stric the definition to the END of the 15min period had been >3.9.
    # this just adds an extra 20mins to the hypo time which i dont think should happen
    # we can define true hypos as >3 for 15 mins with the event not ending until >3.9 15in but it should end at the first 3.9 it went to when the event ended
    #for now comment this out
    # padd with 0s again so this works
    # BGunder3 <- c(0, hypo$BGunder3, 0, 0, 0, 0)
    # for (i in 2:length(BGunder3)) {
    #   if (BGunder3[i] == 0 && BGunder3[i - 1] == 1) {
    #     BGunder3[i] <- 2
    #     BGunder3[i + 1] <- 2
    #     BGunder3[i + 2] <- 2
    #     BGunder3[i + 3] <- 2
    #   }
    # }
    # # replace in the hypo df and turn those 2s into 1s
    # hypo$BGunder3 <- BGunder3[2:(length(BGunder3) - 4)]
    # hypo <- hypo %>% dplyr::mutate(BGunder3 = dplyr::case_when(
    #   BGunder3 == 2 ~ 1,
    #   TRUE ~ BGunder3
    # ))

    #if there is a gap in a hypo then the gap would be >20 mins long we have not done interpolation here therefore we cant assume hypo is the same one so we make the gap =0

    hypo$BGunder3 <- ifelse(is.na(hypo$id),0,hypo$BGunder3)


    BG3.rle <- base::rle(base::as.numeric(hypo$BGunder3, length = 1))
    # the hypos picked up here will always be > excursion length as for loop from above found the "true" hypos but can keep this code in here just incase
    cgmupload["hypo_under_3", f] <- base::length(base::as.numeric(BG3.rle$lengths[base::which(BG3.rle$values == 1)]))


    # count the time spent in hypo
    # make a new array of the 0 and 1 BGunder3 and then pad either end with 0s so this will always work if we end or start in a 1
    minshypo <- diff(c(0, as.numeric(hypo$BGunder3), 0))
    # timestamp also as a separate array
    timehypo <- as.array(hypo$timestamp)
    # pick up the time where 1 sequence starts, and 0 starts as the end. Here the hypo array is
    # one element longer than time but since the last element for mins13 == 1 will always be false because padded by 0, it won't affect the result.
    # we already defined the end as the end of the 15 mins earlier so this is the true end
    resultshypo <- data.frame(start = timehypo[minshypo == 1], end = timehypo[(minshypo == -1)[-1]]) %>%
      dplyr::summarise(sum(difftime(end, start, units = "secs")))


    cgmupload["min_spent_under_hypo3", f] <- base::round(base::sum(as.numeric(resultshypo) / 60))
    cgmupload["percent_time_under_hypo3", f] <- base::round((base::sum(as.numeric(resultshypo)) / totaltime) * 100, digits = 2)


    # more range variables
    # doesnt matter if these are consecutive or not or where they occured on which day as we just want to know how many times they were here
    # in range <3
    BGinrange4 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange4 <- ifelse(BGinrange4 %in% seq(0, 2.99, 0.01), 1, 0)
    cgmupload["min_spent_<3", f] <- base::round(base::sum(BGinrange4) * (interval / 60), digits = 2)
    cgmupload["percent_time_<3", f] <- base::round(((base::sum(BGinrange4) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)


    # in range 3-<3.9
    BGinrange1 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange1 <- ifelse(BGinrange1 %in% seq(3, 3.89, 0.01), 1, 0)
    cgmupload["min_spent_3_3.8", f] <- base::round(base::sum(BGinrange1) * (interval / 60), digits = 2)
    cgmupload["percent_time_3_3.8", f] <- base::round(((base::sum(BGinrange1) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)

    # in range 3.9-10
    BGinrange2 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange2 <- ifelse(BGinrange2 %in% seq(3.9, 10, 0.01), 1, 0)
    cgmupload["min_spent_3.9_10", f] <- base::round(base::sum(BGinrange2) * (interval / 60), digits = 2)
    cgmupload["percent_time_3.9_10", f] <- base::round(((base::sum(BGinrange2) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)

    # in range 3.9-7.8
    BGinrange3 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange3 <- ifelse(BGinrange3 %in% seq(3.9, 7.8, 0.01), 1, 0)
    cgmupload["min_spent_3.9_7.8", f] <- base::round(base::sum(BGinrange3) * (interval / 60), digits = 2)
    cgmupload["percent_time_3.9_7.8", f] <- base::round(((base::sum(BGinrange3) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)

    # over 10
    BGinrange10 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange10 <- ifelse(BGinrange10 > 10, 1, 0)
    cgmupload["min_spent_over10", f] <- base::round(base::sum(BGinrange10) * (interval / 60), digits = 2)
    cgmupload["percent_time_over10", f] <- base::round(((base::sum(BGinrange10) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)

    # over 13
    BGinrange13 <- base::as.numeric(table$sensorglucose[base::which(!is.na(table$sensorglucose))], length = 1)
    BGinrange13 <- ifelse(BGinrange13 > 13.9, 1, 0)
    cgmupload["min_spent_over13", f] <- base::round(base::sum(BGinrange13) * (interval / 60), digits = 2)
    cgmupload["percent_time_over13", f] <- base::round(((base::sum(BGinrange13) * (interval / 60)) * 60 / totaltime) * 100, digits = 2)


    # total AUC
    sensorBG <- base::as.numeric(table$sensorglucose, length = 1)
    xaxis <- base::seq(from = 0, length.out = base::length(sensorBG), by = (interval / 60))
    xaxis[base::which(is.na(sensorBG))] <- NA
    xaxis <- xaxis[!is.na(xaxis)]
    sensorBG <- sensorBG[!is.na(sensorBG)]
    aucs <- pracma::cumtrapz(xaxis, sensorBG)

    cgmupload["total_auc", f] <- base::round(aucs[base::length(sensorBG)], digits = 2)


    # Calculate MAGE
    # https://go.gale.com/ps/i.do?id=GALE%7CA252447314&sid=googleScholar&v=2.1&it=r&linkaccess=abs&issn=15209156&p=AONE&sw=w&userGroupName=loyoland_main
    # Smooth data using an exponentially weighted 9 point moving average, calculate SD of unsmoothed data.
    #require 12 hours of data for this
    #if (base::round(base::length(table$sensorglucose) / (3600 / interval)) > 12 & !is.null(length(table$sensorglucose))) {
    if (!is.null(length(table$sensorglucose))) {
      table$smoothed <- base::as.numeric(zoo::rollapply(zoo::zoo(table$sensorglucose),
        9, function(x) {
          c(1, 2, 4, 8, 16, 8, 4, 2, 1) %*%
            (x / 46)
        },
        fill = NA
      ))
      table$smoothed[1:4] <- base::mean(stats::na.omit(table$sensorglucose[1:4]))
      table$smoothed[(base::length(table$smoothed) - 3):base::length(table$smoothed)] <- base::mean(table$sensorglucose[(base::length(table$sensorglucose) - 3):base::length(table$sensorglucose)])
      # above is because smoothing doesnt work on first and last 4 values so sub in for their mean

      # SD of the
      sd <- stats::sd(table$sensorglucose)



      tryCatch({
      # Identify turning points, peaks, and nadirs.
      tpoints <- pastecs::turnpoints(table$smoothed)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
      tpointposition<- pastecs::extract(tpoints, no.tp = 0, peak = 1, pit = -1)
      peaks <- base::which(tpointposition==1)
      pits <- base::which(tpointposition==-1)



      # Calculate the difference between each nadir and its following peak. If the
      # data starts on a peak, remove it. Otherwise remove the final pit to create an even number of pits and peaks.
      if (tpoints[["firstispeak"]] == TRUE && base::length(peaks) != base::length(pits)) {
        peaks <- peaks[2:base::length(peaks)]
      } else if (tpoints[["firstispeak"]] == FALSE && base::length(peaks) != base::length(pits)) {
        pits <- pits[1:(base::length(pits) - 1)]
      }
      differences <- table$sensorglucose[peaks] - table$sensorglucose[pits]
      # if differecen between adjacent turning points are < 1sd (usually 1 but can be others) then it is not a turning point,
      #if there are no turning points then MAGE will be NaN
      #MAGE is Na if there wasnt 12 hours of data for this? maybe


      # Calculate the average of the differences greater than the entire dataset SD, 2SD, etc
      if (magedef == "1sd") {
        cgmupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > sd)])), digits = 2)

      }else if (magedef == "1.5sd") {
        cgmupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > (sd * 1.5))])), digits = 2)
      }else if (magedef == "2sd") {
        cgmupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > (sd * 2))])), digits = 2)
      }else {
        cgmupload["r_mage", f] <- base::round(base::mean(stats::na.omit(differences[base::which(differences > magedef)])), digits = 2)
      }
    }

    # J-index:combination of information from mean and SD of all glucose values
    cgmupload["j_index", f] <- base::round((0.001 * (base::mean(table$sensorglucose, na.rm = T) + stats::sd(table$sensorglucose, na.rm = T))^2), digits = 2)

    # CONGA:continuous overlapping net glycaemic action
    n <- (congan * 3600)
    conga.times <- table$timestamp + n
    conga.times <- conga.times[!is.na(conga.times)]
    conga.times <- conga.times[base::order(conga.times)]
    conga.times <- conga.times[base::which(conga.times %in% table$timestamp)]
    begin.times <- conga.times - n
    suppressWarnings(congas <- table$sensorglucose[base::which(table$timestamp %in% conga.times)] -
      table$sensorglucose[base::which(table$timestamp %in% begin.times)])
    cgmupload[base::paste0("conga_", congan), f] <- base::round(stats::sd(congas, na.rm = T), digits = 2)

    # MODD: mean of daily difference
    # if there is not a full day of data then the MODD will be NAN e when doing exercise analysis
    table$time <- lubridate::round_date(
      table$timestamp,
      "5 minutes"
    )
    table$time <- base::strftime(table$time,
      format = "%H:%M",
      tz = "UTC"
    )
    moddtable <- base::data.frame(base::matrix(
      ncol = 2,
      nrow = base::length(unique(table$time))
    ))
    base::colnames(moddtable) <- c("time", "mean_differences")

    moddtable$time <- base::unique(table$time)
    # For each time, calculate differences (absolute values) and average them.
    for (r in 1:nrow(moddtable)) {
      moddtable$mean_differences[r] <- base::mean(base::abs(base::diff(table$sensorglucose[base::which(table$time ==
        moddtable$time[r])])))
    }
    # Average the averages.
    cgmupload["modd", f] <- base::mean(stats::na.omit(moddtable$mean_differences))

    # LBGI and HBGI (based on dc1386 appendix)
    #

    #old mmol/l paramerters
    a <- 1.026
    b <- 1.861
    y <- 1.794

    table$gluctransform <- y * (((base::log(table$sensorglucose))^a) - b)
    table$rBG <- 10 * ((table$gluctransform)^2)
    rl <- table$rBG[base::which(table$gluctransform < 0)]
    if(pracma::isempty(rl)){
      rl<-0
    }

    rh <- table$rBG[base::which(table$gluctransform > 0)]
    if(pracma::isempty(rh)){
      rl<-0
    }

    cgmupload["lbgi", f] <- base::round(base::mean(stats::na.omit(rl)), digits = 2)
    cgmupload["hbgi", f] <- base::round(base::mean(stats::na.omit(rh)), digits = 2)

  }
  }

  # Write file.
  cgmupload <-
    base::cbind("Variable / Field Name" = rownames(cgmupload), cgmupload)
  if (format == "rows") {
    cgmupload <- base::as.data.frame(base::t(cgmupload))
    cgmupload <- cgmupload[-1, ]
  }
  filename <- base::paste0(outputdirectory, outputname, ".csv")
  rio::export(cgmupload, file = filename, row.names = FALSE)

  closeAllConnections()
}
