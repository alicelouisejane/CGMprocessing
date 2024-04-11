#' @title exercise_split
#'
#' @description This function splits up clean CGM data into timeperiods specfic to post exercise analysis: default 6 hours (changeable) post exercise, 24 hours post exercise, overnight (0000-0600), postexercise-0000, next day post exercise (0600-2400)
#' This function accepts clean CGM data in the format of the output of cleanCGM(). See README for details)
#' @returns Returns folders containing individual files for the corresponding split CGM data per individual.
#'  These files in each folder can then be run through the analyseCGM() function with the exercise parameter = TRUE to get CGM metrics within those specific periods and used for plotting CGM traces.
#' @param inputdirectory path to folder containing cleaned CGM files. See README on structure for these files follwoing the output of CleanCGM
#' @param outputdirectory path to folder where new folders will be created
#' @param exercisefile path to file containing list of exercise timestamps for every person in study. See README on structure for this file.
#' @param hourspostexercise numeric specified for the time of interest post exercise, defaults to 6
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate across contains filter select group_by inner_join slice ungroup arrange
#' @import tidyr
#' @import utils
#' @importFrom lubridate parse_date_time hours days
#' @author Alice Carr
#'
#' @export
#'
#' @seealso
#' analyseCGM and cleanCGM




exercise_split <- function(inputdirectory,outputdirectory,exercisefile,hourspostexercise) {

#load exercise file here:
exerciseinstance <- rio::import(exercisefile)

# files to split
  files <- base::list.files(path = inputdirectory, full.names = TRUE) # list of data files that have been ran through CGM clean (and prepare sheets if neccessary)

  # final empty lists for filling
  filter_fin_0 <- list()# filter of glucose from during exercise through until XX hours post exercise
  filter_fin_00 <- list() # filter of glucose from end of exercise until 0000
  filter_fin_06_24 <- list() # filter of glucose next day after end of exercise until
  filter_fin_00_06 <-list() # filter glucose during sleep
  filter_fin_24 <-list() # filter glucose from end of exercise to 24hrs after
  all_cgm <- list() # all cgm data
  exercise_characteristics <-list() #glucose at exact start and exact end only

  base::dir.create(paste0(outputdirectory,"data-", "after_",hourspostexercise, "hrs"), showWarnings = F)
  base::dir.create(paste0(outputdirectory,"data-after_0000/"), showWarnings = F)
  base::dir.create(paste0(outputdirectory,"data-after_0600_2400/"), showWarnings = F)
  base::dir.create(paste0(outputdirectory,"data-after_0000_0600"), showWarnings = F)
  base::dir.create(paste0(outputdirectory,"data-after_24"), showWarnings = F)
  base::dir.create(paste0(outputdirectory,"data-all"), showWarnings = F) # just added this so all files end up in one folder as well for ease later


  # dateparseorder <- c(
  #   "mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "dmy HM", "dmy HMS",
  #   "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS", "ymd HM", "ymd HMS",
  #   "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS"
  # )

  for (f in 1:base::length(files)) {
    table <- rio::import(files[f],guess_max = 1048576)

    table<-unique(table) #ensure the rep because of libre data isnt her
    table<-dplyr::filter(table,!is.na(sensorglucose))

    patid<-base::unlist(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_"))[1]
    exercise_type <- base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][2]

    print(paste(patid,exercise_type))

    # exercise timestamps that we log to split the cgm data on (table)
    exerciseinstance_pt <- exerciseinstance %>%
      dplyr::filter(pt_id == patid & type==exercise_type) %>%
      dplyr::mutate(startdatetime=base::as.POSIXct(startdatetime, tz = "UCT")) %>%
      dplyr::mutate(finishdatetime=base::as.POSIXct(finishdatetime, tz = "UCT")) %>%
      dplyr::mutate(durationmins = as.numeric(difftime(base::as.POSIXct(finishdatetime, tz = "UCT"), base::as.POSIXct(startdatetime, tz = "UTC"),units = "mins"))) %>%
      dplyr::arrange(startdatetime) %>%
      dplyr::select(pt_id,startdatetime,finishdatetime,type,durationmins)

    table$pt_id<-patid
    table$type<-exercise_type

    if (nrow(exerciseinstance_pt) == 0) {
      warning(paste(patid, exercise_type, "no exercise times check data (likely no exercise days recorded)"))
      next
    }

    # from exercise diary finding if we have the timestamp in the cgm data and match up to closed time stamp
    splitlist <- exerciseinstance_pt %>%
      dplyr::group_by(pt_id, finishdatetime) %>%
      dplyr::inner_join(table, by = c("pt_id","type"),multiple="all") %>%
      dplyr::mutate(diff = as.numeric(abs(difftime(timestamp,startdatetime,units = "hours")))) %>%
      dplyr::arrange(diff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::filter(diff<=24) %>% #if there are no matching CGM to exercise times within 24 hours
      select(-c(diff,sensorglucose))

    if (nrow(splitlist) == 0) {
      warning(paste(patid,exercise_type, "no matching time with CGM time check data"))
      next
    }

      # intermediate lists for the individual for splitting each timeperiod, as there may be multiple exercise sessions in an intervention period so we collect the timeperiods here
      filter_ls_00_06<-list() #sleep
      filter_ls_0 <- list() # time post exericse short timeperiod
      filter_ls_06_24<- list() # next day
      filter_ls_00<- list() # time from exercise end to midnight
      filter_ls_24<-list() # 24hrs from exercise end

      #filter during sleep
      for (h in 1:length(splitlist$finishdatetime)) {
        filter_ls_00_06[[h]] <-table %>%
          dplyr::filter(timestamp >= base::as.POSIXct(paste(as.Date(splitlist$finishdatetime[[h]]) +lubridate::days(1),"00:00:00"), tz = "UCT") &
                   timestamp < base::as.POSIXct(paste(as.Date(splitlist$finishdatetime[[h]])+lubridate::days(1),"06:00:00"), tz = "UCT")) %>%
          dplyr::mutate(start_split = splitlist$finishdatetime[[h]],
                        startdatetime=splitlist$startdatetime[[h]],
                        finishdatetime = splitlist$finishdatetime[[h]]) %>%
          dplyr::mutate(diff = as.numeric(difftime(timestamp, finishdatetime, units = "hours"))) %>%
        dplyr::mutate(diff_disc = ceiling(diff))
      }

      filter_fin_00_06[[f]] <- dplyr::bind_rows(filter_ls_00_06)


      # filter to 24h post exercise
      for (h in 1:length(splitlist$startdatetime)) {
          filter_ls_24[[h]] <-table %>%
            dplyr::filter(timestamp >= base::as.POSIXct(splitlist$finishdatetime[[h]], tz = "UCT") &
                   timestamp <=base::as.POSIXct(splitlist$finishdatetime[[h]] +lubridate::hours(24), tz = "UCT"))  %>%
            dplyr::mutate(start_split = splitlist$startdatetime[[h]],
                          startdatetime=splitlist$startdatetime[[h]],
                          finishdatetime = splitlist$finishdatetime[[h]]) %>%
            dplyr::mutate(diff = as.numeric(difftime(timestamp, finishdatetime, units = "hours")))  %>%
            dplyr::mutate(diff_disc = ceiling(diff)) %>%
            dplyr::mutate(diff_disc=ifelse(diff_disc<0,0,diff_disc))
      }

      filter_fin_24[[f]] <- dplyr::bind_rows(filter_ls_24)

      #filter for X hours after exercise
      # include time during exercise for use in analyseCGM under the exercise=T parameter

      for (h in 1:length(splitlist$startdatetime)) {
        filter_ls_0[[h]] <-table %>%
          dplyr::filter(timestamp >= base::as.POSIXct(splitlist$startdatetime[[h]], tz = "UCT") &
                   timestamp <=base::as.POSIXct(splitlist$finishdatetime[[h]] +lubridate::hours(hourspostexercise), tz = "UCT"))  %>%
          dplyr::mutate(start_split = splitlist$startdatetime[[h]],
                        startdatetime=splitlist$startdatetime[[h]],
                        finishdatetime = splitlist$finishdatetime[[h]]) %>%
          dplyr::mutate(diff = as.numeric(difftime(timestamp, finishdatetime, units = "hours")))  %>%
          dplyr::mutate(diff_disc = ceiling(diff)) %>%
          dplyr::mutate(diff_disc=ifelse(diff_disc<0,0,diff_disc))
      }

      filter_fin_0[[f]] <- dplyr::bind_rows(filter_ls_0)

      # filter until time from end of exercise to sleep
      for (h in 1:length(splitlist$finishdatetime)) {
        filter_ls_00[[h]] <-table %>%
          dplyr::filter(timestamp > splitlist$finishdatetime[[h]] & timestamp < base::as.POSIXct(paste(as.Date(splitlist$finishdatetime[[h]]) +lubridate::days(1),"00:00:00"), tz = "UCT"))  %>%
           dplyr::mutate(start_split = splitlist$finishdatetime[[h]],
                          startdatetime=splitlist$startdatetime[[h]],
                          finishdatetime = splitlist$finishdatetime[[h]]) %>%
            dplyr::mutate(diff = base::as.numeric(base::difftime(timestamp, finishdatetime, units = "hours"))) %>%
          dplyr::mutate(diff_disc = base::ceiling(diff))
      }

      filter_fin_00[[f]] <-dplyr::bind_rows(filter_ls_00)

      # filter from 0600 the next day after exercise to midnight that next day
      for (h in 1:length(splitlist$finishdatetime)) {
        filter_ls_06_24[[h]] <-table %>%
          dplyr::filter(timestamp >= base::as.POSIXct(paste(as.Date(splitlist$finishdatetime[[h]]) +lubridate::days(1),"06:00:00"), tz = "UCT") &
                   timestamp< base::as.POSIXct(paste(as.Date(splitlist$finishdatetime[[h]]) +lubridate::days(2),"00:00:00"), tz = "UCT"))  %>%

                   dplyr::mutate(start_split = splitlist$finishdatetime[[h]],
                                             startdatetime=splitlist$startdatetime[[h]],
                                             finishdatetime = splitlist$finishdatetime[[h]]) %>%
                   dplyr::mutate(diff = as.numeric(difftime(timestamp, finishdatetime, units = "hours"))) %>%
                   dplyr::mutate(diff_disc = ceiling(diff))
      }

      filter_fin_06_24[[f]] <-dplyr::bind_rows(filter_ls_06_24)

      allcgm<-table %>%
        dplyr::inner_join(exerciseinstance_pt, by = c("pt_id","type"),multiple="all") %>%
        dplyr::mutate(diff_finish = as.numeric(difftime(timestamp, finishdatetime, units = "hours"))) %>%
        dplyr::mutate(diff_start = as.numeric(difftime(timestamp, startdatetime, units = "hours"))) %>%
        dplyr::mutate(diff_disc = ifelse(diff_finish>0,ceiling(diff_finish),
                                         ifelse(timestamp>=startdatetime & timestamp<=finishdatetime,0,
                                                ifelse(diff_start<0,floor(diff_start),NA))))

# start time exercise characteristics
      exercise_characteristics_s<- exerciseinstance_pt %>%
        dplyr::group_by(pt_id, finishdatetime) %>%
        dplyr::inner_join(table, by = c("pt_id","type"),multiple="all") %>%
        dplyr::mutate(diff_start = abs(as.numeric(difftime(timestamp,startdatetime, units = "mins")))) %>%
        dplyr::arrange(diff_start) %>%
        dplyr::filter(diff_start<=5) %>% # ensure start glucose is within 5 mins of start time
        dplyr::arrange(diff_start) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        rename(start_sensorglucose=sensorglucose) %>%
        select(pt_id, type,startdatetime,finishdatetime,durationmins,start_sensorglucose)

      # end time exercise characteristics
      exercise_characteristics_f<- exerciseinstance_pt %>%
        dplyr::group_by(pt_id, finishdatetime) %>%
        dplyr::inner_join(table, by = c("pt_id","type"),multiple="all") %>%
        dplyr::mutate(diff_end = as.numeric(abs(difftime(timestamp,finishdatetime)))) %>%
        dplyr::arrange(diff_end) %>%
        dplyr::filter(diff_end<=5) %>% # ensure end glucose is within 5 mins of end time
        dplyr::arrange(diff_end) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        rename(end_sensorglucose=sensorglucose) %>%
        select(pt_id, type,startdatetime,finishdatetime,durationmins,end_sensorglucose)


      exercise_characteristics[[f]]<- base::merge(exercise_characteristics_s,exercise_characteristics_f)

    all_cgm[[f]] <- allcgm


  }

  # all CGM trace to plot the trace if needed
  all_cgm_fin <- bind_rows(all_cgm[!sapply(all_cgm, is.null)])
  rio::export(all_cgm_fin,paste0(outputdirectory,"/all_CGM.csv")) # this is all data from all patients in one file

  all_characteristics_fin <- bind_rows(exercise_characteristics[!sapply(exercise_characteristics, is.null)])
  rio::export(all_characteristics_fin,paste0(outputdirectory,"/exercisecharactersitics.xlsx"))

  #write files
  #sleep
    output_fin_00_06 <- filter_fin_00_06[!sapply(filter_fin_00_06, is.null)]
    new.names.00_06 <- list() # create lst to store names for dfs
    for (i in 1:length(output_fin_00_06)) {
      new.names.00_06[i] <- paste0(output_fin_00_06[[i]]$pt_id[1], "_",output_fin_00_06[[i]]$type[1],"_00_06", collapse = ",") # file name Id_timepoint
      rio::export(output_fin_00_06[[i]], file = paste0(outputdirectory,"data-after_0000_0600/", new.names.00_06[i], ".csv")) # write file
      rio::export(output_fin_00_06[[i]], file = paste0(outputdirectory,"data-all/", new.names.00_06[i], ".csv")) # write file
    }

    # end to 0000
    output_fin_00 <- filter_fin_00[!sapply(filter_fin_00, is.null)]
    new.names.00 <- list() # create lst to store names for dfs
    for (i in 1:length(output_fin_00)) {
      new.names.00[i] <- paste0(output_fin_00[[i]]$pt_id[1], "_",output_fin_00[[i]]$type[1],"_00", collapse = ",") # file name Id_timepoint
      rio::export(output_fin_00[[i]], file = paste0(outputdirectory,"data-after_0000/", new.names.00[i], ".csv")) # write file
      rio::export(output_fin_00[[i]], file = paste0(outputdirectory,"data-all/", new.names.00[i], ".csv")) # write file

    }

    # end to selected time after
    output_fin_0 <- filter_fin_0[!sapply(filter_fin_0, is.null)]
    new.names.0 <- list() # create lst to store names for dfs
    for (i in 1:length(output_fin_0)) {
      new.names.0[i] <- paste0(output_fin_0[[i]]$pt_id[1], "_",output_fin_0[[i]]$type[1],"_",hourspostexercise, collapse = ",") # file name Id_timepoint
      rio::export(output_fin_0[[i]], file = paste0(outputdirectory,"data-after_", hourspostexercise, "hrs/", new.names.0[i], ".csv")) # write file
      rio::export(output_fin_0[[i]], file = paste0(outputdirectory,"data-all/", new.names.0[i], ".csv")) # write file

    }

    # end to 24h after
    output_fin_24 <- filter_fin_24[!sapply(filter_fin_24, is.null)]
    new.names.24 <- list() # create lst to store names for dfs
    for (i in 1:length(output_fin_24)) {
      new.names.24[i] <- paste0(output_fin_24[[i]]$pt_id[1], "_",output_fin_24[[i]]$type[1],"_24", collapse = ",") # file name Id_timepoint
      rio::export(output_fin_24[[i]], file = paste0(outputdirectory,"data-after_24/", new.names.24[i], ".csv")) # write file
      rio::export(output_fin_24[[i]], file = paste0(outputdirectory,"data-all/", new.names.24[i], ".csv")) # write file

    }

    #next day
    output_fin_06_24 <- filter_fin_06_24[!sapply(filter_fin_06_24, is.null)] %>%
      purrr::discard(~ nrow(.x) == 0)

    new.names.06_24 <- list() # create lst to store names for dfs
    for (i in 1:length(output_fin_06_24)) {
      new.names.06_24[i] <- paste0(output_fin_06_24[[i]]$pt_id[1], "_",output_fin_06_24[[i]]$type[1],"_06_24", collapse = ",") # file name Id_timepoint
      rio::export(output_fin_06_24[[i]], file = paste0(outputdirectory,"data-after_0600_2400/", new.names.06_24[i], ".csv")) # write file
      rio::export(output_fin_06_24[[i]], file = paste0(outputdirectory,"data-all/", new.names.06_24[i], ".csv")) # write file

    }

}
