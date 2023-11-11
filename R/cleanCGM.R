#' @title cleanCGM
#'
#' @description This function cleans and standardizes raw Continuous glucose monitoring (CGM) devices used in diabetes management. This function is optimised for raw Dexcom or libre data however has some functionality to handle pre coalesced trial data bases.
#' Remember to always check your data structures and follow the guide in the README for more help optimising the cose for your specific needs.
#' @returns Returns individual files of CGM data that have variable names standardized, missing glucose removed, high and low limits re coded to specific sensor limits. Option of "other" in
#' argument device is specific to data that is coalesced into one sheet (ie multiple device uploads + multiple people in 1 file) and may use CGM with calibrations (for eg JAEB data).
#' detailed output of percentage wear and sensor drop out and optional CGM graphs created.
#' Cleaned files can then be run through the analyseCGM and/or exercise_split functions
#'
#' @param inputdirectory path to folder containing raw (or in house preprocessed) files. Preferred csv format but can read in others.
#'
#' @param outputdirectory path to folder where cleaned output files will be stored
#'
#' @param device Use "other" if CGM required calibrations. Options are can be any dexcom (g4,g6etc), libre (1,2,pro), or "other." Important to specify what device is being used for the alignment of standardized variable names and to ensure sensor limits, ensure device type is also in the cgm dictionary next to variables.
#'  "Other" is specified if dealing with data that requires calibration or is in an alternative layout. Gap testing and standard percentage wear and droupout measures will not be created with this but number of days worn will be reported. Edit code as necessary if sensor is other and calibration is false
#'
#' @param calibration only works for device = "other" TRUE/FALSE Default is FALSE. The majority of CGM data will not require a calibration, this is sensor dependant. For guardian sensors, dexcom g4 (and older generations) and medtronic ipro2 blood calibrations were required. The layout of the data may effect how this argument works please ensure calibration blood glucoses are labelled in a recordtype variable in the raw data (ie. in JAEB data)
#'
#' @param removerow TRUE/FALSE Default is TRUE. used when raw data has additional first row before variables. Usually encountered in freestyle libre. If sure this doesn't exist use F. Default to T otherwise which will check if row exists and act accordingly
#'
#' @param nrow used in conjunction with removerow=T. Number of rows to removed before variable names. Requires inspection of data files
#'
#' @param expectedwear Default is full, can acccept a numeric. This indicates if a participant was expected to wear the CGM for the lifetime of the sensor or for a particular number of days in a study. Enter the number of days expected as a numeric if so.
#'
#' @param saveplot TRUE/FALSE Default is True. Save the overall CGM plot over the total number of days of wear. Will not generate if device type is "other"
#'
#' @param impute - for development. needs fixing and implementing correctly
#'
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate summarise n lead across contains filter select group_by inner_join slice ungroup arrange bind_rows rename
#' @import tidyr
#' @importFrom cowplot plot_grid ggdraw draw_label
#' @import ggplot2
#' @import hms
#' @importFrom tools file_path_sans_ext
#' @importFrom janitor clean_names
#' @importFrom anytime anytime
#' @importFrom here here
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
                     outputdirectory = tempdir(),
                     device = "dexcomg6",
                     calibration = F,
                     removerow=T,
                     nrow=3,
                     expectedwear="full",
                     impute=F,
                     saveplot=T) {


  #cgm variable dictionary in documentation data file. Edit source file as necessary if using other CGM
  file_path <- here::here("inst/extdata", "cgmvariable_dictionary.xlsx")

  cgm_dict<-rio::import(file_path)

  # Read in data: anticipated structure is a single file containing raw CGM downloads per individual
  files <- base::list.files(path = inputdirectory, full.names = TRUE)

  # output directory is created and lists initialised
  base::dir.create(outputdirectory, showWarnings = FALSE)
  gaptestoutput<-list()
  data_collected_output<-list()


  # Step 1: clean the CGM data
  for (f in 1:base::length(files)) {

    #id from filename (used only in dexcom and libre if device is other then this is irrelavent)
    Id <- tools::file_path_sans_ext(basename(files[f]))
    Id <- gsub("^([^_]+_[^_]+).*", "\\1", Id)
    print(Id)


    table <-  base::suppressWarnings(rio::import(files[f], guess_max = 10000000))

    #if dealing with some libre raw data there may be additional rows added,
    #if youre certain there arent then use FALSE otherwise TRUE can handle both ways
    if(removerow==T & grepl("V[0-9]",names(table)[1])){
    names(table)<-table[nrow,]
    table<-table[-(1:nrow), ]
    } else if(removerow==T |!grepl("V[0-9]",names(table)[1])){
      table<-table
    }else if(removerow==F){
      table<-table
    }

    #lower case and space removed from names
    table<- janitor::clean_names(table)

    #indicates what device we are using the data from, possibly important to keep track of
    cgm_dict<-filter(cgm_dict,type==device)
    device_vars<- cgm_dict[cgm_dict$old_vars %in% names(table), ]

    # rename the variables to standardised variables names
    colnames(table) <- dplyr::recode(
      colnames(table),
      !!!setNames(as.character(cgm_dict$new_vars), cgm_dict$old_vars))

    # try to anticipate problematic dates
    if(is.character(table$timestamp)){
      table$timestamp <-as.POSIXct(lubridate::parse_date_time(table$timestamp, orders = c("dmy HMS","dmy HM","mdy HMS","mdy HM")),tz="UTC")
      table$timestamp <-anytime::anytime(table$timestamp, tz = "UTC")

    } else if(!is.character(table$timestamp)){
      table$timestamp <-anytime::anytime(table$timestamp, tz = "UTC")
    }

    #find what the interval in the data is ie. 5min for dexcom 15 min for libre
    interval <- pracma::Mode(base::diff(base::as.numeric(table$timestamp)/60))

    #convert to mmol/l - if loop tests if glucose is mg/dl as would have higher max value than what would be max in mmol/l
    if(max(table$sensorglucose, na.rm = T)>30){
      table$sensorglucose<-round(table$sensorglucose/18,digits = 2)
    }

    # high and low limits from :
    #https://uk.provider.dexcom.com/sites/g/files/rrchkb126/files/document/2021-09/LBL017451%2BUsing%2BYour%2BG6%2C%2BG6%2C%2BUK%2C%2BEN%2C%2Bmmol_0.pdf
    # suggests libre 2 has different limits to libre 1 but this is not the case whan i look at my own data
    #https://www.freestyle.abbott/us-en/support/faq.html?page=device/freestyle-libre-2-system/faq/topic/reader

    if (grepl("dexcom",device_vars$type[1])) {
      #change instances of low/ high to sensor limits
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("low", sensorglucose, ignore.case = TRUE) ~ "2.2",
          grepl("high", sensorglucose, ignore.case = TRUE)  ~ "22.2",
          TRUE ~ table$sensorglucose
        ))
      )
      #for plotting:
      sensormin=2.2
      sensormax=24

    } else if (grepl("^libre$",device_vars$type[1])) {
      #DO NOT INCLUDE ANY OTHER RECODS OTHER THAN CGM ie. NOT scanglucose
      table <- dplyr::filter(table,recordtype==0)
      table<-dplyr::select(table,-c(recordtype,scanglucose))
      table$sensorglucose <- as.character(table$sensorglucose)
      base::suppressWarnings(
        table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
          grepl("lo", sensorglucose, ignore.case = TRUE)  ~ "2.2",
          grepl("hi", sensorglucose, ignore.case = TRUE)  ~ "27.8",
          TRUE ~ table$sensorglucose
        ))
      )
      #for plotting:
      sensormin=2.2
      sensormax=28

    } else if(unique(device_vars$type)=="other" | unique(device_vars$type=="dexcomg4")){
      #if device type is "other" then calibration is likely necessary this is the loop it will be handled in
      # first identify record types that are calibration and remove them from the main table

      if(calibration==T){
      calibration<-dplyr::filter(table,grepl("calib",table$recordtype,ignore.case = T)) %>%
        dplyr::rename("timestampfp"="timestamp") %>%
        dplyr::rename("fingerprickglucose"="sensorglucose") %>%
        data.table::as.data.table() %>%
        dplyr::mutate(timestampfp_copy=timestampfp)

      table_cal<-dplyr::filter(table,!grepl("calib",table$recordtype,ignore.case = T)) %>%
        data.table::as.data.table() %>%
        mutate(date=as.Date(timestamp))

      # Set keys for data.tables
      data.table::setkey(calibration, id, timestampfp)
      data.table::setkey(table_cal, id, timestamp)

      # Perform a rolling join to merge the data.tables based on the closest timestamp
      calibration_merged <- table_cal[calibration, roll = "nearest"] %>%
        dplyr::mutate(diff = as.numeric(abs(timestamp - timestampfp_copy))) %>%
        dplyr::mutate(nocalibration=ifelse(diff>900,1,NA)) %>%  # if the matching glucose is > 15 mins then ensure to mark
        filter(is.na(nocalibration)) %>% # remove these
        dplyr::ungroup() %>%
        dplyr::mutate(date=as.Date(timestamp)) %>%
        group_by(id,date) %>%
        dplyr::mutate(MD = mean(abs((fingerprickglucose - sensorglucose )/ fingerprickglucose) * 100)) %>%
        dplyr::mutate(remove=ifelse(floor(MD)>20,1,NA)) %>%
        dplyr::mutate(calibrationperformed=1) %>%
        select(id,remove,date,MD,calibrationperformed) %>%
        group_by(id,date) %>%
        dplyr::mutate(num_calibrations_perday=sum(calibrationperformed,na.rm=T)) %>%
        unique()

      datesincgm<-table_cal %>%
        dplyr::mutate(date=as.Date(timestamp)) %>%
        dplyr::select(id,date) %>%
        unique() %>%
        merge(calibration_merged, by=c("id","date"), all=T) %>%
        dplyr::mutate(remove=ifelse(is.na(MD),1,remove)) %>% # if missing MD this means calibration were not perfomed on this day. usually calibrations are done everyday in those sensor that required calibration. these dates should also be removed from CGM
        dplyr::group_by(id) %>%
        dplyr:: mutate(num_days_wear=dplyr::n()) %>%
        dplyr::mutate(num_days_calibration=sum(calibrationperformed,na.rm = T)) %>%
        dplyr::mutate(num_days_remove_calibration=sum(remove,na.rm=T)) %>%
        dplyr::mutate(average_calibrations_perday_overtimeofwear= mean(num_calibrations_perday,na.rm=T))

      remove <- select(datesincgm,id,remove,date) %>%
        filter(remove==1)

      # remove bad calibrated days/ days where no calibration was performed
      table <- table_cal %>%
        merge(remove, by=c("id","date"), all=T) %>%
        filter(is.na(remove)) %>%
        select(-remove)

      table <- table[base::order(table$id,table$timestamp), ]
# assessing sensor drop out and not taking into account days removed because of calibration
      gaptest<-table %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(diff=as.numeric(difftime(timestamp,dplyr::lead(timestamp), units = "mins"))) %>%
        dplyr::mutate(gap=ifelse(abs(diff)>interval+1,1,0)) %>%
        dplyr::filter(gap==1) %>%
        dplyr::mutate(gapcount=sum(gap)) %>%
        dplyr::mutate(gaptime=paste(timestamp,"length:",abs(diff),"mins")) %>%
        dplyr::select(gaptime,gapcount,timestamp,diff) %>%
        dplyr::filter(diff/60>23.5) # focuses only on gaps that are from either the same time of wear or removes where a gap now exisits becuase of calibration removal

      #store this as a dataframe
      gaptestoutput[[f]]<-table %>%
        group_by(id) %>%
        mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
        mutate(gap=ifelse(abs(diff)>interval+1,1,0)) %>%
        filter(gap==1)
        select(id,timestamp,diff)

#you can edit this code further to include measures for percentage expected wear etc depending on your needs
        # as this data is expected to be coalesed there with be multiple sensor uploads from the same person
        # unsure on what the expected time of wear should therefore be but if you know this then edit as neccessary follwing the loops that are below

      # summarise data collected -  different for when processing raw data
      # and is aimed at when all data was put into 1 dataframe like in JAEB and not separate files
      # and if there were calibrations performed
      data_collected<- datesincgm %>%
        select(id,num_days_wear,num_days_calibration,num_days_remove_calibration,average_calibrations_perday_overtimeofwear) %>%
        unique()

      data_collected_output[[f]]<-data_collected

      }else if(calibration==F){
        table<-table

        gaptest<-table %>%
          group_by(id) %>%
          mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
          mutate(gap=ifelse(abs(diff)>interval+1,1,0)) %>%
          filter(gap==1) %>%
          mutate(gapcount=sum(gap)) %>%
          mutate(gaptime=paste(timestamp,"length:",abs(diff),"mins")) %>%
          select(gaptime,gapcount,timestamp,diff) %>%
          filter(diff/60>23.5) # focuses only on gaps that are from either the same time of wear or removes where a gap now exisits becuase of calibration removal

        #store this as a dataframe
        gaptestoutput[[f]]<-table %>%
          group_by(id) %>%
          mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
          mutate(gap=ifelse(abs(diff)>interval+1,1,0)) %>%
          filter(gap==1)
        select(id,timestamp,diff)

        }
    }

    #this get rid of the first lines in dexcom as all these rows miss a timestamp
    #but also gets rid of any problematic missing rows
    table <- dplyr::filter(table,!is.na(timestamp))

    # keep only variables of interest
    vars_to_keep <- dplyr::intersect(names(table), unique(cgm_dict$new_vars))

    table$device<-device_vars$type[1]

    table<-dplyr::select(table,c(all_of(vars_to_keep),contains("percent"))) %>%
      dplyr::select(-contains("record"))

    #change sensor id to be patient id take from filename
    if(unique(device_vars$type!="other")){
      table$id <- Id
    }

    #make sure glucose is numeric
    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))

    #order by timestamp
    table <- table[base::order(table$timestamp), ]


    # test for sensor drop out/ gaps in data. This section will output a summary file for later use containing percentage wear and drop out
    if(grepl("libre",device_vars$type[1])) {

    gaptest<-table %>%
      mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
      mutate(gap=ifelse(abs(diff)>16,1,0)) %>%
      filter(gap==1) %>%
      mutate(gapcount=sum(gap)) %>%
      mutate(gaptime=paste(timestamp,"length:",abs(diff),"mins")) %>%
      select(gaptime,gapcount,timestamp,diff)

    gaptestoutput[[f]]<-table %>%
      mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
      mutate(gap=ifelse(abs(diff)>16,1,0)) %>%
      filter(gap==1) %>%
      select(timestamp,diff) %>%
      mutate(subject_id=Id)
    } else if(grepl("dexcomg[6-9]|[1-9][0-9]+",device_vars$type[1])) {
      gaptest<-table %>%
        mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
        mutate(gap=ifelse(abs(diff)>6,1,0)) %>%
        filter(gap==1) %>%
        mutate(gapcount=sum(gap)) %>%
        mutate(gaptime=paste(timestamp,"length:",abs(diff),"mins")) %>%
        select(gaptime,gapcount,timestamp,diff)

      gaptestoutput[[f]]<-table %>%
        mutate(diff=as.numeric(difftime(timestamp,lead(timestamp), units = "mins"))) %>%
        mutate(gap=ifelse(abs(diff)>6,1,0)) %>%
        filter(gap==1) %>%
        select(timestamp,diff) %>%
        mutate(subject_id=Id)
    }


if(unique(device_vars$type!="other")){

  if(nrow(gaptest)>0){

    #in this study a participant was expected to wear CGM for at least 24 hours after the finish exercise time
    percentageexpectedwear <- table

    if(grepl("dexcomg[6-9]|[1-9][0-9]+",device_vars$type[1])) {

    if(expectedwear=="full"){
    expectedtime<-24*10 # 5 min readings => 10days =12 lots of 5 mins in 60 -> 24hrs a day -> 10 days
    expectedtime_mins<-12*24*10
    }else if(is.numeric(expectedwear)){
      expectedtime<-24*expectedwear
      expectedtime_mins<-12*24*expectedwear
    }

    percentage_expectedwear_overstudy<-as.numeric((round(difftime(max(percentageexpectedwear$timestamp), min(percentageexpectedwear$timestamp), units = "hours"))/expectedtime)*100)

    # look for percentage drop out during the time of interest
    percentage_dropout <- gaptest %>%
      dplyr::summarise(totallosttime=sum(abs(diff)))

    #this is the total amount of time in mins that we were expecting and the sum total of the timegaps in mins that we have
    percentage_datacollected_overstudy<-((expectedtime_mins-percentage_dropout$totallosttime)/expectedtime_mins)*100

    percentage_dropout_overstudy<-((percentage_dropout$totallosttime)/expectedtime_mins)*100


    gaptest_study <- gaptest

    data_collected<-data.frame(subject_id = Id,
                               percentage_expectedwear_overstudy=percentage_expectedwear_overstudy,
                               percentage_datacollected_overstudy=percentage_datacollected_overstudy,
                               percentage_dropout_overstudy=percentage_dropout_overstudy)

    data_collected_output[[f]]<-data_collected
    }else if(grepl("libre",device_vars$type[1])) {
      if(expectedwear=="full"){
        expectedtime<-24*14 # 15 min readings => 14days =4 lots of 15 mins in 60 -> 24hrs a day -> 14 days
        expectedtime_mins<-4*24*10
      }else if(is.numeric(expectedwear)){
        expectedtime<-24*expectedwear
        expectedtime_mins<-12*24*expectedwear
      }


        percentage_expectedwear_overstudy<-as.numeric((round(difftime(max(percentageexpectedwear$timestamp), min(percentageexpectedwear$timestamp), units = "hours"))/expectedtime)*100)

        # look for percentage drop out during the time of interest which is startexercise up to 24 hours after end of exercise
        percentage_dropout <- gaptest %>%
          dplyr::summarise(totallosttime=sum(abs(diff)))

        #this is the total amount of time in mins that we were expecting and the sum total of the timegaps in mins that we have
        percentage_datacollected_overstudy<-((expectedtime_mins-percentage_dropout$totallosttime)/expectedtime_mins)*100

        percentage_dropout_overstudy<-((percentage_dropout$totallosttime)/expectedtime_mins)*100


        gaptest_study <- gaptest

        data_collected<-data.frame(subject_id = Id,
                                   percentage_expectedwear_overstudy=percentage_expectedwear_overstudy,
                                   percentage_datacollected_overstudy=percentage_datacollected_overstudy,
                                   percentage_dropout_overstudy=percentage_dropout_overstudy)

        data_collected_output[[f]]<-data_collected
      }
} else if(nrow(gaptest)==0){

  percentageexpectedwear <- table

  if(grepl("dexcom",device_vars$type[1])) {
    expectedtime<-24*10 # 5 min readings => 10days =12 lots of 5 mins in 60 -> 24hrs a day -> 10 days
    expectedtime_mins<-12*24*10

    percentage_expectedwear_overstudy<-as.numeric((round(difftime(max(percentageexpectedwear$timestamp), min(percentageexpectedwear$timestamp), units = "hours"))/expectedtime)*100)


    #this is the total amount of time in mins that we were expecting and the sum total of the timegaps in mins that we have
    percentage_datacollected_overstudy<-100

    percentage_dropout_overstudy<-0


    gaptest_study <- gaptest

    data_collected<-data.frame(subject_id = Id,
                               percentage_expectedwear_overstudy=percentage_expectedwear_overstudy,
                               percentage_datacollected_overstudy=percentage_datacollected_overstudy,
                               percentage_dropout_overstudy=percentage_dropout_overstudy)

    data_collected_output[[f]]<-data_collected
  }else if(grepl("libre",device_vars$type[1])) {
    expectedtime<-24*14 # 15 min readings => 14days =4 lots of 15 mins in 60 -> 24hrs a day -> 14 days
    expectedtime_mins<-4*24*10

    percentage_expectedwear_overstudy<-as.numeric((round(difftime(max(percentageexpectedwear$timestamp), min(percentageexpectedwear$timestamp), units = "hours"))/expectedtime)*100)

    #this is the total amount of time in mins that we were expecting and the sum total of the timegaps in mins that we have
    percentage_datacollected_overstudy<-100

    percentage_dropout_overstudy<-0

    gaptest_study <- gaptest

    data_collected<-data.frame(subject_id = Id,
                               percentage_expectedwear_overstudy=percentage_expectedwear_overstudy,
                               percentage_datacollected_overstudy=percentage_datacollected_overstudy,
                               percentage_dropout_overstudy=percentage_dropout_overstudy)

    data_collected_output[[f]]<-data_collected

  }
}
}

     table<-filter(table,!is.na(sensorglucose))

if(unique(device_vars$type!="other")){
    graph1<-table %>%
      mutate(date=as.Date(timestamp)) %>%
      mutate(time=hms::as_hms(timestamp)) %>%
      ggplot(aes(x = as.POSIXct(time, format = "%H:%M:%S"), y = sensorglucose)) +
      geom_path(aes(group=as.factor(date),colour=as.factor(date)), colour="grey")+
      labs(x = "Time", y = "Glucose", title=paste("Summary of CGM wear over:",as.numeric(round(difftime(max(table$timestamp),min(table$timestamp),"days"))),"days","\n Raw data collected:",data_collected$percentage_datacollected_overstudy,"%")) +
      theme_minimal() +
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "2 hours") +
      stat_summary(aes(fill = "Median hilow"),fun.data = median_hilow, geom = "ribbon",alpha = 0.5, colour = "darkblue", show.legend = T)+
      stat_summary(aes(fill = "IQR"),fun.data = function(x) {
        y <- quantile(x, c(0.25, 0.75))
        names(y) <- c("ymin", "ymax")
        y
      }, geom = "ribbon", colour = "lightblue", alpha = 0.5,show.legend = T) +
      theme(
        legend.position = c(0.85, 0.9),  # Adjust the position of the legend box (top-right corner)
        legend.background = element_rect(fill = "white", color = "black"),  # Customize the legend box
        legend.key.size = unit(0.5, "cm"),  # Adjust the size of the legend keys
        legend.text = element_text(size = 10),  # Adjust the size of the legend text
        legend.title = element_text(size = 12, face = "bold")  # Adjust the size and style of the legend title
      ) +
      scale_fill_manual("Key",values = c("lightblue","darkblue")) +
      scale_y_continuous(limits = c(2,(sensormax)),breaks=c(seq(2,sensormax,2)))

    graphoutput_title<-cowplot::ggdraw(cowplot::plot_grid(
      NULL,
      graph1,
      ncol = 1,
      rel_heights = c(0.07, 0.93)
    )) +
      cowplot::draw_label(paste("Patient ID:",Id), x = 0.5, y = 0.95, hjust = 0.5, fontface = "bold", size = 14)

    if(saveplot==T){
      base::dir.create("graphs/", showWarnings = FALSE)
      #save the plot, all patients
      ggsave(paste("graphs/",Id,"summaryCGM.pdf"),graphoutput_title, width=6,height=6)
    }
}
    #output
    table$date <- as.Date(table$timestamp)
    filename <- base::paste0(outputdirectory, "/", basename(files[f]))
    table<-select(table, c(id,date,timestamp,sensorglucose))
    rio::export(table, file = filename)
  }

  gaptestfinaloutput <- bind_rows(gaptestoutput[!sapply(gaptestoutput, is.null)])
  rio::export(gaptestfinaloutput,"output/gap_info.xlsx")

  data_collected_output_final<-bind_rows(data_collected_output[!sapply(data_collected_output, is.null)])
  rio::export(data_collected_output_final,"output/percentage_data_collected_info.xlsx")

}
