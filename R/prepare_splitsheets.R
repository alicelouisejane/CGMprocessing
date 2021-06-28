## code to prepare files goes here
#Identify where raw files are
files <- paste("EXTOD education/data-raw/",base::list.files(path = "EXTOD education/data-raw/" ),sep="") #input path of folder with raw files in first argument. Input name of the folder in the second argument
#variables that are in the raw files (names cleaned in first loop) these are identified by inspection of the output from the first loop in outputdirectory_1

#Read in variable renameing dictionary
var_dict<- rio::import("cgmvariable_dictionary.xlsx")


# output of first loop stored
outputdirectory_1<-"EXTOD education/data-split/"
base::dir.create(outputdirectory_1,showWarnings = FALSE)
# outout of second loop stored
outputdirectory_2<-"EXTOD education/data-preprocessed/"
base::dir.create(outputdirectory_2,showWarnings = FALSE)

#First loop splits sheets from an excel file that have combined ie.baseline 6 months and 12 months sheets And stores to the data-split folder
#IMPORTANT this loop only needs to be ran for EXTOD batch NOT EXTOD education
 for (f in 1:base::length(files)) {
    Id<-tools::file_path_sans_ext(basename(files[f]))
    sheets <- readxl::excel_sheets(files[f])
    filenames <- file.path(outputdirectory_1, paste0(Id,"_",sub(" ","",tolower(sheets)), ".csv"))
    dats <- lapply(sheets, readxl::read_excel, path = files[f], col_types=c("text", "text", "guess", "text", "guess", "text", "guess"))
    dats.low<-lapply(dats, function(x) { setNames(x, tolower(names(x))) } )
    dats.nam<-lapply(dats.low, function(x) { setNames(x, gsub("\\s+|\\.+", "_",names(x))) } )
    lapply(seq_along(dats.nam), function(i) readr::write_csv(dats.nam[[i]], filenames[i]))
    invisible()
  }


#Second loop reads over these files and splits up files that had none consecuative dates as this suggests data was not wiped from DEXCOM before applying to patient
# Read in data-split files and store outputs in the folder data
files <- base::list.files(path = outputdirectory_1,full.names = TRUE)
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")


  for (f in 1:base::length(files)) {
    Id<-base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][1]
    table <- rio::import(files[f],which = 1, guess_max = min(8400, n_max = NULL)) #guess max increases the maximum row we read in to guess the column type important for not converting high and low to NA
    #change names to lower
    table<- setNames(table, tolower(names(table)))
    table<- setNames(table, gsub("\\s+|\\.+", "_",names(table)))
    table<- data.table::setnames(table, old = var_dict$old_vars, new = var_dict$new_vars, skip_absent=TRUE)
    table$id<- base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][1]
    table <- dplyr::select(table,c('id','timestampfp','fingerprickglucose','timestamp','sensorglucose'))

    length<-sum(!is.na(table$timestampfp))
    #moves columns up-required for extod education batch
    if(is.na(table$timestamp[1])){
      table <-useful::shift.column(data=table, columns=c("timestamp","sensorglucose"), len=length)
      table$timestamp<-table$timestamp.Shifted
      table$sensorglucose<-table$sensorglucose.Shifted
      table<-table[-c(6,7)]
    }
    table$timestamp <-
      base::as.POSIXct(lubridate::parse_date_time(table$timestamp,
                                                  dateparseorder),tz = "UTC")

    table$timestampfp <-
      base::as.POSIXct(lubridate::parse_date_time(table$timestampfp,
                                                  dateparseorder),tz = "UTC")

    require(dplyr)
    table$sensorglucose<-as.character(table$sensorglucose)
    base::suppressWarnings(
      table <- table %>% dplyr::mutate(sensorglucose = dplyr::case_when(
        sensorglucose=="Low"~ "2.2",
        sensorglucose=="High"~"22.2",
        TRUE ~ table$sensorglucose))
    )

    table$fingerprickglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$fingerprickglucose),digits = 2))
    table$sensorglucose <-
      base::suppressWarnings(base::round(base::as.numeric(table$sensorglucose), digits = 2))
    table <- table[base::order(table$timestamp),]


    table<-table[order(as.Date(table$timestamp)),]

    #splits up sensor and calibration into 2 separate dfs
    tablesensor<-table[,c('timestamp','sensorglucose')]
    tablesensor_Date<-as.Date(tablesensor$timestamp)
    tablesensor_consecutive <- c(NA,diff(tablesensor_Date)>1)

    calibration<-table[,c('id','timestampfp','fingerprickglucose')]
    calibration_Date<-na.omit(as.Date(calibration$timestampfp))
    calibration_consecutive <- c(NA,diff(calibration_Date)>1)

    # rownumbersensor<- which(tablesensor$consecutive==TRUE)[1]
    # rownumbersensor2<- which(tablesensor$consecutive==TRUE)[2]
    #
    # rownumbercal<- which(calibration$consecutive==TRUE)[1]
    # rownumbercal2<- which(calibration$consecutive==TRUE)[2]

    rownumbersensor<- which(tablesensor_consecutive==TRUE)

    #splitting up the files
    if(length(rownumbersensor)==0){
      tableoutsensor<-tablesensor
      tableoutcal<-calibration[calibration_Date %in% c(unique(as.Date(tableoutsensor$timestamp))),]
      tableout<-rowr::cbind.fill(tableoutcal,tableoutsensor,fill = NA)
      filename <-base::paste(outputdirectory_2,"/",tools::file_path_sans_ext(basename(files[f])),".csv",sep = "")
      utils::write.csv(tableout,file = filename,row.names = FALSE)

      }else if(length(rownumbersensor)!=0){
      tableoutsensor<-tablesensor[rownumbersensor[length(rownumbersensor)]:nrow(tablesensor), ]
      tableoutcal<-calibration[calibration_Date %in% c(unique(as.Date(tableoutsensor$timestamp))),]
      tableout<-rowr::cbind.fill(tableoutcal,tableoutsensor,fill = NA)
      filename <-base::paste(outputdirectory_2,"/",tools::file_path_sans_ext(basename(files[f])),".csv",sep = "")
      utils::write.csv(tableout,file = filename,row.names = FALSE)

      tableoutsensor1<-tablesensor[1:(rownumbersensor[1]-1), ]
      tableoutcal1<-calibration[calibration_Date %in% c(unique(as.Date(tableoutsensor1$timestamp))),]
      tableout1<-rowr::cbind.fill(tableoutcal1,tableoutsensor1,fill = NA)
      filename1 <-base::paste(outputdirectory_2,"/",tools::file_path_sans_ext(basename(files[f])),"_wrongstart_1.csv",sep = "")
      utils::write.csv(tableout1,file = filename1,row.names = FALSE)

      if(base::length(rownumbersensor)>1){
        for (i in 2:base::length(rownumbersensor)){
          filename<-base::paste(outputdirectory_2,"/",tools::file_path_sans_ext(basename(files[f])),"_wrongstart_",i,".csv",sep = "")
          date<-tablesensor[rownumbersensor[i-1]:(rownumbersensor[i]-1), ]
          tableout<-assign(paste0("tableout",i),rowr::cbind.fill(assign(paste0("calibration",i),calibration[calibration_Date %in% c(unique(date$Date)),]),
                                                                 assign(paste0("tableoutsensor",i),tablesensor[rownumbersensor[i-1]:(rownumbersensor[i]-1), ]),
                                                                 fill = NA))
          utils::write.csv(tableout,file = filename,row.names = FALSE)
        }
      }
      }
  }



