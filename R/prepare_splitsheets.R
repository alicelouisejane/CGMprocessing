## code to prepare files goes here

#splits sheets from an excel file that have combined baseline 6 months and 12 months
files <- paste("data-raw/10007.xlsx",base::list.files(path = "data-raw/10007.xlsx" ),sep="")
 for (f in 1:base::length(files)) {
    Id<-tools::file_path_sans_ext(basename(files[f]))
    sheets <- readxl::excel_sheets(files[f])
    filenames <- file.path("data-raw/data-split/", paste0(Id,"_",sheets, ".csv"))
    dats <- lapply(sheets, readxl::read_excel, path = files[f], col_types=c("text", "text", "guess", "text", "guess", "text", "guess"))

    keep_vars <- list("ID","Date.and.time.event...5","Finger.prick..Glucose","Date.and.time.event...7","CGMs.glucose.reading")
    names(keep_vars) <- keep_var

    kept_tables <- kept_tables %>%
      map(~ mutate_at(.x, .vars = vars(weight, height), .funs = funs(parse_number)))
    dats <- map(dats, ~ select(.x, one_of(names(keep_vars))))

    lapply(seq_along(dats), function(i) readr::write_csv(dats[[i]], filenames[i]))
    invisible()
  }

files <- base::list.files(path = "data-raw/data-split/",full.names = TRUE)
  base::dir.create("data/",showWarnings = FALSE)
  dateparseorder <- c("mdy HM","mdy HMS","mdY HM","mdY HMS","dmy HM","dmy HMS",
                      "dmY HM","dmY HMS","Ymd HM","Ymd HMS","ymd HM","ymd HMS",
                      "Ydm HM","Ydm HMS","ydm HM","ydm HMS")

  # Read in data
  for (f in 1:base::length(files)) {
    Id<-tools::file_path_sans_ext(basename(files[f]))
    table <- read.csv(files[f],header =TRUE)
    table$ID<-table$ID[1]
    table<-table[,c("ID","Date.and.time.event...5","Finger.prick..Glucose","Date.and.time.event...7","CGMs.glucose.reading")]
    base::colnames(table) <- c('id','timestampfp','fingerprickglucose','timestamp','sensorglucose')

    length<-sum(!is.na(table$timestampfp))
    #moves columns up
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
    tablesensor<-table[,c('id','timestamp','sensorglucose')]
    tablesensor$Date<-as.Date(tablesensor$timestamp)
    tablesensor$consecutive <- c(NA,diff(tablesensor$Date)>1)

    calibration<-table[,c('id','timestampfp','fingerprickglucose')]
    calibration$Date<-as.Date(calibration$timestampfp)
    calibration$consecutive <- c(NA,diff(calibration$Date)>1)

    # rownumbersensor<- which(tablesensor$consecutive==TRUE)[1]
    # rownumbersensor2<- which(tablesensor$consecutive==TRUE)[2]
    #
    # rownumbercal<- which(calibration$consecutive==TRUE)[1]
    # rownumbercal2<- which(calibration$consecutive==TRUE)[2]

    rownumbersensor<- which(tablesensor$consecutive==TRUE)
    #splitting up the files
    if(length(rownumbersensor)==0){
      tableout<-table
      filename <-base::paste(outputdirectory,"/",tools::file_path_sans_ext(basename(files[f])),".csv",sep = "")
      utils::write.csv(tableout,file = filename,row.names = FALSE)

    }else if(length(rownumbersensor)!=0){
      tableoutsensor<-tablesensor[rownumbersensor[length(rownumbersensor)]:nrow(tablesensor), ]
      tableoutcal<-calibration[calibration$Date %in% c(unique(tableoutsensor$Date)),]
      tableout<-rowr::cbind.fill(tableoutcal,tableoutsensor,fill = NA)
      filename <-base::paste(outputdirectory,"/",tools::file_path_sans_ext(basename(files[f])),".csv",sep = "")
      utils::write.csv(tableout,file = filename,row.names = FALSE)

      tableoutsensor1<-tablesensor[1:(rownumbersensor[1]-1), ]
      tableoutcal1<-calibration[calibration$Date %in% c(unique(tableoutsensor1$Date)),]
      tableout1<-rowr::cbind.fill(tableoutcal1,tableoutsensor1,fill = NA)
      filename1 <-base::paste(outputdirectory,"/",tools::file_path_sans_ext(basename(files[f])),"_wrongstart_1.csv",sep = "")
      utils::write.csv(tableout1,file = filename1,row.names = FALSE)

      if(base::length(rownumbersensor)>1){
        for (i in 2:base::length(rownumbersensor)){
          filename<-base::paste(outputdirectory,"/",tools::file_path_sans_ext(basename(files[f])),"_wrongstart_",i,".csv",sep = "")
          date<-tablesensor[rownumbersensor[i-1]:(rownumbersensor[i]-1), ]
          tableout<-assign(paste0("tableout",i),rowr::cbind.fill(assign(paste0("calibration",i),calibration[calibration$Date %in% c(unique(date$Date)),]),
                                                                 assign(paste0("tableoutsensor",i),tablesensor[rownumbersensor[i-1]:(rownumbersensor[i]-1), ]),
                                                                 fill = NA))
          utils::write.csv(tableout,file = filename,row.names = FALSE)
        }
      }

    }

  }



