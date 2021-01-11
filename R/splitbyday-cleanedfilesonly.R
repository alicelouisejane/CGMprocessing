#split extod education files by day for analyseCGM per day (commmented out parts)
#now function works to window 12 hours post exercise of data

inputdirectory<-"data/"
files <- base::list.files(path = inputdirectory, full.names = TRUE)
split_df2<-list()
split_df4<-list()
split_df6<-list()
split_df8<-list()
split_df10<-list()
split_df12<-list()

exerciseinstance<-rio::import("exercise/splitcgmexerciselastinstance.xlsx")
for (f in 1:base::length(files)) {

  table <- utils::read.csv(files[f], stringsAsFactors = FALSE,
                           na.strings = c("NA", ""))
  id<-unique(table$id)
  timepoint<-ifelse(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][2]=="baseline",0,6)

  splitlist<- exerciseinstance %>% filter(subject_id==id & baselineorsix==timepoint)
  if(nrow(splitlist)==0){
    next
  }

  splitlist$hours2after<-splitlist$startdatetime + 2*60*60
  splitlist$hours4after<-(splitlist$startdatetime + 2*60*60) + 2*60*60
  splitlist$hours6after<-(splitlist$startdatetime + 2*60*60 + 2*60*60) + 2*60*60
  splitlist$hours8after<-(splitlist$startdatetime + 2*60*60 + 2*60*60 + 2*60*60) + 2*60*60
  splitlist$hours10after<-(splitlist$startdatetime + 2*60*60+ 2*60*60 + 2*60*60 + 2*60*60) + + 2*60*60
  splitlist$hours12after<-(splitlist$startdatetime + 2*60*60+ 2*60*60 + 2*60*60 + 2*60*60 + 2*60*60 ) + 2*60*60


  #split_df <- split(table,table$Date)
  for(h in 1:length(splitlist$startdatetime)){
  split_df2[[h]]<- table %>% filter( timestamp >= splitlist$startdatetime[[h]] & timestamp <= splitlist$hours2after[[h]])
  split_df4[[h]]<- table %>% filter( timestamp >= splitlist$hours2after[[h]] & timestamp <= splitlist$hours4after[[h]])
  split_df6[[h]]<- table %>% filter( timestamp >= splitlist$hours4after[[h]] & timestamp <= splitlist$hours6after[[h]])
  split_df8[[h]]<- table %>% filter( timestamp >= splitlist$hours6after[[h]] & timestamp <= splitlist$hours8after[[h]])
  split_df10[[h]]<- table %>% filter( timestamp >= splitlist$hours8after[[h]] & timestamp <= splitlist$hours10after[[h]])
  split_df12[[h]]<- table %>% filter( timestamp >= splitlist$hours10after[[h]] & timestamp <= splitlist$hours12after[[h]])
  }




  # Write out separate CSV for each date
  #for (Date in names(split_df)) {
    write.csv(split_df[[h]], paste0("data-bydaylastinstance//",tools::file_path_sans_ext(basename(files[f])),"_",as.Date(splitlist$startdatetime[[h]]), ".csv"))
  #}
  }
}

