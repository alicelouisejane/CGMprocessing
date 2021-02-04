#
#Script works to window 12 hours post exercise of data

#generate individualised plots for monitoring glucose post exercise (12 hours after mainly but will also have 24 hour interval on too)
#usful to make this into a finction / r markdown file

source("/Users/aljc201/Documents/PhD/themes.R")

#load in dataframes
inputdirectory<-"data/"
files <- base::list.files(path = inputdirectory, full.names = TRUE) #list of data files that have been ran through CGM clean (and prepare sheets if neccessary)

exerciseinstance<-rio::import("exercise/splitcgmexerciselastinstance.xlsx") #from Exercise linking script, the datetimes of exercise we want to split the CGM on

exercise<-rio::import("exercise/all_exercise_corrected.xlsx") #exercise types etc - cleaned version of exercise diary

patient_data <- read.csv("CGMupload/CGM.data.patient.12hoursafterexercise.csv")


#handeling dates
dateparseorder <- c(
  "mdy HM", "mdy HMS", "mdY HM", "mdY HMS", "dmy HM", "dmy HMS",
  "dmY HM", "dmY HMS", "Ymd HM", "Ymd HMS", "ymd HM", "ymd HMS",
  "Ydm HM", "Ydm HMS", "ydm HM", "ydm HMS"
)

#empty lists for filling
filter_fin <- list() #filter of glucose from during exercise through until XX hours post exercise
all_cgm<-list() #all cgm data
all_exercise<-list() # all exercise time points

#define what we want to look at, either
#after first instance of exercise, after last insatnce of exercise of  after longest instance
#after last instace
exercise_point<-"last"

for (f in 1:base::length(files)) {

  table <- utils::read.csv(files[f], stringsAsFactors = FALSE,
                           na.strings = c("NA", ""))

  table$timestamp <- base::as.POSIXct(lubridate::parse_date_time(table$timestamp, dateparseorder), tz = "UTC")

  id<-unique(table$id)
  crossection<-ifelse(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][2]=="baseline",0,
                    ifelse(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][2]=="6months",6,
                           ifelse(base::strsplit(tools::file_path_sans_ext(basename(files[f])), "_")[[1]][2]=="12months",12,NA)))

  table$timepoint<-crossection
  splitlist<- exerciseinstance %>% filter(subject_id==id & timepoint==crossection) %>% rename("id"= subject_id)

  if(exercise_point=="last"){
    splitlist<-splitlist %>% filter(!is.na(startdatetime_last)) %>% dplyr::select(c("id",contains("last"),"count","averageborg","averagedurationmins"))
    }else if (exercise_point=="first") {
    splitlist<-splitlist %>% filter(!is.na(startdatetime_first)) %>% dplyr::select(c("id",contains("first"),"count","averageborg","averagedurationmins"))
    }else if (exercise_point=="longest") {
    splitlist<-splitlist %>% filter(!is.na(startdatetime_longest)) %>% dplyr::select(c("id",contains("longest"),"count","averageborg","averagedurationmins"))
    } else {
    var=NA
    }

  if(nrow(splitlist)==0){
    warnings(paste(id,"no exercise times (likely no exercise days recorded)"))
    next
  }


#from exercise diary finding if we have the timestmp in the cgm data and match up to closed time stamp
  exercisetimetamp <- splitlist %>%
    dplyr::group_by(id, finishdatetime_last) %>%
    dplyr::inner_join(table, by = "id") %>%
    dplyr::mutate(diff = abs(base::as.POSIXct(lubridate::parse_date_time(timestamp,dateparseorder), tz="UCT") -base::as.POSIXct(lubridate::parse_date_time(finishdatetime_last,dateparseorder), tz = "UTC"))) %>%
    dplyr::arrange(diff) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>% filter(as.Date(timestamp)==as.Date(finishdatetime_last) & finishdatetime_last<=max(table$timestamp)) %>%
    dplyr::select(c("id", "timestamp","type_of_exercise_last", "startdatetime_last","finishdatetime_last", "borg_last","count","averagedurationmins","averageborg"))

  if(nrow(exercisetimetamp)==0){
    warnings(paste(id,"no matching exercise time with cgm time"))
    next
  }

#filter until time from end of exercise to next time of exercise
    filter_ls<-list()
    for(h in 1:length(exercisetimetamp$timestamp)){
     if(h<length(exercisetimetamp$timestamp)){
       filter_ls[[h]] <- table %>% filter(timestamp >= exercisetimetamp$startdatetime_last[[h]] & timestamp <exercisetimetamp$startdatetime_last[[h+1]]) %>%
        mutate(start=exercisetimetamp$startdatetime_last[[h]], fin=exercisetimetamp$finishdatetime_last[[h]]) %>% mutate(diff=difftime(timestamp,exercisetimetamp$finishdatetime_last[[h]], units = "hours")) %>%
        dplyr::select(c("id", "timestamp", "sensorglucose","diff", "start", "fin","timepoint")) %>% mutate(diff_disc=findInterval(as.numeric(diff), seq(0, max(as.numeric(diff)), 1))) %>%
         mutate(diff_disc=ifelse(timestamp<exercisetimetamp$finishdatetime_last[[h]],0,diff_disc))
      }else if(h==length(exercisetimetamp$timestamp)){
        filter_ls[[h]] <- table %>% filter(timestamp >= exercisetimetamp$startdatetime_last[[h]] & timestamp < max(timestamp)) %>%
          mutate(start=exercisetimetamp$startdatetime_last[[h]], fin=exercisetimetamp$finishdatetime_last[[h]]) %>% mutate(diff=difftime(timestamp,exercisetimetamp$finishdatetime_last[[h]], units = "hours")) %>%
          dplyr::select(c("id", "timestamp", "sensorglucose","diff", "start","fin", "timepoint")) %>% mutate(diff_disc=findInterval(as.numeric(diff), seq(0, max(as.numeric(diff)), 1))) %>%
          mutate(diff_disc=ifelse(timestamp<exercisetimetamp$finishdatetime_last[[h]],0,diff_disc))
      }
    }

    #all exercise
    exercisetimetamp_all <- exercise %>% rename("id"=subject_id) %>%
      dplyr::group_by(id, finishdatetime) %>%
      dplyr::inner_join(table, by = "id") %>%
      dplyr::mutate(diff = abs(base::as.POSIXct(lubridate::parse_date_time(timestamp,dateparseorder), tz="UCT") -base::as.POSIXct(lubridate::parse_date_time(finishdatetime,dateparseorder), tz = "UTC"))) %>%
      dplyr::arrange(diff) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>% filter(as.Date(timestamp)==as.Date(finishdatetime) & finishdatetime<=max(table$timestamp)) %>%
      dplyr::select(c("id", "timepoint","timestamp","type of exercise", "startdatetime","finishdatetime", "borg", "duration_alice")) %>%
      merge(exercisetimetamp, by=c("timestamp","id"), all=T)



#time post exercise
   filter_fin[[f]]<-bind_rows(filter_ls)
#all sensor readings
   all_cgm[[f]]<-table
#all exercise data that align with cgm
   all_exercise[[f]]<-exercisetimetamp_all

}


#combining all lists
#all CGM trace to plot the trace (in gg1)
all_cgm_fin <- all_cgm[!sapply(all_cgm, is.null)]
all_cgm_output<-bind_rows(all_cgm_fin) %>% group_by(id,timepoint) %>% mutate(dayofwear= match(as.Date(timestamp),unique(as.Date(timestamp))))


#exercise time points for addition of green and red lines on plot
all_exercise_fin <- all_exercise[!sapply(all_exercise, is.null)]
all_exercise_output<-bind_rows(all_exercise)

#day of wear. ie. what day of wear was the exercise date on
date_dayofwear<- all_cgm_output %>% select(id,timepoint, Date,dayofwear) %>% rename(exercise_date="Date") %>% unique(.)

#Arm in RCT
patient_data_arms <-openxlsx::read.xlsx("Patientdata_arms.xlsx") %>% rename_all(tolower) %>% select(id,arm) %>%
  mutate(arm = ifelse(grepl("control",arm, ignore.case = T),0,
                      ifelse(grepl("treatment",arm, ignore.case = T),1, arm)))


  # dataframe calculating hte percentage change from the first (at the start ie. after exercise to 1 hour after exercise)
output_fin <- filter_fin[!sapply(filter_fin, is.null)]
#separate files to run the CGM analysis metircs over
new_output_fin<-list() #cret lst to store dfs
for (i in 1:length(output_fin)){

  new.names[i]<-paste0(output_fin[[i]]$id[1],"_",output_fin[[i]]$timepoint[1], collapse = ",") #file name Id_timepoint
  new_output_fin[[i]]<- output_fin[[i]] %>% filter(diff_disc<=12) #only include cgm traces from 12 hours after last exercise (all days )
  write.csv(new_output_fin[[i]], file=paste0("data-12hrs after/",  new.names[i], ".csv")) #write file
}

output_1 <-bind_rows(output_fin) %>% mutate(date=as.Date(timestamp), exercise_date=as.Date(fin)) %>%
  arrange(id, timepoint, exercise_date) %>% group_by(id, timepoint, exercise_date,diff_disc) %>%
  slice(c(1, n())) %>% group_by(id, timepoint, exercise_date,diff_disc) %>%
  mutate(gluc_difference = (sensorglucose-lag(sensorglucose))) %>%
  mutate(rate=(gluc_difference/lag(sensorglucose))*100) %>% merge(date_dayofwear) %>% merge(patient_data_arms)


# dataframe calculating the CV per hour (at the start ie. after exercise to 1 hour after exercise)
output_2 <-bind_rows(output_fin) %>%  mutate(date=as.Date(timestamp), exercise_date=as.Date(fin)) %>%
  group_by(id,timepoint,diff_disc, exercise_date) %>%
  mutate(cv_byhour=base::round((stats::sd(sensorglucose[base::which(!is.na(sensorglucose))])) / base::mean(sensorglucose[base::which(!is.na(sensorglucose))]), digits = 2)) %>%
  merge(date_dayofwear)  %>% merge(patient_data_arms)


###################### PLOTTING FOR INDIVIDUALS ########################################

# Isolating one (or a few) ID for ease of data vis - could facet wrap a couple of IDs also could facet by timepoint
id_of_interest <- sample(unique(output_1$id), 1)

#exercise df for these ids of interest
grouping_ex<- all_exercise_output %>% subset(id %in% id_of_interest) %>% filter(duration_alice>15)%>% filter(!is.na(finishdatetime_last)) %>% mutate(exercise_date=as.Date(finishdatetime)) %>%
  merge(date_dayofwear, by=c("id","timepoint","exercise_date"))

grouping<- output_2 %>% subset(id %in% id_of_interest) %>% arrange(id, timepoint, exercise_date)
# to enable line drawing between points, there is a gap if the gap between points is > 5 mins
idx=c(5, round(diff(grouping$timestamp))) #difference is 5 mins round up this is to make graphs of the
i2 =c(1,which(idx != 5), nrow(grouping)+1)
grouping$grp = rep(1:length(diff(i2)), diff(i2))

#format hm is for scale of x axis
format_hm <- function(sec) stringr::str_sub(format(sec), end = -4L)

#plot the percentage change per hour
# filter for lesss than 12 hours after exercise ie somtimes weve have big gap in the data if day is removed. can look at 24th hour after too
# Rate of change for individual
gg_sub1<-output_1 %>% group_by(id) %>%  filter (id %in% id_of_interest) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% filter(!is.na(rate)) %>% ungroup () %>%
  ggplot(aes(as.factor(diff_disc),rate)) +
  geom_violin() +  geom_jitter(aes(colour=as.factor(dayofwear)),height = 0, width = 0.1, size=5) +
  stat_summary(fun = "median", colour = "red", size = 0.5, geom = "crossbar") +
  #geom_boxplot() +
  facet_wrap(~timepoint, labeller = label_both) + pub_theme4 +
  xlab("Hours post exercise") + ylab("Rate of change \n per hour post exercise") +
  facet_grid(cols = vars(timepoint), rows = vars(id), labeller = label_both, scales="free_y") +
  scale_color_discrete(name="Day of CGM wear") + geom_hline(aes(yintercept = 0), linetype="dashed")

#CV for individual
gg_sub2<-output_2 %>% group_by(id) %>%  filter (id %in% id_of_interest) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% group_by(exercise_date) %>% distinct(diff_disc, .keep_all = T) %>% ungroup () %>%
  ggplot(aes(as.factor(diff_disc),cv_byhour)) +
  geom_violin() +  geom_jitter(aes(colour=as.factor(dayofwear)),height = 0, width = 0.1, size=5) +
  stat_summary(fun = "median", colour = "red", size = 0.5, geom = "crossbar") +
  #geom_boxplot() +
  facet_wrap(~timepoint, labeller = label_both) + pub_theme4 +
  xlab("Hours post exercise") + ylab("Coeffient of Variation \n per hour post exercise") +
  facet_grid(cols = vars(timepoint), rows = vars(id), labeller = label_both, scales="free_y") +
  scale_color_discrete(name="Day of CGM wear")


#average glucose for those time windows
gg_sub3<-output_2 %>% group_by(id) %>%  filter (id %in% id_of_interest) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>%
  group_by(diff_disc, timepoint, id) %>%
  summarise(mean_gluc = mean(sensorglucose),  # calculates the mean of each group
            sd_gluc = sd(sensorglucose), # calculates the standard deviation of each group
            n_gluc = n(),  # calculates the sample size per group
            SE_gluc = sd(sensorglucose)/sqrt(n())) %>%
  ggplot(aes(as.factor(diff_disc), mean_gluc)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_gluc - SE_gluc, ymax = mean_gluc + SE_gluc), width=0.2) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Mean Glucose (mmol/L) ± SE ") +
  facet_grid(cols = vars(timepoint), rows=vars(id), labeller = label_both, scales="free_y")


#plots sensor glucose for the time window post exercise for individual

grouping<- grouping %>% filter(diff_disc<=12)

gg1<- ggplot(grouping,aes(hms::as_hms(diff),sensorglucose)) + geom_rect(aes(xmin=-Inf, xmax=Inf, ymin=4, ymax=7), alpha=0.02, fill="#e0f3db") +
  geom_line(aes(group=interaction(dayofwear,grp) , color=as.factor(dayofwear)),  size=1.5) + # geom_point() +
   facet_grid(cols= vars(timepoint),rows = vars(id), labeller = label_both, scales="free") + scale_x_time(name = "Time from exercise (hh:mm)",labels = format_hm) + ylab("Sensor Glucose (mmol/L)") +
  geom_vline(data=distinct(grouping,exercise_date, .keep_all = T), aes(xintercept = hms::as_hms(diff)), linetype = "dashed", colour="green") +
  geom_vline(aes(xintercept = 0), linetype = "dashed", colour="red") + scale_color_discrete(name="Day of CGM wear") + pub_theme4

# sapply(hms::as_hms(difftime(grouping_ex$startdatetime,grouping_ex$timestamp,unit="hours")), function(xint) geom_vline(aes(xintercept = xint), linetype = "dashed", colour="green")) +
#   sapply(hms::as_hms(difftime(grouping_ex$finishdatetime,grouping_ex$finishdatetime,unit="hours")), function(xint) geom_vline(aes(xintercept = xint), linetype = "dashed", colour="red")) +
#scale_x_datetime(date_breaks = "12 hour", date_minor_breaks = "1 hours", date_labels = "%y-%m-%d %H:%M") +

#table of exercise per individual
table.ex <- grouping_ex %>% filter(!is.na(finishdatetime_last)) %>% select(dayofwear, timepoint,type_of_exercise_last,startdatetime_last,finishdatetime_last,borg_last,duration_alice,count,averagedurationmins,averageborg) %>%
  rename("Day of CGM wear"=dayofwear,"Type of exercise"=type_of_exercise_last,"Start time of last \n exercise instance"=startdatetime_last,"Finish time of last \n exercise instance"=finishdatetime_last,"BORG of last \n exercise instance"=borg_last,"Duration of last \n exercise instance"=duration_alice,"Number of exercise \n episodes on day"=count,"Average duration of \n all exercise on day"=averagedurationmins,"Average BORG of \n all exercise on day" =averageborg) %>%
  ggpubr::ggtexttable(rows = NULL,
                        theme = ggpubr::ttheme("minimal"))

#summary metric for specific patient
table.patient<- read.csv("CGMupload/CGM.data.patient.12hoursafterexercise.csv") %>% rename("id"=subject_id) %>%
  filter(id %in% ids_in_analysis) %>% mutate(bmi=ifelse(grepl("0|baseline",timepoint),weight_vst1/((height_vst1)/100)^2,
                                                        ifelse(grepl("6|6months",timepoint),weight_vst7/((height_vst7)/100)^2,NA)),
                                             ageatonset=age-durationofdiabetes,
                                             hba1c=ifelse(grepl("0|baseline",timepoint),hba1c_result_vst1,
                                                          ifelse(grepl("6|6months",timepoint),hba1c_result_vst7,NA)),
                                             weight=ifelse(grepl("0|baseline",timepoint),weight_vst1,
                                                           ifelse(grepl("6|6months",timepoint),weight_vst7,NA))) %>%
  select(id,timepoint,age,sex, ageatonset,durationofdiabetes,bmi, weight,hba1c,
         cpeptidepmolcoded) %>%
  merge(patient_data_arms, by="id") %>% filter (id %in% id_of_interest & timepoint==0) %>%
  mutate(across(c(ageatonset,durationofdiabetes, bmi),round)) %>%
  rename("ID"=id,"RCT timepoint" =timepoint,"Age (yrs) "=age,"Sex"=sex,"Age at onset (yrs)"= ageatonset,"Diabetes duration (yrs)"=durationofdiabetes,
         "BMI"= bmi,"Weight (kg)" =weight,"HbA1c"=hba1c,"C-peptide (pmol/L)"=cpeptidepmolcoded) %>%
  ggpubr::ggtexttable(rows = NULL,
                      theme = ggpubr::ttheme("minimal"))

arrange<-ggpubr::ggarrange(table.patient,
  gg1, gg_sub1,gg_sub3,gg_sub2,table.ex,
  common.legend = TRUE, legend = "top", nrow=6, ncol=1,
  widths= c(1,1,1,1,1,1), heights = c(0.3,1,1,1,1,1))

ggsave("2045_analysed_withmeanglu.png", arrange, width = 20, height = 22)

##########################################################################################################################################
#Plotting for all combined below...
##########################################################################################################################################
#plot the percentage change per hour and rate for all
#mean glucose to orientate against rates and CV add this as a small pannel under each
#allnot withhin arms but can test for other stratification
#make stratification groups

ids_in_analysis<-unique(output_1$id) # incluse everyone in output 1 data frame not got rid of not randomised the next df does that
patient_data <- read.csv("CGMupload/CGM.data.patient.12hoursafterexercise.csv") %>% rename("id"=subject_id) %>%
  filter(id %in% ids_in_analysis) %>% mutate(bmi=ifelse(grepl("0|baseline",timepoint),weight_vst1/((height_vst1)/100)^2,
                                                        ifelse(grepl("6|6months",timepoint),weight_vst7/((height_vst7)/100)^2,NA)),
                                             ageatonset=age-durationofdiabetes,
                                             hba1c=ifelse(grepl("0|baseline",timepoint),hba1c_result_vst1,
                                                          ifelse(grepl("6|6months",timepoint),hba1c_result_vst7,NA)),
                                             weight=ifelse(grepl("0|baseline",timepoint),weight_vst1,
                                                           ifelse(grepl("6|6months",timepoint),weight_vst7,NA))) %>%
  select(id,timepoint,sex, ageatonset,durationofdiabetes,bmi, weight,hba1c,
         cpeptidepmolcoded, num_hrs_good_data, standard_deviation,average_sensor ,cv,percent_time_3.9_10, percent_time_under_hypo3,
         percent_time_over10,lbgi,hbgi) %>% merge(patient_data_arms, by="id")



#stratification investigation can only be done at timepoint 0 (before trial)
ex.forgroup<-all_exercise_output %>% filter(!is.na(finishdatetime_last)) %>% select(c(id,timepoint,startdatetime_last, finishdatetime_last,
                                                                                      borg_last,type_of_exercise_last, duration_alice,count,
                                                                                      averagedurationmins,averageborg)) #filter exercise

output2_strat<-output_2 %>% group_by(id) %>%
  filter(diff_disc<=12 | diff_disc==24) %>%
  group_by(id,timepoint,exercise_date) %>%
  distinct(diff_disc, .keep_all = T) %>%
  ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(timepoint==0) %>%
  merge(patient_data,by=c("id","timepoint","arm")) %>% #anyone not in arm is lost here ie. 2018
  rename("startdatetime_last"=start, "finishdatetime_last"= fin) %>%
  merge(ex.forgroup, by= c("id","timepoint","startdatetime_last", "finishdatetime_last")) %>% #merge with exercise that were looking at in these windows
  mutate(across(contains("borg"),as.numeric)) %>%
  mutate(cpeptide_grp= as.factor(cut(cpeptidepmolcoded, breaks = c(0,2.9,100,max(cpeptidepmolcoded)), include.lowest = F, labels = c("≤2.9","3-100","101-930"))),
         borg_grp= as.factor(cut(borg_last, breaks = c(0,12,max(borg_last, na.rm = T)), include.lowest = F, labels = c("≤12",">12"))))



output1_strat<-output_1 %>% group_by(id) %>%
  group_by(exercise_date) %>%
  filter(diff_disc<=12 | diff_disc==24) %>%
  filter(!is.na(rate)) %>%
  ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(timepoint==0) %>%
  merge(patient_data,by=c("id","timepoint","arm")) %>%
  rename("startdatetime_last"=start, "finishdatetime_last"= fin) %>%
  merge(ex.forgroup, by= c("id","timepoint","startdatetime_last", "finishdatetime_last")) %>%
  mutate(across(contains("borg"),as.numeric)) %>%
  mutate(cpeptide_grp= as.factor(cut(cpeptidepmolcoded, breaks = c(0,2.9,100,max(cpeptidepmolcoded)), include.lowest = F,labels = c("≤2.9","3-100","101-930"))),
         borg_grp= as.factor(cut(borg_last, breaks = c(0,12,max(borg_last, na.rm = T)), include.lowest = F,labels = c("≤12",">12"))))



#strat by cpeptide
#mean glucose
meanglu_cpep_strat<- output2_strat %>%
  group_by(diff_disc, cpeptide_grp) %>%
  summarise(mean_gluc = mean(sensorglucose),  # calculates the mean of each group
            sd_gluc = sd(sensorglucose), # calculates the standard deviation of each group
            n_gluc = n(),  # calculates the sample size per group ie. counting each single glucose reading within that group
            SE_gluc = sd(sensorglucose)/sqrt(n())) %>% ungroup() %>%
  mutate(across(diff_disc, as.factor))  %>%
  ggplot(aes(as.factor(diff_disc), mean_gluc, colour=cpeptide_grp, group=cpeptide_grp)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = mean_gluc - SE_gluc, ymax = mean_gluc + SE_gluc), width=0.2, position=position_dodge(width=0.9)) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Mean Glucose (mmol/L) ± SE ") +
  theme(strip.text = element_text(size=14, face="bold")) +
   geom_text(aes(label=n_gluc ,y=11.5, colour=cpeptide_grp),position=position_dodge(width=0.9), size=7) +
  scale_colour_discrete(name = "C-peptide group")

#rate of change
summ_cpep_strat <- output1_strat %>%
  mutate(across(diff_disc, as.factor)) %>%
  group_by(diff_disc, cpeptide_grp) %>%
  summarise(n = n(), rate=median(rate)) # n number is how many rates we have per that hourly interval (1 rate per person per interval)


rate_cpep_strat<-output1_strat %>%
  ggplot(aes(as.factor(diff_disc),rate, colour=cpeptide_grp, fill=cpeptide_grp)) +
  geom_violin(position=position_dodge(width=0.9)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar", position=position_dodge(width=0.9)) +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Rate of change per hour post exercise\n") +
  geom_hline(yintercept = 0, linetype="dashed", colour="blue", size=0.75) +
  theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n ,y=360), data=summ_cpep_strat, size=7,position=position_dodge(width=0.9), show.legend=F) +
  scale_colour_discrete(name = "C-peptide group") + scale_fill_discrete(guide = FALSE)



sum.cv_cpep_strat<-
  output2_strat %>%
  mutate(across(diff_disc, as.factor)) %>%
  group_by(diff_disc, cpeptide_grp) %>%
  summarise(n = n(), cv_byhour=median(cv_byhour, na.rm = T))

cv_cpep_strat<-output2_strat %>%
  ggplot(aes(as.factor(diff_disc),cv_byhour, colour=cpeptide_grp, fill=cpeptide_grp)) +
  geom_violin(position=position_dodge(width=0.9)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar", position=position_dodge(width=0.9)) +
  #geom_boxplot() +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  scale_y_continuous(labels = scales::percent) +
  xlab("Hours post exercise") + ylab("CV per hour post exercise (%) \n") +
  theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n, y=0.8), data=sum.cv_cpep_strat,size=7,position = position_dodge(width=0.9), show.legend=F) +
  scale_colour_discrete(name = "C-peptide group") + scale_fill_discrete(guide = FALSE)

arrange_cpeptidestrat<-ggpubr::ggarrange(rate_cpep_strat,meangluc_cpep_strat, cv_cpep_strat,
                                nrow=3, ncol=1,
                                widths= c(1,1,1), heights = c(1,0.65,1), common.legend = TRUE, legend = "top")

ggsave("rate.cv_stratby_cpep.png", arrange_cpeptidestrat, width = 20, height = 15)




#strat by BORG
#mean glucose
meangluc_borg_strat<- output2_strat %>%
  filter(!is.na(borg_grp)) %>%
  group_by(diff_disc, borg_grp) %>%
  summarise(mean_gluc = mean(sensorglucose),  # calculates the mean of each group
            sd_gluc = sd(sensorglucose), # calculates the standard deviation of each group
            n_gluc = n(),  # calculates the sample size per group ie. counting each single glucose reading within that group
            SE_gluc = sd(sensorglucose)/sqrt(n())) %>% ungroup() %>%
  mutate(across(diff_disc, as.factor))  %>%
  ggplot(aes(as.factor(diff_disc), mean_gluc, colour=borg_grp, group=borg_grp)) +
  geom_point(position=position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = mean_gluc - SE_gluc, ymax = mean_gluc + SE_gluc), width=0.2, position=position_dodge(width=0.9)) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Mean Glucose (mmol/L) ± SE ") +
  theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n_gluc ,y=11.5, colour=borg_grp),position=position_dodge(width=0.9), size=7) +
  scale_colour_discrete(name = "BORG group")


#rate of change
summ_borg_strat <- output1_strat %>%
  filter(!is.na(borg_grp)) %>%
  mutate(across(diff_disc, as.factor)) %>%
  group_by(diff_disc, borg_grp) %>%
  summarise(n = n(), rate=median(rate)) # n number is how many rates we have per that hourly interval (1 rate per person per interval)


rate_borg_strat<-output1_strat %>%
  filter(!is.na(borg_grp)) %>%
  ggplot(aes(as.factor(diff_disc),rate, colour=borg_grp, fill=borg_grp)) +
  geom_violin(position=position_dodge(width=0.9)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar", position=position_dodge(width=0.9)) +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Rate of change per hour post exercise\n") +
  geom_hline(yintercept = 0, linetype="dashed", colour="blue", size=0.75) +
  theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n ,y=360), data=summ_borg_strat, size=7,position=position_dodge(width=0.9))+
  scale_colour_discrete(name = "BORG group")  + scale_fill_discrete(guide = FALSE)

sum.cv_borg_strat<-
  output2_strat %>%
  filter(!is.na(borg_grp)) %>%
  mutate(across(diff_disc, as.factor)) %>%
  group_by(diff_disc, borg_grp) %>%
  summarise(n = n(), cv_byhour=median(cv_byhour, na.rm = T))

cv_borg_strat<-output2_strat %>%
  filter(!is.na(borg_grp)) %>%
  ggplot(aes(as.factor(diff_disc),cv_byhour, colour=borg_grp, fill=borg_grp)) +
  geom_violin(position=position_dodge(width=0.9)) +
  geom_point(position=position_jitterdodge(dodge.width=0.9)) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar", position=position_dodge(width=0.9)) +
  #geom_boxplot() +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  scale_y_continuous(labels = scales::percent) +
  xlab("Hours post exercise") + ylab("CV per hour post exercise (%) \n") +
  theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n, y=0.8), data=sum.cv_borg_strat,size=7,position = position_dodge(width=0.9))+
  scale_colour_discrete(name = "BORG group")  + scale_fill_discrete(guide = FALSE)



arrange_borgstrat<-ggpubr::ggarrange(rate_borg_strat,meangluc_borg_strat, cv_borg_strat,
                                         nrow=3, ncol=1,
                                         widths= c(1,1,1), heights = c(1,0.65,1), common.legend = TRUE, legend = "top")

ggsave("rate.cv_stratby_BORG.png", arrange_borgstrat, width = 20, height = 15)








############################################################################################

#mean glucose
#intervention arm
gg2_sub2_arm1<-output_2 %>% group_by(id) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% ungroup() %>%
  filter(arm== 1) %>%
  group_by(diff_disc, timepoint,arm) %>%
  summarise(mean_gluc = mean(sensorglucose),  # calculates the mean of each group
            sd_gluc = sd(sensorglucose), # calculates the standard deviation of each group
            n_gluc = n(),  # calculates the sample size per group ie. counting each single glucose reading within that group
            SE_gluc = sd(sensorglucose)/sqrt(n())) %>% ungroup() %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  ggplot(aes(as.factor(diff_disc), mean_gluc)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_gluc - SE_gluc, ymax = mean_gluc + SE_gluc), width=0.2) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Mean Glucose (mmol/L) ± SE ") +
  facet_grid(cols = vars(timepoint), rows=vars(arm), labeller = label_both, scales="free_y") +
  theme(strip.text = element_text(size=14, face="bold"))

#control arm
gg2_sub2_arm0<-output_2 %>% group_by(id) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% ungroup() %>%
  filter(arm==0) %>%
  group_by(diff_disc, timepoint,arm) %>%
  summarise(mean_gluc = mean(sensorglucose),  # calculates the mean of each group
            sd_gluc = sd(sensorglucose), # calculates the standard deviation of each group
            n_gluc = n(),  # calculates the sample size per group ie. counting each single glucose reading within that group
            SE_gluc = sd(sensorglucose)/sqrt(n())) %>%
  ungroup() %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  ggplot(aes(as.factor(diff_disc), mean_gluc)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_gluc - SE_gluc, ymax = mean_gluc + SE_gluc), width=0.2) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Mean Glucose (mmol/L) ± SE ") +
  facet_grid(cols = vars(timepoint), rows=vars(arm), labeller = label_both, scales="free_y") +
  theme(strip.text = element_text(size=14, face="bold"))

#RATE of change
#intervention arm
# filter for lesss than 12 hours after exercise ie somtimes weve have big gap in the data if day is removed. can look at 24th hour after too
summ_arm1 <- output_1 %>% group_by(id) %>%
  group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>%
  filter(!is.na(rate)) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==1) %>%
  group_by(diff_disc, timepoint) %>%
  summarise(n = n(), rate=median(rate)) # n number is how many rates we have per that hourly interval (1 rate per person per interval)


gg2_arm1<-output_1 %>% group_by(id) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% filter(!is.na(rate)) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>% transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm== 1) %>%
  ggplot(aes(as.factor(diff_disc),rate)) +
  geom_violin() +  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Rate of change per hour post exercise\n") +
  facet_grid(cols = vars(timepoint), rows=vars(arm),labeller = label_both, scales="free_y") +
  geom_hline(yintercept = 0, linetype="dashed", colour="blue", size=0.75) + theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n ,y=360), data=summ_arm1, size=7)

#control arm
# filter for lesss than 12 hours after exercise ie somtimes weve have big gap in the data if day is removed. can look at 24th hour after too
summ_arm0 <- output_1 %>% group_by(id) %>%
  group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>%
  filter(!is.na(rate)) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==0) %>%
  group_by(diff_disc, timepoint) %>%
  summarise(n = n(), rate=median(rate)) # n number is how many rates we have per that hourly interval (1 rate per person per interval)


gg2_arm0<-output_1 %>% group_by(id) %>% group_by(exercise_date) %>% filter(diff_disc<=12 | diff_disc==24) %>% filter(!is.na(rate)) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>% transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==0) %>%
  ggplot(aes(as.factor(diff_disc),rate)) +
  geom_violin() +  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  xlab("Hours post exercise") + ylab("Rate of change per hour post exercise\n") +
  facet_grid(cols = vars(timepoint),rows=vars(arm),labeller = label_both, scales="free_y") +
  geom_hline(yintercept = 0, linetype="dashed", colour="blue", size=0.75) + theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n ,y=360), data=summ_arm0, size=7)



arrange_rate<-ggpubr::ggarrange(gg2_arm0,gg2_sub2_arm0,gg2_arm1,gg2_sub2_arm1,
                                nrow=4, ncol=1,
                           widths= c(1,1,1,1), heights = c(1,0.65,1,0.65))


ggsave("rate_within_arms_meangluc.png", arrange_rate, width = 20, height = 15)


#CV#
#control arm
sum.cv_arm0<-
  output_2 %>% group_by(id) %>%filter(diff_disc<=12 | diff_disc==24) %>%
  group_by(id,timepoint,exercise_date) %>% distinct(diff_disc, .keep_all = T) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==0) %>%
  group_by(diff_disc, timepoint, arm) %>%
  summarise(n = n(), cv_byhour=median(cv_byhour, na.rm = T))

gg3_arm0<-output_2 %>% group_by(id) %>%filter(diff_disc<=12 | diff_disc==24) %>% group_by(id,timepoint,exercise_date) %>% distinct(diff_disc, .keep_all = T) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>% transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==0) %>%
  ggplot(aes(as.factor(diff_disc),cv_byhour)) +
  geom_violin() +  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  scale_y_continuous(labels = scales::percent) +
  xlab("Hours post exercise") + ylab("CV per hour post exercise (%) \n") +
  facet_grid(cols = vars(timepoint), rows=vars(arm),labeller = label_both, scales="free_y") + theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n, y=0.8), data=sum.cv_arm0,size=8)

#intervention arm
sum.cv_arm1<-
  output_2 %>% group_by(id) %>%filter(diff_disc<=12 | diff_disc==24) %>%
  group_by(id,timepoint,exercise_date) %>% distinct(diff_disc, .keep_all = T) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>%
  transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==1) %>%
  group_by(diff_disc, timepoint, arm) %>%
  summarise(n = n(), cv_byhour=median(cv_byhour, na.rm = T))

gg3_arm1<-output_2 %>% group_by(id) %>%filter(diff_disc<=12 | diff_disc==24) %>% group_by(id,timepoint,exercise_date) %>% distinct(diff_disc, .keep_all = T) %>% ungroup () %>%
  mutate(across(diff_disc, as.factor)) %>% transform(diff_disc=plyr::revalue(diff_disc,c("0"="During \n exercise"))) %>%
  filter(arm==1) %>%
  ggplot(aes(as.factor(diff_disc),cv_byhour)) +
  geom_violin() +  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
  #facet_wrap(~timepoint, labeller = label_both) +
  pub_theme4 +
  scale_y_continuous(labels = scales::percent) +
  xlab("Hours post exercise") + ylab("CV per hour post exercise (%) \n") +
  facet_grid(cols = vars(timepoint), rows=vars(arm),labeller = label_both, scales="free_y") + theme(strip.text = element_text(size=14, face="bold")) +
  geom_text(aes(label=n, y=0.8), data=sum.cv_arm1,size=8)


arrange_cv<-ggpubr::ggarrange(gg3_arm0,gg2_sub2_arm0,gg3_arm1,gg2_sub2_arm1,
                                nrow=4, ncol=1,
                                widths= c(1,1,1,1), heights = c(1,0.65,1,0.65))


ggsave("CV_within_arms_meangluc.png", arrange_cv, width = 20, height = 15)


#comparing exercise and intensity
borg_violin<-all_exercise_output %>% merge(patient_data_arms) %>% filter(!is.na(finishdatetime_last)) %>%
  mutate(across(borg_last,as.numeric)) %>%
  filter(arm== 1 | arm==0) %>%
  ggplot(aes(as.factor(timepoint),borg_last)) + geom_violin() +
  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
   pub_theme4 +
  facet_wrap(~arm, labeller = label_both)+
  xlab("Timepoint") + ylab("BORG of last exercise instance")

borg_hist<-all_exercise_output %>% merge(patient_data_arms) %>%
  filter(!is.na(finishdatetime_last)) %>% mutate(across(borg_last,as.numeric)) %>%
  filter(arm== 1 | arm==0) %>%
  ggplot(aes(borg_last)) +
  geom_histogram(aes(y=..density..,fill=as.factor(timepoint)),binwidth = 1, position = "identity", alpha=0.75) +
  pub_theme4 +
  facet_wrap(~arm, labeller = label_both) +
  xlab("BORG of last exercise instance") + ylab("Density")+  labs(fill = "Timepoint")


dur<-all_exercise_output %>%  merge(patient_data_arms) %>%
  filter(!is.na(finishdatetime_last)) %>%
  filter(duration_alice<600) %>%
  filter(arm== 1 | arm==0) %>%
  ggplot(aes(as.factor(timepoint),duration_alice)) + geom_violin() +
  geom_jitter(height = 0, width = 0.1) +
  stat_summary(fun = "median", colour = "red", size = 1, geom = "crossbar") +
  #geom_boxplot() +
  pub_theme4 +
  facet_wrap(~arm, labeller = label_both) +
    xlab("Timepoint") + ylab("Duration of last exercise instance (mins)")


table.ex.all.control <- all_exercise_output %>% merge(patient_data_arms) %>%
  filter(arm==0) %>%
  filter(!is.na(finishdatetime_last)) %>%
  select(timepoint,borg_last,duration_alice,count) %>%
  mutate(across(borg_last,as.numeric)) %>%
  rename("BORG of last exercise instance"=borg_last,"Duration of last exercise instance"=duration_alice,"Number of exercise episodes on day"=count) %>%
 gtsummary::tbl_summary(by = c(timepoint)) %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))

table.ex.all.int <- all_exercise_output %>% merge(patient_data_arms) %>%
  filter(arm==1) %>%
  filter(!is.na(finishdatetime_last)) %>%
  select(timepoint,borg_last,duration_alice,count) %>%
  mutate(across(borg_last,as.numeric)) %>%
  rename("BORG of last exercise instance"=borg_last,"Duration of last exercise instance"=duration_alice,"Number of exercise episodes on day"=count) %>%
  gtsummary::tbl_summary(by = c(timepoint)) %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))


combined_tbl <- gtsummary::tbl_merge(
  tbls = list(table.ex.all.control, table.ex.all.int),
  tab_spanner = c("**Control**", "**Intervention**"))
  gtsummary::as_flex_table(combined_tbl) %>% flextable::save_as_docx(path="graphs/exerciseaverages_lastinstances_arms.docx")

arrange_2<-ggpubr::ggarrange(
  borg_violin, borg_hist,dur, nrow=2, ncol=2)

ggsave("exercisesummary_lastinstance_arms.png", arrange_2, width = 20, height = 15)


#Patient data tables
#comparing the timepoints and arms within that
tbl_patient_data_0<-patient_data %>% filter(timepoint==0) %>% select(-timepoint) %>%
  gtsummary::tbl_summary(by = c(arm)) %>%
  gtsummary::italicize_levels() %>%
  gtsummary::bold_labels() %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))

tbl_patient_data_6<-patient_data %>% filter(timepoint==6) %>% select(-timepoint) %>%
  gtsummary::tbl_summary(by = c(arm)) %>%
  gtsummary::italicize_levels() %>%
  gtsummary::bold_labels() %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))

combined_tbl <- gtsummary::tbl_merge( tbls = list(tbl_patient_data_0, tbl_patient_data_6), tab_spanner = c("**Baseline**", "**6 months**"))
gtsummary::as_flex_table(combined_tbl) %>% flextable::save_as_docx(path="graphs/12hrsafter_cgmmetrics_armsvstimepoint.docx")



#comparing the arms within the timpoints
tbl_patient_data_cont<-patient_data %>% filter(arm==0) %>% select(-arm) %>%
  gtsummary::tbl_summary(by = c(timepoint)) %>%
  gtsummary::italicize_levels() %>%
  gtsummary::bold_labels() %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))

tbl_patient_data_int<-patient_data %>% filter(arm==1) %>% select(-arm) %>%
  gtsummary::tbl_summary(by = c(timepoint)) %>%
  gtsummary::italicize_levels() %>%
  gtsummary::bold_labels() %>%
  gtsummary::add_p(pvalue_fun = ~gtsummary::style_pvalue(.x, digits = 2))


combined_tbl <- gtsummary::tbl_merge( tbls = list(tbl_patient_data_cont, tbl_patient_data_int), tab_spanner = c("**Control**", "**Intervention**"))
gtsummary::as_flex_table(combined_tbl) %>% flextable::save_as_docx(path="graphs/12hrsafter_cgmmetrics_arms_timepointvsarms.docx")


