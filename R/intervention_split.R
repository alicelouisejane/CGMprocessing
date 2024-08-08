#' @title intervention_split
#'
#' @description This function splits up clean CGM data into specfic periods before and after intervention(s) based on intervention description file, to enable
#' This function accepts clean CGM data in the format of the output of cleanCGM(). (See README for details)

#' @returns Returns folders containing individual (or pre aggregated depending on input structure) files for the corresponding split CGM data per individual.
#'
#' @param inputdirectory path to folder containing cleaned CGM files. See README on structure for these files follwoing the output of cleanCGM
#' @param aggregated TRUE/FALSE dictates if data is pre-aggregated or in individual separate files (usual for clinical trials)
#' @param outputdirectory path to folder where new folders will be created
#' @param interventionfile path to file containing list of intervention timestamps for every person in study. See README on structure for this file. Ids must match ids in CGM
#'
#' @importFrom rio import export
#' @importFrom dplyr mutate filter select bind_rows
#' @import dplyr
#' @author Alice Carr
#'
#' @export
#'
#' @seealso
#' analyseCGM and cleanCGM
#'


intervention_split <- function(inputdirectory,
                               aggregated=F,
                               outputdirectory,
                               interventionfile) {

#load intervention file here:
intervention <- rio::import(interventionfile)
intervention$date<-anytime::anytime(intervention$date)

if (aggregated == F) {
  # Read in data: anticipated structure is a single folder containing clean CGM  per individual
  files <- base::list.files(path = inputdirectory, full.names = TRUE)
  } else if (aggregated == T) {
    # Read in data: anticipated structure is a single file of aggregated clean CGM
    files<-inputdirectory
  }


base::dir.create(paste0(outputdirectory,"data-after_interventions/"), showWarnings = F)
base::dir.create(paste0(outputdirectory,"data-before_interventions/"), showWarnings = F)
#identify before intervention and after intervention
before_intervention<-list()
after_intervention<-list()
before_intervention_all_patients<-list()
after_intervention_all_patients<-list()
#interventions are discrete, ie. if multiple interventions listed for 1 id time before will be before sequentional intervention but not including all time over other interventions

for (f in 1:base::length(files)) {
  if (aggregated == F) {
    # id from filename (used only in dexcom and libre if device is other then this is irrelavent)
    Id <- tools::file_path_sans_ext(basename(files[f]))
    # Id <- gsub("^([^_]+_[^_]+).*", "\\1", Id)
    print(Id)
    table <- base::suppressWarnings(rio::import(files[f], guess_max = 10000000))
  } else if (aggregated == T) {
    table <- base::suppressWarnings(rio::import(files))
  }


  if (aggregated == F) {
    if (length(base::unique(table$id)) > 1) {
      stop(print(paste("Data seems aggregated. There is more than one base::unique id in this individuals file, use aggregated=T. Check this ID if you are not expecting this:", Id)))
    } else if (length(base::unique(table$id)) == 1) {
      # order by timestamp
      table$id <- Id
      table <- table[base::order(table$timestamp), ]
    }

  } else if (aggregated == T) {
    table <- table[order(table$id, table$timestamp), ]
    Id <- "aggregated_data"
  }


  #merge table with interventions to find where to split
  table_int<-merge(table,intervention,all.x=T,by=c("id","date"))


 if(aggregated==F){
  intervention_points <- base::sort(base::unique(na.omit(table_int$intervention)))

  if(length(seq_along(intervention_points))==1) {
    before_intervention <- dplyr::filter(table_int, date < base::unique(na.omit(date[intervention==intervention_points[1]]))) %>%
      dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i])) %>%
      dplyr::select(-intervention)
    after_intervention <- dplyr::filter(table_int,date >= base::unique(na.omit(date[intervention==intervention_points[1]]))) %>%
      dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
      dplyr::select(-intervention)

    rio::export(x=before_intervention,file=paste0(outputdirectory,"data-before_interventions/",Id,"_beforeintervention",intervention_points[1],".csv"))
    rio::export(x=after_intervention,file=paste0(outputdirectory,"data-after_interventions/",Id,"_afterintervention",intervention_points[1],".csv"))

  }else if(length(seq_along(intervention_points))>1){

    for (i in seq_along(intervention_points)) {
      if (i==1){
        before_intervention[[i]] <- dplyr::filter(table_int, date < base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
          dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i])) %>%
          dplyr::select(-intervention)
        after_intervention[[i]] <- dplyr::filter(table_int,date >= base::unique(na.omit(date[intervention==intervention_points[i]])) & date<base::unique(na.omit(date[intervention==intervention_points[i+1]]))) %>%
          dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
          dplyr::select(-intervention)

        rio::export(x=before_intervention[[i]],file=paste0(outputdirectory,"data-before_interventions/",Id,"_beforeintervention",intervention_points[i],".csv"))
        rio::export(x=after_intervention[[i]],paste0(outputdirectory,"data-after_interventions/",Id,"_afterintervention",intervention_points[i],".csv"))

      } else if(i<max(length(seq_along(intervention_points)))){
        before_intervention[[i]] <- dplyr::filter(table_int, date > base::unique(na.omit(date[intervention==intervention_points[i-1]])) & date< base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
          dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i]))%>%
          dplyr::select(-intervention)
        after_intervention[[i]] <- dplyr::filter(table_int,date >= base::unique(na.omit(date[intervention==intervention_points[i]])) & date<base::unique(na.omit(date[intervention==intervention_points[i+1]]))) %>%
          dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
          dplyr::select(-intervention)

        rio::export(x=before_intervention[[i]],file=paste0(outputdirectory,"data-before_interventions/",Id,"_beforeintervention",intervention_points[i],".csv"))
        rio::export(x=after_intervention[[i]],paste0(outputdirectory,"data-after_interventions/",Id,"_afterintervention",intervention_points[i],".csv"))

      }else if(i==max(length(seq_along(intervention_points)))){
        before_intervention[[i]] <- dplyr::filter(table_int, date > base::unique(na.omit(date[intervention==intervention_points[i-1]])) & date< base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
        dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i]))%>%
          dplyr::select(-intervention)
        after_intervention[[i]] <- dplyr::filter(table_int,date >= base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
          dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
          dplyr::select(-intervention)

        rio::export(x=before_intervention[[i]],file=paste0(outputdirectory,"data-before_interventions/",Id,"_beforeintervention",intervention_points[i],".csv"))
        rio::export(x=after_intervention[[i]],paste0(outputdirectory,"data-after_interventions/",Id,"_afterintervention",intervention_points[i],".csv"))
      }
    }

    }else if(length(seq_along(intervention_points))==0){
      print(paste(Id,"has no intervention points listed in intervention file"))
    }

  }else if(aggregated==F){
    for(j in seq_along(base::unique(table_int$id))){
      table_int_patient<-dplyr::filter(table_int,id==base::unique(table_int$id)[j])
      intervention_points <- sort(base::unique(na.omit(table_int_patient$intervention)))

      if (length(seq_along(intervention_points))==1) {
        # Before the first intervention
        before_intervention_all_patients[[j]] <- dplyr::filter(table_int_patient, date < base::unique(na.omit(date[intervention==intervention_points[1]])))
        after_intervention_all_patients[[j]] <- dplyr::filter(table_int_patient,date >= base::unique(na.omit(date[intervention==intervention_points[1]])))
      } else if(length(seq_along(intervention_points))>1) {

        for (i in seq_along(intervention_points)) {

          # Between interventions
          if (i==1){
            before_intervention[[i]] <- dplyr::filter(table_int_patient, date < base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
              dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i])) %>%
              dplyr::select(-intervention)
            after_intervention[[i]] <- dplyr::filter(table_int_patient,date >= base::unique(na.omit(date[intervention==intervention_points[i]])) & date<base::unique(na.omit(date[intervention==intervention_points[i+1]]))) %>%
              dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
              dplyr::select(-intervention)
          } else if(i<max(length(seq_along(intervention_points)))){
            before_intervention[[i]] <- dplyr::filter(table_int_patient, date > base::unique(na.omit(date[intervention==intervention_points[i-1]])) & date< base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
              dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i]))%>%
              dplyr::select(-intervention)
            after_intervention[[i]] <- dplyr::filter(table_int_patient,date >= base::unique(na.omit(date[intervention==intervention_points[i]])) & date<base::unique(na.omit(date[intervention==intervention_points[i+1]]))) %>%
              dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
              dplyr::select(-intervention)
          }else if(i==max(length(seq_along(intervention_points)))){
            before_intervention[[i]] <- dplyr::filter(table_int_patient, date > base::unique(na.omit(date[intervention==intervention_points[i-1]])) & date< base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
              dplyr::mutate(id=paste0(id,"_beforeintervention",intervention_points[i]))%>%
              dplyr::select(-intervention)
            after_intervention[[i]] <- dplyr::filter(table_int_patient,date >= base::unique(na.omit(date[intervention==intervention_points[i]]))) %>%
              dplyr::mutate(id=paste0(id,"_afterintervention",intervention_points[i]))%>%
              dplyr::select(-intervention)
          }
        }
        before_intervention_all_patients[[j]]<-dplyr::bind_rows(before_intervention[[i]])
        after_intervention_all_patients[[j]]<-dplyr::bind_rows(after_intervention[[i]])

        }else if(length(seq_along(intervention_points))==0){
          warning(print(paste(base::unique(table_int$id)[j],"has no intervention points listed in intervention file")))
        }
    }
    rio::export(x=dplyr::bind_rows(before_intervention_all_patients),file=paste0(outputdirectory,"data-before_interventions/",Id,"_beforeintervention.csv"))
    rio::export(x=dplyr::bind_rows(after_intervention_all_patients),paste0(outputdirectory,"data-after_interventions/",Id,"_afterintervention.csv"))

    }

}
}



