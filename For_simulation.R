# clean memory and load statistical data for OR cards
rm(list = ls())
setwd("~/projects/betrirodun")
load("adkort.Rdata")
# load patient list from json file and convert to data frame
library(jsonlite)
Pat_list <- fromJSON("getWaitingList_arid_2017.json", flatten = TRUE)
Pat_list_df <- as.data.frame(Pat_list$OrbitWaitingList, stringsAsFactors = TRUE)

# choose period to work with
Start_date <- as.Date("2017-10-01")
End_date <- as.Date("2017-10-31")

#------------------------------------------------------------#
# Determine if we expect the patient to go to ward or ICU
ICU <- rep(0, nrow(Pat_list_df))
for (m in (1:nrow(Pat_list_df))) {
  ICU[m] <- (Pat_list_df$OrbitOperation.OvernightICU[m] == 1)
}
Ward <- rep(0, nrow(Pat_list_df))
for (n in (1:nrow(Pat_list_df))) {
  Ward[n] <- (Pat_list_df$OrbitOperation.PatientAdmission[n] %in% c('Legudeild'))
}

kort <- Pat_list_df$OrbitOperation.OperationCard
kt <- Pat_list_df$OrbitOperation.PatientSSN
laeknir <- Pat_list_df$OrbitOperation.RequestedOperator_Name
new_df <- data.frame(kort, kt, Ward, ICU, laeknir)

# now pick out the time window used for the analysis
idx = which(
  as.Date(Pat_list_df$OrbitOperation.PlannedStartTime_Date) >= Start_date &
    as.Date(Pat_list_df$OrbitOperation.PlannedStartTime_Date) <= End_date &
    Pat_list_df$OrbitOperation.OperationSpecialty %in% c('Hb. Alm.') &
    !Pat_list_df$OrbitOperation.OperationType  %in% c('Bráðaaðgerð') &
    !Pat_list_df$OrbitOperation.RequestedOperator_Name %in% c('Þorvaldur Jónsson') &
    !Pat_list_df$OrbitOperation.OperationRoom %in% c('Kv. Stofa 21')
)
Pat_list_df <- Pat_list_df[idx,]
new_df <- new_df[idx,]

#--------------------------------------------------------#


# For each surgeon we create a set of files
Unique_surg <- unique(Pat_list_df$OrbitOperation.RequestedOperator_Name)

# Change directory to pattern, many txt files to dump
setwd(paste0(getwd(), "/patterns"))

# Now we loop through the surgeons
# map out the ICU times file
for (mm in Unique_surg) {
  # find the patients that belong to this surgeon
  idxx = which(mm == Pat_list_df$OrbitOperation.RequestedOperator_Name)
  tmpPat_list_df = Pat_list_df[idxx,]
  fname = paste0("Gjorgaesla", '_', gsub(" ", "", mm), '.txt')
  cat("", file = fname , sep = "", append = FALSE)
  for (i in c(1:nrow(tmpPat_list_df))) {
    adge = tmpPat_list_df$OrbitOperation.OperationCard[i]
    kt = tmpPat_list_df$OrbitOperation.PatientSSN[i]
    tmpname <- gsub("[[:space:]]", "", paste0(kt, '-', adge))
    cat(paste0(tmpname, ' 1'),file = fname , sep = "", append = TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f", GjorLikur[adge, j]),file = fname, sep = "", append = TRUE)
    cat("", file = fname, sep = "\n", append = TRUE)
  }
}    

# map out the Ward times file
for (mm in Unique_surg) {
  # find the patients that belong to this surgeon
  idxx = which(mm == Pat_list_df$OrbitOperation.RequestedOperator_Name)
  tmpPat_list_df = Pat_list_df[idxx,]
  fname = paste0("Lega", '_', gsub(" ", "", mm), '.txt')
  cat("", file = fname , sep = "", append = FALSE)
  for (i in c(1:nrow(tmpPat_list_df))) {
    adge = tmpPat_list_df$OrbitOperation.OperationCard[i]
    kt = tmpPat_list_df$OrbitOperation.PatientSSN[i]
    tmpname <- gsub("[[:space:]]", "", paste0(kt, '-', adge))
    cat(paste0(tmpname, ' 1'), file = fname , sep = "", append = TRUE)
    for (j in c(1:6))
      cat(sprintf(" %.3f", LeguLikur[adge, j]), file = fname, sep = "", append = TRUE)
    cat("", file = fname, sep = "\n", append = TRUE)
  }
}
# map out the ICU/Ward times file
for (mm in Unique_surg) {
  idxx = which(mm == new_df$laeknir)
  tmpnew_df = new_df[idxx,]
  fname = paste0("Ward_ICU", '_', gsub(" ", "", mm), '.txt')
  cat("", file = fname , sep = "", append = FALSE)
  for (i in (1:nrow(tmpnew_df))) {
    tmpname <- gsub("[[:space:]]", "", paste0(tmpnew_df$kt[i], '-', tmpnew_df$kort[i]))
    cat(paste0(tmpname,' ',tmpnew_df$Ward[i],'\t',tmpnew_df$ICU[i],'\n'), file = fname,sep = "",append = TRUE)
  }
}

# and finally we drop the surgery times
for (mm in Unique_surg) {
  idxx = which(mm == Pat_list_df$OrbitOperation.RequestedOperator_Name)
  tmpPat_list_df <- Pat_list_df[idxx,]
  fname = paste0("Surgery_duration", '_', gsub(" ", "", mm), '.txt')
  cat("", file = fname , sep = "", append = FALSE)
  for (i in c(1:nrow(tmpPat_list_df))) {
    adge = tmpPat_list_df$OrbitOperation.OperationCard[i] #ur bidlista
    laeknir = tmpPat_list_df$OrbitOperation.RequestedOperator_Name[i] #ur bidlista
    kt = tmpPat_list_df$OrbitOperation.PatientSSN[i] #ur bidlista
    idx = which(adkort$Adgerdakort == adge & adkort$Laeknir == laeknir &
          adkort$AdgerdaTimi <= 24 * 60 &
          adkort$AdgerdaTimi > 0 & adge %in% adkort$Adgerdakort &
          adkort$Skurdstofutimi > 0
      )
    tmpname <- gsub("[[:space:]]", "", paste0(kt, '-', adge))
    cat(tmpname, sep = "", file = fname , append = TRUE)
      
    if (length(idx) < 10) {
        # Notum tima  annara ef thad finnst ekki
      idx = which(adkort$Adgerdakort == adge &
                  adkort$AdgerdaTimi <= 24 * 60 &
                  adkort$AdgerdaTimi > 0 & adge %in% adkort$Adgerdakort &
                  adkort$Skurdstofutimi > 0
      )
    }
    idx <- rev(idx)
    idx <- idx[1:min(30, length(idx))]
    for (i in idx)
      cat(paste0(' ',adkort$Skurdstofutimi[i]), file = fname, sep = "", append = TRUE)
    cat("", file = fname,sep = "\n",append = TRUE)
  }
}
