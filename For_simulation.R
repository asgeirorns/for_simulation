#Bua til gogn fyrir hermun

rm(list=ls())
load("adkort.Rdata")
library(jsonlite)


Pat_list <- fromJSON("getWaitinglist_all_year.json", flatten=TRUE)
Pat_list_df <- as.data.frame(Pat_list$OrbitWaitingList, stringsAsFactors = TRUE)


#Skodum oktober
Start_date <- as.Date("2017-10-01")
End_date <- as.Date("2017-10-31")


#------------------------------------------------------------#
#Skrifum ut ef Ward eda ICU fyrir hvern pat
Ward <- rep(0, nrow(Pat_list_df))
ICU <- rep(0, nrow(Pat_list_df))
for(m in (1:nrow(Pat_list_df))){
  idx = which(Pat_list_df$OrbitOperation.OvernightICU[m] == 1)
  if(length(idx)>0){
    ICU[m] <- 1
  }
  else{
    ICU[m]<- 0
  }
}


for(n in (1:nrow(Pat_list_df))){
  idx= which(Pat_list_df$OrbitOperation.PatientAdmission[n] %in% c('Legudeild'))
  if(length(idx)>0){
    Ward[n] <- 1
  }
  else
    Ward[n] <- 0
}

kort <- Pat_list_df$OrbitOperation.OperationCard
kt <- Pat_list_df$OrbitOperation.PatientSSN
laeknir <- Pat_list_df$OrbitOperation.RequestedOperator_Name
new_df <- data.frame(kort, kt, Ward, ICU, laeknir)

#--------------------------------------------------------#

#Filterum ut
idx = which(as.Date(Pat_list_df$OrbitOperation.PlannedStartTime_Date) >= Start_date & as.Date(Pat_list_df$OrbitOperation.PlannedStartTime_Date) <=End_date & 
              Pat_list_df$OrbitOperation.OperationSpecialty %in% c('Hb. Alm.') & !Pat_list_df$OrbitOperation.OperationType  %in% c('Bráðaaðgerð') &
              !Pat_list_df$OrbitOperation.RequestedOperator_Name %in% c('Þorvaldur Jónsson') & !Pat_list_df$OrbitOperation.OperationRoom %in% c('Kv. Stofa 21'))
Pat_list_df <- Pat_list_df[idx,]
new_df <- new_df[idx,]

#Finnum unqieu surgeons
Unique_surg <- unique(Pat_list_df$OrbitOperation.RequestedOperator_Name)

#Keyrum skurdlaekna i loopu

for(mm in Unique_surg){
  idxx=which(mm==Pat_list_df$OrbitOperation.RequestedOperator_Name)
  if(length(idxx)>0){
    fname= paste0("Gjorgaesla",'_',gsub(" ", "", mm),'.txt')

for (i in c(1:nrow(Pat_list_df))) {
    idx = which(Pat_list_df$OrbitOperation.OvernightICU[i]==c("1") & Pat_list_df$OrbitOperation.OperationCard[i] %in% adkort$Adgerdakort &
                  Pat_list_df$OrbitOperation.RequestedOperator_Name[i] == mm)
    if(length(idx)>0){
      adge=Pat_list_df$OrbitOperation.OperationCard[i]
      kt = Pat_list_df$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",GjorLikur[adge,j]), file=fname, sep =
              "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
}






fname= paste0("Lega",'_',gsub(" ", "", mm),'.txt')

#Skrifum ut expectedWard fyrir hvern dag, viljum einungis tha sem eru merktir fyrir ad thurfa ward
for (i in c(1:nrow(Pat_list_df))) {
    #We must also check if the operation card exist in our data
    idx = which(Pat_list_df$OrbitOperation.PatientAdmission[i]==c("Legudeild") & Pat_list_df$OrbitOperation.OperationCard[i] %in% adkort$Adgerdakort &
                  Pat_list_df$OrbitOperation.RequestedOperator_Name[i] == mm) 
    if(length(idx)>0){
      adge = Pat_list_df$OrbitOperation.OperationCard[i]
      kt = Pat_list_df$OrbitOperation.PatientSSN[i]
      cat(paste0('"',kt,'-',adge,'"','\t',' 1'),file=fname ,sep="", append=TRUE)
      for (j in c(1:6))
        cat(sprintf(" %.3f ",LeguLikur[adge,j]), file=fname, sep = "", append=TRUE)
      cat(" ",file=fname,sep="\n", append=TRUE)
    }
  }

  }
}


for(surg in Unique_surg){
  idxx=which(surg==Pat_list_df$OrbitOperation.RequestedOperator_Name)
  if(length(idxx)>0){
    fname= paste0("Ward_ICU",'_',gsub(" ", "", surg),'.txt')
   
     for(i in (1:nrow(new_df))){
      idx= which(new_df$laeknir[i] == surg)
      if(length(idx)>0){
        cat(paste0('"',new_df$kt[i],'-',new_df$kort[i],'"','\t',new_df$Ward[i],'\t',new_df$ICU[i],'\n'),file=fname ,sep="", append=TRUE)
}
}


  }}


for(surg in Unique_surg){
  idxx=which(surg==Pat_list_df$OrbitOperation.RequestedOperator_Name)
  if(length(idxx)>0){
    fname= paste0("Surgery_duration",'_',gsub(" ", "", surg),'.txt')

for (i in c(1:nrow(Pat_list_df))){
  idxy = which(Pat_list_df$OrbitOperation.RequestedOperator_Name[i] == surg)
  if(length(idxy)>0){
    adge = Pat_list_df$OrbitOperation.OperationCard[i] #ur bidlista
    laeknir = Pat_list_df$OrbitOperation.RequestedOperator_Name[i] #ur bidlista
    kt = Pat_list_df$OrbitOperation.PatientSSN[i] #ur bidlista
    idx = which(adkort$Adgerdakort==adge & adkort$Laeknir==laeknir 
                & adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 &adge %in% adkort$Adgerdakort
                & adkort$Skurdstofutimi>0 )
    
    cat(paste0('"',kt,'-',adge,'"'), sep="", file=fname ,append=TRUE)
    
    if(length(idx)<10){
      # Notum tima  annara ef thad finnst ekki 
      idx = which(adkort$Adgerdakort==adge &
                    adkort$AdgerdaTimi<=24*60 & adkort$AdgerdaTimi>0 & adge %in% adkort$Adgerdakort
                  & adkort$Skurdstofutimi>0)
    }
    idx <- rev(idx)
    idx <- idx[1:min(10,length(idx))]
    cat(paste('\t',adkort$Skurdstofutimi[idx],'\t'),file=fname, sep="", append=TRUE)
    cat(" ",file=fname,sep="\n", append=TRUE)
}
      
}}
}









