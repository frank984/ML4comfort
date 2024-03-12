# Only outdoor

# Load packages
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lubridate)

library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
#library(ggmap)
library(gstat)
library(automap)

load("cleaned_data.Rdata")
cozie_train_out=cozie_train[which(cozie_train$q_location=="Outdoor"),]

# Summarize physio data
physio_vars=c("ts_oxygen_saturation",
              "ts_resting_heart_rate",
              "ts_stand_time",
              "ts_step_count",
              "ts_walking_distance",
              "ts_heart_rate",
              "ts_audio_exposure_environment")
physio_data=cozie_train[c("id_participant","time",physio_vars)]

participants=unique(cozie_train$id_participant)

# wnd_phy="2 hours"
wnd_phy="1 hour"

physio_summ=function(x,window=wnd_phy){
  
  mean_x=x %>% group_by(time=floor_date(time,window))%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  # sd_x=x %>% group_by(time=floor_date(time,window))%>%
  #   summarise_if(is.numeric, sd, na.rm = TRUE)
  
  return(mean_x)
  
}

physio_data$id_participant=as.character(physio_data$id_participant)

physio_data_x=physio_data[physio_data$id_participant==participants[1],]
physio_data_x=physio_summ( physio_data_x[,-1])
physio_data_x=data.frame(id_participant=participants[1],physio_data_x)

for(i in 2:length(participants)){
  temp=physio_data[physio_data$id_participant==participants[i],]
  temp=physio_summ( temp[,-1])
  temp=data.frame(id_participant=participants[i],temp)
  physio_data_x=full_join(physio_data_x,temp)
  #temp2=merge(physio_data_x,temp,by=c("time","id_participant"))
}

apply(physio_data_x,2,function(x){100*sum(is.na(x))/length(x)})
# physio_lowT_all=physio_data_x[!is.na(physio_data_x$ts_oxygen_saturation),]
# physio_lowT_all=physio_lowT_all[!is.na(physio_lowT_all$ts_resting_heart_rate ),]

# Consider variables with more than 50% of data
physio_highT_all=subset(physio_data_x, select = -c(ts_oxygen_saturation,
                                                   ts_resting_heart_rate
                                                   # , ts_stand_time
))
physio_highT_all=physio_highT_all[complete.cases(physio_highT_all), ]
#save(physio_highT_all,file="physio_highT_all.Rdata")
load("physio_highT_all.Rdata")
dim(physio_highT_all)

# merge with cozie
library(bayesbio)
data_cozie_train_clean=subset(cozie_train_out, select = -c(ts_oxygen_saturation,
                                                       ts_resting_heart_rate,
                                                       ts_stand_time,
                                                       ts_step_count,
                                                       ts_walking_distance, 
                                                       ts_heart_rate,
                                                       ts_audio_exposure_environment))

data_cozie_train_clean=data_cozie_train_clean[complete.cases(data_cozie_train_clean), ]

dim(data_cozie_train_clean)

#### Rivedere
nearestTimeandID2=function (df1, df2, timeCol1, timeCol2, IDcol){
  if (!timeCol1 %in% colnames(df1)) 
    stop("timeCol1 must specify a column name in df1.")
  if (!timeCol2 %in% colnames(df2)) 
    stop("timeCol2 must specify a column name in df2.")
  dfMinTime = data.frame(matrix(ncol = (ncol(df2) - 1), nrow = nrow(df1)))
  colnames(dfMinTime) = colnames(df2)[!colnames(df2) %in% timeCol2]
  ties_count = 0
  #
  min_times=NULL
  #
  A=NULL
  for (i in 1:nrow(df1)) {
    ID = df1[i, IDcol]
    min_rows = vector()
    for (j in 1:nrow(df2)) {
      if (df2[j, IDcol] == ID) {
        tmp =abs(as.numeric(
          difftime(df1[i, timeCol1],df2[j, timeCol2]),units="secs"))
        # #
        # min_times=c(tmp,min_times)
        # #
        # #tmp=abs(as.numeric(tmp))
        min_rows = c(min_rows, tmp)
      }
      else {
        min_rows = c(min_rows, NA)
      }
    }
    mins = (min_rows == min(min_rows))
    
    A = rbind(df2[which.min(min_rows), !(colnames(df2) %in% 
                                           timeCol2), drop = FALSE],A)
    
    ### Why does it change factors into integer here???
    # dfMinTime[i, ]=df2[which.min(min_rows), !(colnames(df2) %in% 
    #                                                   timeCol2), drop = FALSE]
    ###
    min_times=c(min(min_rows,na.rm = T),min_times)
    if (sum(mins, na.rm = TRUE) > 1) {
      ties_count = ties_count + 1
    }
  }
  
  if (ties_count > 0) {
    message("Warning: there were ", ties_count, " difftime ties, for which the first corresponding row of df2 was chosen for merging.")
  }
  #dfAll = cbind(df1, dfMinTime)
  dfAll = cbind(df1, A)
  dfAll = dfAll[, !duplicated(colnames(dfAll))]
  return(list(dfAll=dfAll,
              min_times=min_times))
}

df_cozie_merged=nearestTimeandID2(physio_highT_all, 
                                  data_cozie_train_clean,
                                  "time",
                                  "time",
                                  "id_participant")

# save(df_cozie_merged,file="df_cozie_merged.Rdata")
# load("df_cozie_merged.Rdata")

dim(df_cozie_merged$dfAll)
max(df_cozie_merged$min_time)/60/60

load("df_cozie_merged.Rdata")

# maximum time difference allowed between measurement of physio and cozie variables
# tol_hours=1
tol_hours=.5
indx=which(df_cozie_merged$min_times<60*60*tol_hours)
length(indx)

