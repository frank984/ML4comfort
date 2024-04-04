
# Load libraries ----------------------------------------------------------
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
library(bayesbio)


# Load data ---------------------------------------------------------------

load("cleaned_data.Rdata")

# Summarize physio data --------------------------------------------------------
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
# wnd_phy="1 hour"
wnd_phy="15 mins"

physio_summ=function(x,window=wnd_phy){
  
  mean_x=x %>% group_by(time=floor_date(time,window))%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  # sd_x=x %>% group_by(time=floor_date(time,window))%>%
  #   summarise_if(is.numeric, sd, na.rm = TRUE)
  
  return(mean_x)
  
}

# Change the id_participant to character (needed for the join)
physio_data$id_participant=as.character(physio_data$id_participant)

# Create a summary data frame for the first participant based on a time window of length wdn_phy (15 mins)
# This step is needed because some participants have data at a 1 sec frequency 
physio_data_x=physio_data[physio_data$id_participant==participants[1],]
physio_data_x=physio_summ( physio_data_x[,-1])
physio_data_x=data.frame(id_participant=participants[1],physio_data_x)

# Loop through the rest of the participants
for(i in 2:length(participants)){
  temp=physio_data[physio_data$id_participant==participants[i],]
  temp=physio_summ( temp[,-1])
  temp=data.frame(id_participant=participants[i],temp)
  
  #Merge the summary data frame with the rest of the participants
  physio_data_x=full_join(physio_data_x,temp)
}

# Inpute NaN values with NA
physio_data_x[,-(1:2)]=physio_data_x[,-(1:2)]%>% mutate_all(~ifelse(is.nan(.), NA, .))

# Check for missing values
apply(physio_data_x,2,function(x){100*sum(is.na(x))/length(x)})

# If we consider all physio vars, after dropping NA rows, we obtain the following dataset
physio_lowT_all=physio_data_x[complete.cases(physio_data_x), ]
dim(physio_lowT_all)

# But the final dimension is not satisfactory, so we will consider only the following physio vars
# as ts_oxygen_saturation and ts_resting_heart_rate have a lot of missing values (more than 90%)
physio_highT_all=subset(physio_data_x, select = -c(ts_oxygen_saturation,
                                                   ts_resting_heart_rate))
physio_highT_all=physio_highT_all[complete.cases(physio_highT_all), ]


# Match physio with survey data -------------------------------------------

# Drop NAs for survey data
data_cozie_train_clean=subset(cozie_train, select = -c(ts_oxygen_saturation,
                                                       ts_resting_heart_rate,
                                                       ts_stand_time,
                                                       ts_step_count,
                                                       ts_walking_distance, 
                                                       ts_heart_rate,
                                                       ts_audio_exposure_environment))

data_cozie_train_clean=data_cozie_train_clean[complete.cases(data_cozie_train_clean), ]

# We modify the bayesbio::nearestTimeandID function to output minimum times (time difference computed for matching the two datasets)

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

# Merge with cozie data finding the "closest" physio var for each row of cozie_train

df_cozie_merged=nearestTimeandID2(physio_highT_all, 
                                  data_cozie_train_clean,
                                  "time",
                                  "time",
                                  "id_participant")
# load("df_cozie_merged.Rdata")
dim(df_cozie_merged$dfAll)

# Check maximum time difference
max(df_cozie_merged$min_time)/60/60

# Maximum time difference allowed between measurement of physio and cozie variables: 15 mins
tol_hours=.25
indx=which(df_cozie_merged$min_times<60*60*tol_hours)
length(indx)
df_cozie=df_cozie_merged$dfAll[indx,]
