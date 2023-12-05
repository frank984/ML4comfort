# Source: https://www.kaggle.com/competitions/cool-quiet-city-competition/data

# This competition's goal is to use contextual data to predict noise distraction 
# and thermal preference across a diversity of indoor and outdoor spaces

# Load packages
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)

# Load data ---------------------------------------------------------------

cozie_test=read.csv("cozie_responses_and_physiological_data_test_public.csv")
cozie_train=read.csv("cozie_responses_and_physiological_data_training.csv")

# latitude and longitude of the weather stations
locations=read.csv("weather_stations.csv")

air_temp=read.csv("weather_air-temperature.csv")
rainfall=read.csv("weather_rainfall.csv")
RH=read.csv("weather_relative-humidity.csv")
wind_dir=read.csv("weather_wind-direction.csv")
wind_speed=read.csv("weather_wind-speed.csv")

# Format dates ------------------------------------------------------------

# Round dates to minutes
formatdates=function(x){
  colnames(x)[1]="time"
  x$time=strptime(x$time, format="%Y-%m-%d  %H:%M:%S",tz="Asia/Singapore")
  # Format POSIXct requeired by ggplot
  x$time=as.POSIXct(x$time, format="%Y-%m-%d  %H:%M:%S",tz="Asia/Singapore")
  return(x)
}

cozie_train=formatdates(cozie_train)

# Convert microsurvey columns and id participant to factor
microsurvey=c("q_alone_group","q_earphones","q_location",
              "q_location_office","q_location_transport",
              "q_noise_kind","q_noise_nearby","q_thermal_preference",
              "q_activity_category_alone" ,"q_activity_category_group")

cozie_train[microsurvey] <- lapply(cozie_train[microsurvey], factor)

cozie_test=formatdates(cozie_test)
cozie_test[microsurvey] <- lapply(cozie_test[microsurvey], factor)

cozie_train$id_participant=factor(cozie_train$id_participant)
cozie_test$id_participant=factor(cozie_test$id_participant)


air_temp=formatdates(air_temp)
rainfall=formatdates(rainfall)
RH=formatdates(RH)
wind_dir=formatdates(wind_dir)
wind_speed=formatdates(wind_speed)


# Check and adjust structure ----------------------------------------------

str(cozie_train)
cozie_train$ws_timestamp_location=strptime(cozie_train$ws_timestamp_location, 
                                           format="%Y-%m-%d  %H:%M:%S",
                                           tz="Asia/Singapore")
# Format POSIXct requeired by ggplot
cozie_train$ws_timestamp_location=as.POSIXct(cozie_train$ws_timestamp_location, 
                                             format="%Y-%m-%d  %H:%M:%S",
                                             tz="Asia/Singapore")

cozie_train$ws_timestamp_start=strptime(cozie_train$ws_timestamp_start, 
                                           format="%Y-%m-%d  %H:%M:%S",
                                           tz="Asia/Singapore")
# Format POSIXct requeired by ggplot
cozie_train$ws_timestamp_start=as.POSIXct(cozie_train$ws_timestamp_start, 
                                             format="%Y-%m-%d  %H:%M:%S",
                                             tz="Asia/Singapore")

## 
cozie_test$ws_timestamp_location=strptime(cozie_test$ws_timestamp_location, 
                                           format="%Y-%m-%d  %H:%M:%S",
                                           tz="Asia/Singapore")
# Format POSIXct requeired by ggplot
cozie_test$ws_timestamp_location=as.POSIXct(cozie_test$ws_timestamp_location, 
                                             format="%Y-%m-%d  %H:%M:%S",
                                             tz="Asia/Singapore")

cozie_test$ws_timestamp_start=strptime(cozie_test$ws_timestamp_start, 
                                          format="%Y-%m-%d  %H:%M:%S",
                                          tz="Asia/Singapore")
# Format POSIXct requeired by ggplot
cozie_test$ws_timestamp_start=as.POSIXct(cozie_test$ws_timestamp_start, 
                                            format="%Y-%m-%d  %H:%M:%S",
                                            tz="Asia/Singapore")
str(cozie_test)

# Data visualization -------------------------------------------------------------

# Time ranges
as.Date(range(cozie_train$time))
as.Date(range(air_temp$time))
as.Date(range(RH$time))
as.Date(range(wind_dir$time))
as.Date(range(wind_speed$time))
as.Date(range(rainfall$time))

# Function to create list with elements names (not used)
# List <- function(...) {
#   names <- as.list(substitute(list(...)))[-1L]
#   setNames(list(...), names)
# }


# Function to plot all environmental vars for each site
plot_onesite=function(x1=air_temp,x2=rainfall,x3=RH,x4=wind_dir,x5=wind_speed,
                      site="S24"){
  
  P_x1=ggplot(data=x1,aes(x=time,y=x1[[site]]))+
    geom_line(linewidth=.65,color="#9999CC")+
    labs(y="Air temperature",x="Time", title=site)+
    theme_bw()+
    scale_x_datetime(labels = date_format("%y-%m-%d"))
  
  P_x2=ggplot(data=x2,aes(x=time,y=x2[[site]]))+
    geom_line(linewidth=.65,color="#9999CC")+
    labs(y="Rainfall",x="Time")+
    theme_bw()+
    scale_x_datetime(labels = date_format("%y-%m-%d"))
  
  P_x3=ggplot(data=x3,aes(x=time,y=x3[[site]]))+
    geom_line(linewidth=.65,color="#9999CC")+
    labs(y="Relative humidity",x="Time")+
    theme_bw()+
    scale_x_datetime(labels = date_format("%y-%m-%d"))
  
  P_x4=ggplot(data=x4,aes(x=time,y=x4[[site]]))+
    geom_line(linewidth=.65,color="#9999CC")+
    labs(y="Wind direction",x="Time")+
    theme_bw()+
    scale_x_datetime(labels = date_format("%y-%m-%d"))
  
  P_x5=ggplot(data=x5,aes(x=time,y=x5[[site]]))+
    geom_line(linewidth=.65,color="#9999CC")+
    labs(y="Wind speed",x="Time")+
    theme_bw()+
    scale_x_datetime(labels = date_format("%y-%m-%d"))
  
  all_P=ggarrange(P_x1, P_x2,P_x3,P_x4,P_x5,
                  nrow = 5)
  return(all_P)
}

windows()
plot_onesite(site="S24")


# Plot a given variable for different sites

P_s24_RH=ggplot(data=RH,aes(x=time,y=S24))+
  geom_line(linewidth=.65,color="#9999CC")+
  labs(y="RH - S24",x="Time",title="Relative Humidity")+
  theme_bw()+
  scale_x_datetime(labels = date_format("%y-%m-%d"))
P_s102_RH=ggplot(data=RH,aes(x=time,y=S102))+
  geom_line(linewidth=.65,color="#9999CC")+
  labs(y="RH - S102",x="Time")+
  theme_bw()+
  scale_x_datetime(labels = date_format("%y-%m-%d"))
P_s116_RH=ggplot(data=RH,aes(x=time,y=S116))+
  geom_line(linewidth=.65,color="#9999CC")+
  labs(y="RH - S116",x="Time")+
  theme_bw()+
  scale_x_datetime(labels = date_format("%y-%m-%d"))

windows()
ggarrange(P_s24_RH, P_s102_RH,P_s116_RH,
          nrow = 3)

# Plot cozie vars
windows()
ggplot(data=cozie_train,aes(x=time,y=ts_heart_rate))+
  geom_line(color="#9999CC")+
  labs(y="Heart rate",x="Time")+
  theme_bw()+
  scale_x_datetime(labels = date_format("%y-%m-%d %H:%M:%S"))

#

# Sparsity analysis -------------------------------------------------------
# Hence, you can filter for watch survey responses with df[df.ws_survey_response.notna()], 
# which will show all rows with watch survey responses.

length(which(cozie_train$q_thermal_preference!=""))
cozie_train_nosparse=cozie_train[which(cozie_train$q_thermal_preference!=""),]

dim(cozie_train_nosparse)
# Approx 4900 not-NA observations

# Summary stats
summary(cozie_train[microsurvey])

#Relative frequencies for the microsurvey vars without considering the NAs
apply(cozie_train_nosparse[microsurvey],2,function(x)prop.table(table(x)))

ggplot(cozie_train_nosparse, aes(x=q_thermal_preference )) +
  geom_bar(fill=c("blue","green","red")) 



# OLD ---------------------------------------------------------------------

# extract target variable summary (q_noise_nearby, q_noise_kind, q_earphones, q_thermal_preference)
target_train=cozie_train[,c("q_noise_nearby", "q_noise_kind",
               "q_earphones", "q_thermal_preference")]

# convert columns to factors
target_train=as.data.frame(unclass(target_train),stringsAsFactors=TRUE)

# summary stats of target vars
summary(target_train)

NA_q_thermal_pref=which(cozie_train$q_thermal_preference=="")
NA_q_noise_kind=which(cozie_train$q_noise_kind=="")
NA_q_earphones=which(cozie_train$q_earphones =="")
NA_q_noise_nearby=which(cozie_train$q_noise_nearby =="")

sum(NA_q_earphones!=NA_q_noise_kind)
sum(NA_q_earphones!=NA_q_noise_nearby)
sum(NA_q_earphones!=NA_q_thermal_pref)


