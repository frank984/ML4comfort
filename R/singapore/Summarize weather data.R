# Load packages -----------------------------------------------------------

library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

library(ggplot2)
library(ggpubr)

library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
#library(ggmap)
library(gstat)
library(automap)
library(geoR)


# Load data ---------------------------------------------------------------

load("cleaned_data.RData")
# The following should be repeated for each weather variables, e.g. relative humidity, rainfall, etc.

# Summarize data ----------------------------------------------------------

# Time window for averaging
wdn="15 mins"
wdn="1 hour"

# Average air temperature
air_temp2=air_temp %>% 
  group_by(time=floor_date(time,wdn))%>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
# here na.rm=TRUE is used to ignore NA values. 
# With this value, when we compute mean for all NA values, it returns NaN.

# Inpute NaN values with NA
air_temp2[,-1]=air_temp2[,-1]%>% mutate_all(~ifelse(is.nan(.), NA, .))

# Equally distanced times -------------------------------------------------

eqdist_times=seq(from=air_temp2$time[1],
                 to=tail(air_temp2$time,1),
                 by=wdn)

# Merge data
air_temp3=air_temp2%>% 
  right_join(data.frame(time=eqdist_times),by="time")%>%
  arrange(time)

# Wrap up in a function
# This function is used to summarize weather data
summarize_weather=function(data,wdn){
  data2=data %>% 
    group_by(time=floor_date(time,wdn))%>%
    summarise_if(is.numeric, mean, na.rm = TRUE) 
  data2[,-1]=data2[,-1]%>% mutate_all(~ifelse(is.nan(.), NA, .))
  eqdist_times=seq(from=data2$time[1],
                   to=tail(data2$time,1),
                   by=wdn)
  data3=data2%>% 
    right_join(data.frame(time=eqdist_times),by="time")%>%
    arrange(time)
  return(data3)
}

# Apply to all weather variables
air_temp3=summarize_weather(air_temp,wdn)
rainfall3=summarize_weather(rainfall,wdn)
relative_humidity3=summarize_weather(RH,wdn)
wind_speed3=summarize_weather(wind_speed,wdn)
wind_direction3=summarize_weather(wind_dir,wdn)

# Save number of points used to calculate 15-mins averages

# Plot  -------------------------------------------------------------------

# Long format for easier plotting
air_temp_long=air_temp3 %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temperature")

windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_temp3$time,y=as.vector(unlist(air_temp3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp3[,i]))
  title(main=colnames(air_temp3)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# Relative humidity
relative_humidity_long=relative_humidity3 %>%
  pivot_longer(!time, names_to = "station", values_to = "relative_humidity")

# Plot
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(relative_humidity3)){
  plot(x=relative_humidity3$time,y=as.vector(unlist(relative_humidity3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(relative_humidity3[,i]))
  title(main=colnames(relative_humidity3)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)


# Rainfall
rainfall_long=rainfall3 %>%
  pivot_longer(!time, names_to = "station", values_to = "rainfall")

# Plot for the first 36 stations
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 2:36){
  plot(x=rainfall3$time,y=as.vector(unlist(rainfall3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rainfall3[,i]))
  title(main=colnames(rainfall3)[i])
}
mtext("Rainfall (1)", side = 3, line = - 2, outer = TRUE)

# Plot for the next 36 stations
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 37:72){
  plot(x=rainfall3$time,y=as.vector(unlist(rainfall3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rainfall3[,i]))
  title(main=colnames(rainfall3)[i])
}
mtext("Rainfall (2)", side = 3, line = - 2, outer = TRUE)

# Wind speed
wind_speed_long=wind_speed3 %>%
  pivot_longer(!time, names_to = "station", values_to = "wind_speed")

# Plot
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wind_speed3)){
  plot(x=wind_speed3$time,y=as.vector(unlist(wind_speed3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wind_speed3[,i]))
  title(main=colnames(wind_speed3)[i])
}
mtext("Wind speed", side = 3, line = - 2, outer = TRUE)

# Wind direction
wind_direction_long=wind_direction3 %>%
  pivot_longer(!time, names_to = "station", values_to = "wind_direction")

# Plot
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wind_direction3)){
  plot(x=wind_direction3$time,y=as.vector(unlist(wind_direction3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wind_direction3[,i]))
  title(main=colnames(wind_direction3)[i])
}
mtext("Wind direction", side = 3, line = - 2, outer = TRUE)


# NA analysis -------------------------------------------------------------
# Percentage of NA for each station in air_temp3
na_air=apply(air_temp3[,-1],2,function(x){sum(is.na(x))/length(x)}); na_air*100
na_air_rm=names(which(na_air>0.4))

# Percentage of NA in rainfall3
na_rainfall=apply(rainfall3[,-1],2,function(x){sum(is.na(x))/length(x)}); na_rainfall*100
na_rainfall_rm=names(which(na_rainfall>0.4))

# Percentage of NA in relative_humidity3
na_rh=apply(relative_humidity3[,-1],2,function(x){sum(is.na(x))/length(x)}); na_rh*100
na_rh_rm=names(which(na_rh>0.4))

# Percentage of NA in wind_speed3
na_ws=apply(wind_speed3[,-1],2,function(x){sum(is.na(x))/length(x)}); na_ws*100
na_ws_rm=names(which(na_ws>0.4))

# Percentage of NA in wind_direction3
na_wd=apply(wind_direction3[,-1],2,function(x){sum(is.na(x))/length(x)}); na_wd*100
na_wd_rm=names(which(na_wd>0.4))

# In all datasets, drop stations with more than 40% of missing values
air_temp3=air_temp3%>%
  select(-all_of(na_air_rm))
rainfall3=rainfall3%>%
  select(-all_of(na_rainfall_rm))
relative_humidity3=relative_humidity3%>%
  select(-all_of(na_rh_rm))
wind_speed3=wind_speed3%>%
  select(-all_of(na_ws_rm))
wind_direction3=wind_direction3%>%
  select(-all_of(na_wd_rm))


# Which stations have data for all five weather variables?
stations=intersect(colnames(air_temp3),colnames(rainfall3))
stations=intersect(stations,colnames(relative_humidity3))
stations=intersect(stations,colnames(wind_speed3))
stations=intersect(stations,colnames(wind_direction3))
stations=stations[-1];stations
length(stations)

# Save data for only this stations, name files like air_temp4
air_temp4=air_temp3[,c("time",stations)]
rainfall4=rainfall3[,c("time",stations)]
relative_humidity4=relative_humidity3[,c("time",stations)]
wind_speed4=wind_speed3[,c("time",stations)]
wind_direction4=wind_direction3[,c("time",stations)]

# Transform data to long format a
air_temp_long=air_temp4 %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temperature")

rainfall_long=rainfall4 %>%
  pivot_longer(!time, names_to = "station", values_to = "rainfall")

relative_humidity_long=relative_humidity4 %>%
  pivot_longer(!time, names_to = "station", values_to = "relative_humidity")

wind_speed_long=wind_speed4 %>%
  pivot_longer(!time, names_to = "station", values_to = "wind_speed")

wind_direction_long=wind_direction4 %>%
  pivot_longer(!time, names_to = "station", values_to = "wind_direction")


# Weather dataset ---------------------------------------------------------

# Join all data and add location
weather_data=merge(locations, air_temp_long, by="station")
weather_data=merge(weather_data, rainfall_long, by=c("time","station"))
weather_data=merge(weather_data, relative_humidity_long, by=c("time","station"))
weather_data=merge(weather_data, wind_speed_long, by=c("time","station"))
weather_data=merge(weather_data, wind_direction_long, by=c("time","station"))

# Drop X and name and sort by time
weather_data=weather_data[order(weather_data$time),]
weather_data=subset(weather_data,select = -c(X,name))

# Plot air_temp4
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(air_temp4)){
  plot(x=air_temp4$time,y=as.vector(unlist(air_temp4[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp4[,i]))
  title(main=colnames(air_temp4)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# Plot rainfall4
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(rainfall4)){
  plot(x=rainfall4$time,y=as.vector(unlist(rainfall4[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rainfall4[,i]))
  title(main=colnames(rainfall4)[i])
}
mtext("Rainfall", side = 3, line = - 2, outer = TRUE)

# Plot relative_humidity4
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(relative_humidity4)){
  plot(x=relative_humidity4$time,y=as.vector(unlist(relative_humidity4[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(relative_humidity4[,i]))
  title(main=colnames(relative_humidity4)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)

# Plot wind_speed4
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(wind_speed4)){
  plot(x=wind_speed4$time,y=as.vector(unlist(wind_speed4[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wind_speed4[,i]))
  title(main=colnames(wind_speed4)[i])
}
mtext("Wind speed", side = 3, line = - 2, outer = TRUE)

# Plot wind_direction4
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(wind_direction4)){
  plot(x=wind_direction4$time,y=as.vector(unlist(wind_direction4[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wind_direction4[,i]))
  title(main=colnames(wind_direction4)[i])
}
mtext("Wind direction", side = 3, line = - 2, outer = TRUE)

#save(weather_data, file="weather_data_long.RData")

# Corrplot
library(corrplot)
windows()
corrplot(cor(weather_data[,c("air_temperature",
                             "rainfall",
                             "relative_humidity",
                             "wind_speed",
                             "wind_direction")],use="complete.obs"),
         method="number")

# Scatter plot air_temp and relative_humidity
ggplot(weather_data,
       aes(x=air_temperature,y=relative_humidity)) +
  geom_point() +
  geom_smooth(method="lm") +
  theme_minimal()
