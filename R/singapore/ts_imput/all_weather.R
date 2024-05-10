load("cleaned_data.RData")
source("Utils.R")
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL", "English")
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
#library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
library(ggmap)
library(gstat)
library(automap)
library(xts)
library(geosphere)


# Summarize on hourly basis -----------------------------------------------

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

wdn="1 hour"

# Apply to all weather variables
air_temp=summarize_weather(air_temp,wdn)
rainfall=summarize_weather(rainfall,wdn)
RH=summarize_weather(RH,wdn)
wsp=summarize_weather(wind_speed,wdn)
wdir=summarize_weather(wind_dir,wdn)

# Air temp
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(air_temp)){
  plot(x=air_temp$time,y=as.vector(unlist(air_temp[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp[,i]))
  title(main=colnames(air_temp)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# Relative humidity
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(RH)){
  plot(x=RH$time,y=as.vector(unlist(RH[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(RH[,i]))
  title(main=colnames(RH)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)


# Rainfall
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 2:36){
  plot(x=rainfall$time,y=as.vector(unlist(rainfall[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rainfall[,i]))
  title(main=colnames(rainfall)[i])
}
mtext("Rainfall (1)", side = 3, line = - 2, outer = TRUE)

# Plot for the next 36 stations
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 37:72){
  plot(x=rainfall$time,y=as.vector(unlist(rainfall[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rainfall[,i]))
  title(main=colnames(rainfall)[i])
}
mtext("Rainfall (2)", side = 3, line = - 2, outer = TRUE)

# Wind speed
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wsp)){
  plot(x=wsp$time,y=as.vector(unlist(wsp[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wsp[,i]))
  title(main=colnames(wsp)[i])
}
mtext("Wind speed", side = 3, line = - 2, outer = TRUE)

# Wind direction
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wdir)){
  plot(x=wdir$time,y=as.vector(unlist(wdir[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(wdir[,i]))
  title(main=colnames(wdir)[i])
}
mtext("Wind direction", side = 3, line = - 2, outer = TRUE)

#  NA counts --------------------------------------------------------------

na_count=function(x){
  x %>% summarise_all(funs(sum(is.na(.))))
}

# Drop stations with more than 40%  of missings
# Also, impute as NA values that have been linearly interpolated

air_temp_na=na_count(air_temp); air_temp_na=air_temp_na/dim(air_temp)[1]*100;air_temp_na
air_temp=air_temp[,-which(air_temp_na>40)]

RH_na=na_count(RH); RH_na=RH_na/dim(RH)[1]*100
RH=RH[,-which(RH_na>40)]

rain_na=na_count(rainfall); rain_na=rain_na/dim(rainfall)[1]*100
rainfall=rainfall[,-which(rain_na>40)]

wsp_na=na_count(wind_speed); wsp_na=wsp_na/dim(wind_speed)[1]*100
wsp=wsp[,-which(wsp_na>40)]

wdir_na=na_count(wind_dir); wdir_na=wdir_na/dim(wind_dir)[1]*100
wdir=wdir[,-which(wdir_na>40)]

# New plot
# Air temp
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(air_temp)){
  plot(x=air_temp$time,y=as.vector(unlist(air_temp[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(air_temp[,i]))
  title(main=colnames(air_temp)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# Relative humidity
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(RH)){
  plot(x=RH$time,y=as.vector(unlist(RH[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(RH[,i]))
  title(main=colnames(RH)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)


# Rainfall
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 2:36){
  plot(x=rainfall$time,y=as.vector(unlist(rainfall[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(rainfall[,i]))
  title(main=colnames(rainfall)[i])
}
mtext("Rainfall (1)", side = 3, line = - 2, outer = TRUE)

# Plot for the next 36 stations
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 37:72){
  plot(x=rainfall$time,y=as.vector(unlist(rainfall[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(rainfall[,i]))
  title(main=colnames(rainfall)[i])
}
mtext("Rainfall (2)", side = 3, line = - 2, outer = TRUE)


# Wind speed
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wsp)){
  plot(x=wsp$time,y=as.vector(unlist(wsp[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(wsp[,i]))
  title(main=colnames(wsp)[i])
}
mtext("Wind speed", side = 3, line = - 2, outer = TRUE)

# Wind direction
windows()
par(mfrow=c(4,5),mar=c(2,2,6,2))
for(i in 2:ncol(wdir)){
  plot(x=wdir$time,y=as.vector(unlist(wdir[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(wdir[,i]))
  title(main=colnames(wdir)[i])
}
mtext("Wind direction", side = 3, line = - 2, outer = TRUE)

count_gaps=function(x){
  gaps=list()
  gaps[[1]]=x[,1]
  for(i in 2:ncol(x)){
    d_na <- as.numeric(is.na(x[,i]))
    Nal=rle(d_na)
    tosave=Nal$values==1
    Nals=Nal$lengths[tosave]
    gaps[[i]]=Nals
  }
  names(gaps)=colnames(x)
  return(gaps)
}

# Gap lengths for air_data_wide as % of total length
NA_count_air=count_gaps(air_temp);NA_count_air
lapply(NA_count[-1],function(x) round(x/dim(air_data_wide)[1]*100,1))



# Spatially interpolate missing values ------------------------------------

locations2=locations[,c("id","longitude","latitude")]

air_temp1=weightdist_imp(x_data=air_temp,locations2=locations2)
RH1=weightdist_imp(x_data=RH,locations2=locations2)
rainfall1=weightdist_imp(x_data=rainfall,locations2=locations2)
wsp1=weightdist_imp(x_data=wsp,locations2=locations2)
wdir1=weightdist_imp(x_data=wdir,locations2=locations2)


# New plot
# Air temp
windows()
par(mfrow=c(4,4),mar=c(2,2,6,2))
for(i in 2:ncol(air_temp1)){
  plot(x=air_temp1$time,y=as.vector(unlist(air_temp1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(air_temp1[,i]))
  title(main=colnames(air_temp1)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# Relative humidity
windows()
par(mfrow=c(4,4),mar=c(2,2,6,2))
for(i in 2:ncol(RH1)){
  plot(x=RH1$time,y=as.vector(unlist(RH1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(RH1[,i]))
  title(main=colnames(RH1)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)


# Rainfall
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 2:36){
  plot(x=rainfall1$time,y=as.vector(unlist(rainfall1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(rainfall1[,i]))
  title(main=colnames(rainfall1)[i])
}
mtext("Rainfall (1)", side = 3, line = - 2, outer = TRUE)

# Plot for the next 36 stations
windows()
par(mfrow=c(4,9),mar=c(2,2,6,2))
for(i in 37:68){
  plot(x=rainfall1$time,y=as.vector(unlist(rainfall1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(rainfall1[,i]))
  title(main=colnames(rainfall1)[i])
}
mtext("Rainfall (2)", side = 3, line = - 2, outer = TRUE)


# Wind speed
windows()
par(mfrow=c(4,4),mar=c(2,2,6,2))
for(i in 2:ncol(wsp)){
  plot(x=wsp1$time,y=as.vector(unlist(wsp1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(wsp1[,i]))
  title(main=colnames(wsp1)[i])
}
mtext("Wind speed", side = 3, line = - 2, outer = TRUE)

# Wind direction
windows()
par(mfrow=c(4,4),mar=c(2,2,6,2))
for(i in 2:ncol(wdir1)){
  plot(x=wdir1$time,y=as.vector(unlist(wdir1[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(wdir1[,i]))
  title(main=colnames(wdir1)[i])
}
mtext("Wind direction", side = 3, line = - 2, outer = TRUE)

NA_count_air=count_gaps(air_temp1);NA_count_air

air_decomp=LOESS.df(air_temp1)
RH_decomp=LOESS.df(RH1)
rain_decomp=LOESS.df(rainfall1)

par(mfrow=c(3,1))
plot(rain_decomp$trend$time,rain_decomp$trend$S08,type='l')
plot(rain_decomp$season$time,rain_decomp$season$S08,type='l')
plot(rain_decomp$residuals$time,rain_decomp$residuals$S08,type='l')

wdir_decomp=LOESS.df(wdir1)

par(mfrow=c(3,1))
plot(wdir_decomp$trend$time,wdir_decomp$trend$S100,type='l')
plot(wdir_decomp$season$time,wdir_decomp$season$S100,type='l')
plot(wdir_decomp$residuals$time,wdir_decomp$residuals$S100,type='l')
