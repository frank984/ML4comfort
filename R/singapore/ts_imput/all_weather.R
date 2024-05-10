load("cleaned_data.RData")
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

weather_summ=function(x,window="1 hour"){
  
  mean_x=x %>% group_by(time=floor_date(time,window))%>%
    summarise_if(is.numeric, mean, na.rm = TRUE)
  mean_x[,-1]=mean_x[,-1]%>% mutate_all(~ifelse(is.nan(.), NA, .))
  
  
  return(mean_x)
  
}

air_temp=weather_summ(air_temp,wdn)
RH=weather_summ(RH,wdn)
rain=weather_summ(rainfall,wdn)
wsp=weather_summ(wind_speed,wdn)
wdir=weather_summ(wind_dir,wdn)


#  NA counts --------------------------------------------------------------

na_count=function(x){
  x %>% summarise_all(funs(sum(is.na(.))))
}

# Drop stations with more than 40%  of missings

air_temp_na=na_count(air_temp); air_temp_na=air_temp_na/dim(air_temp)[1]*100;air_temp_na
air_temp=air_temp[,-which(air_temp_na>40)]

RH_na=na_count(RH); RH_na=RH_na/dim(RH)[1]*100
RH=RH[,-which(RH_na>40)]

rain_na=na_count(rainfall); rain_na=rain_na/dim(rainfall)[1]*100
rain=rain[,-which(rain_na>40)]

wsp_na=na_count(wind_speed); wsp_na=wsp_na/dim(wind_speed)[1]*100
wsp=wsp[,-which(wsp_na>40)]

wdir_na=na_count(wind_dir); wdir_na=wdir_na/dim(wind_dir)[1]*100
wdir=wdir[,-which(wdir_na>40)]


