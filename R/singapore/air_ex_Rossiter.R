
# Load data and packages --------------------------------------------------
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)

library(ggplot2)
library(ggpubr)
library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(htmltools)

library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
#library(ggmap)
library(gstat)
library(automap)

library(xts)

load("air_temp3.RData")
load("locations.Rdata")
load("cleaned_data.RData")


# NA analysis -------------------------------------------------------------

# Percentage of NA for each station
na=apply(air_temp3[,-1],2,function(x){sum(is.na(x))/length(x)}); na*100

# Plot stations in air_temp
air_stat=locations$id%in%colnames(air_temp3)
air_stat=locations[locations$id[air_stat],]

# Plot participants locations
participants_loc=cozie_train[,c("ws_latitude","ws_longitude","id_participant")]
participants_loc=participants_loc[complete.cases(participants_loc),]
names(participants_loc)[1:2]=c("latitude","longitude")

# On the same map, plot small black circles for participants_loc and bigger red circles for air_stat
map=leaflet() %>% addTiles() %>%
  addCircleMarkers(data=participants_loc,
                   radius = 2,
                   color = 'black',
                   stroke = FALSE, fillOpacity = 1,
                   popup = ~paste("<br>Lat:",latitude, "<br>Lon:", longitude
                                  , "<br>Participant:", id_participant
                   )
  ) %>%
  addCircleMarkers(data=air_stat,
                   radius = 4,
                   color = 'red',
                   stroke = FALSE, fillOpacity = 1,
                   popup = ~paste("<br>Lat:",latitude, "<br>Lon:", longitude
                                  , "<br>Station:", id
                   )
  )

map=map %>% addLegend("bottomright", 
                      colors = c("red","black"), 
                      labels = c("Stations","Participants"), title = "Legend")

map

# Remove stations with more than 40% of missing (S102, S117)
na*100
air_temp3=subset(air_temp3,select=-c(S102,S117))


# Data preprocessing ------------------------------------------------------

air_temp_long=air_temp3 %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temperature")
names(locations)[2]="station"

# Merge the two dataframes
air_temp_long=merge(air_temp_long, locations, by="station")

air_temp_long=air_temp_long[order(air_temp_long$time),]
air_temp_long=subset(air_temp_long,select = -c(X,name))

# Create spacetime object
air_temp_st=stConstruct(air_temp_long, 
                        time="time", 
                        space=c("longitude","latitude"))


#proj4string(air_temp_st) <- CRS("+init=EPSG:4269")
str(air_temp_st)
# this is of class STIDF, “unstructured spatio-temporal data”,

# If we assume that every spatial record has a time seriesof data of length nrow(time)
# then consider the following object

air_temp_st=as(air_temp_st,"STFDF")
str(air_temp_st)
summary(air_temp_st)
#save(air_temp_st,file="air_temp_st.Rdata")
load("air_temp_st.Rdata")
length(unique(air_temp_st@data$station))
unique(air_temp_st@data$station)

