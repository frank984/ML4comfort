# save(air_temp1_sp.full,file="air_temp1_sp_full.Rdata")
# save(RH1_sp.full,file="RH1_sp_full.Rdata")
# save(rainfall1_sp.full,file="rainfall1_sp_full.Rdata")
# save(wind_dir1_sp.full,file="wind_dir1_sp_full.Rdata")
# save(wind_speed1_sp.full,file="wind_speed1_sp_full.Rdata")

load("air_temp1_sp_full.Rdata")
load("RH1_sp_full.Rdata")
load("rainfall1_sp_full.Rdata")
load("wind_dir1_sp_full.Rdata")
load("wind_speed1_sp_full.Rdata")
load("cleaned_data.RData")

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

# Spatial kriging

# Define spatial grid
# 
# Two crucial questions emerge:
#   
# 1) What range of values should we employ for latitude and longitude? 
#    Opting for the maximum and minimum values recorded by the Cozie app or the weather stations.
# 
# 2) What level of granularity should we adopt for latitude and longitude? 
#    I don't have a definitive answer to this query. 
#    Undoubtedly, higher granularity implies increased computational expenses.

lat_range_cozie=range(cozie_train$ws_latitude,na.rm=T)
lat_range_weath=range(locations$latitude,na.rm=T)
lon_range_cozie=range(cozie_train$ws_longitude,na.rm=T)
lon_range_weath=range(locations$longitude,na.rm=T)

LAT=c(min(lat_range_cozie[1],lat_range_weath[1]),
      max(lat_range_cozie[2],lat_range_weath[2]))

LON=c(min(lon_range_cozie[1],lon_range_weath[1]),
      max(lon_range_cozie[2],lon_range_weath[2]))


weather_STkrig=function(x,len.out=1000,LAT,LON){
  
  # x is a dataframe with no NAs
  # len.out is the grid length for latitude and longitude
  # LAT and LON are vectors, each with max e min values for the grids
  
  # Spatial locations of weather stations in singapore
  indx_x=locations$id%in%colnames(x)
  singapore_sp_x =locations[indx_x,c("latitude","longitude")]
  row.names(singapore_sp_x) = locations$id[indx_x]
  singapore_sp_x = SpatialPoints(singapore_sp_x)
  
  # Define time points
  time=x$time
  
  # Define long data.frame with air temperature values
  # time is of length m, then
  # data is a data.frame with n*m rows corresponding to the observations
  
  x_t=as.vector(t(x[,-1]))
  xdata=data.frame(vr=x_t,time=rep(time,each=ncol(x[,-1])))
  
  stfdf_x = STFDF(singapore_sp_x, time, data = xdata)
  
  # Fit empirical variogram
  vv = variogram(vr~1, stfdf_x, 
                 #width=20, 
                 #cutoff = 200,
                 #tlags=0:5
  )
  # At first, we try to fit a metric model 
  metricVgm <- vgmST("metric",
                     joint=vgm(100,"Exp",100,0),
                     stAni=10)
  metricVgm <- fit.StVariogram(vv, metricVgm)
  
  #preliminary choice
  grid_lat=seq(LAT[1],LAT[2],length.out=len.out)
  grid_lon=seq(LON[1],LON[2],length.out=len.out)
  
  space_gridded <- SpatialPoints(cbind(rep(grid_lat,length(grid_lat)), 
                                           rep(grid_lon,each=length(grid_lon))), 
                                     proj4string=CRS(proj4string(stfdf_x@sp)))
  
  gridded(space_gridded) <- TRUE
  
  space_pred <- STF(sp=as(space_gridded,"SpatialPoints"), time=stfdf_x@time)
  
  start=Sys.time()
  space_kriged <- krigeST(vr~1, data=stfdf_x, newdata=space_pred,
                              modelList=metricVgm)
  end=Sys.time()
  total_time=end-start
  return(list(space_kriged=space_kriged,
              total_time=total_time
              ))
  
}


#attr(metricVgm, "optim")$value
#plot(vv, metricVgm)

# # Now, let us try to fit and plot a separable model
# sepVgm <- vgmST("separable",
#                 space=vgm(0.9,"Exp", 123, 0.1),
#                 time =vgm(0.9,"Exp", 2.9, 0.1),
#                 sill=100)
# 
# sepVgm <- fit.StVariogram(vv, sepVgm, method = "L-BFGS-B"
#                           # ,
#                           # lower = c(10,0,0.01,0,1),
#                           # upper = c(500,1,20,1,200)
# )
# 
# attr(sepVgm, "optim")$value

# Overwhelming outputs! Switching to spatial kriging

air1_STKRIG=weather_STkrig(air_temp1_sp.full,len.out=1000,LAT,LON)
save(air1_STKRIG,file="air1_STKRIG.Rdata")
RH1_STKRIG=weather_STkrig(RH1_sp.full,len.out=1000,LAT,LON)
save(RH1_STKRIG,file="RH1_STKRIG,Rdata")
rainfall1_STKRIG=weather_STkrig(rainfall1_sp.full,len.out=1000,LAT,LON)
save(rainfall1_STKRIG,file="rainfall1_STKRIG.Rdata")
wind_dir1_STKRIG=weather_STkrig(wind_dir1_sp.full,len.out=1000,LAT,LON)
save(wind_dir1_STKRIG,file="wind_dir1_STKRIG.Rdata")
wind_speed1_STKRIG=weather_STkrig(wind_speed1_sp.full,len.out=1000,LAT,LON)
save(wind_speed1_STKRIG,file="wind_speed1_STKRIG.Rdata")


