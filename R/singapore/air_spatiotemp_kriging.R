library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(sp)
library(spacetime)
library(ozmaps)
library(sf)
library(ggmap)
library(gstat)

# Create object of class "sp"
#

# # Create spatial object
# sp = cbind(x = c(0,0,1), y = c(0,1,1))
# row.names(sp) = paste("point", 1:nrow(sp), sep="")
# library(sp)
# sp = SpatialPoints(sp)
# 
# # Then, the time points are defined 
# timeexample = as.POSIXct("2010-08-05", tz = "GMT")+3600*(10:13)
# 
# # Next, a data.frame with the data values is created:
# m = c(10,20,30) # means for each of the 3 point locations
# values = rnorm(length(sp)*length(timeexample), mean = rep(m, 4))
# IDs = paste("ID",1:length(values), sep = "_")
# mydata = data.frame(values = signif(values, 3)
#                     , ID=IDs
# )
# 
# library(spacetime)
# # time is of length m
# # data: data.frame with n*m rows corresponding to the observations
# stfdf = STFDF(sp, timeexample, data = mydata)

##### air temp data replicate #####
# Create spatial object

# extract subset of data
timelim=60
attach(air_temp)
prv_wide=air_temp%>%filter(time>=time[1] & time<=time[timelim])
detach(air_temp)

# check NAs
summary(prv_wide)
NAcount=apply(prv_wide,2,function(x){round(100*sum(is.na(x))/length(x),2)})
NAcount

# Drop stations with more than 50% of NAs
which(NAcount>=50)
st_dropped=names(prv_wide[,which(NAcount>=50)])
prv_wide2 = prv_wide[,!(names(prv_wide) %in% st_dropped)]

# Fill NAs for other stations
summary(prv_wide2)

# Let us assume these NAs are sparse (later we should check for it and find a proper solution)
NAcount2=apply(prv_wide2,2,function(x){round(100*sum(is.na(x))/length(x),2)})
st_impute=names(which(NAcount2>0))

# Count contiguous NAs (we want NAs to be sparse)
# If the following returns 0, the NAs are sparse (check later in mean_impute function)
apply(prv_wide2[st_impute],2,function(x){sum(diff(which(is.na(x)))==1)})

# If NAs are sparse, we impute them considering the mean of two (time)consecutive obs
mean_adiacent=function(x){
  if(is.na(x[1])){
    x[1]=x[2]
  }
  for(i in 2:(length(x)-1)){
    if(is.na(x[i])){
      x[i]=mean(x[c(i-1,i+1)])
    }
  }
  if(is.na(tail(x,1))){
    x[length(x)]=x[(length(x))-1]
  }
  return(x)
}

mean_impute=function(df){
  NAcount2=apply(df,2,function(x){round(100*sum(is.na(x))/length(x),2)})
  st_impute=names(which(NAcount2>0))
  if(all(apply(prv_wide2[st_impute],2,function(x){sum(diff(which(is.na(x)))==1)}))==0){
    newcols=apply(df[st_impute],2,mean_adiacent)
    #colnames(newcols)=st_impute
    df[st_impute]=newcols
    return(df)
  }
  else{
    return(c("No sparse NAs"))
  }
}

prv_wide3=mean_impute(prv_wide2)

summary(prv_wide3)

prv_wide3=subset(prv_wide3,select=-c(S117,S102))


# prv=stfdf_air[,"2022-10-10 00:01:00::2022-10-10 01:00:00"]


#### HERE SUMMARIZE (COMPLETE) DATA BASED ON HOURLY RESOLUTION

### Which stations recorded data for air_temp?
# indx=intersect(colnames(air_temp),locations$id)
indx=locations$id%in%colnames(prv_wide3)

# spatial locations of weather stations in singapore
singapore_sp =locations[indx,c("latitude","longitude")]
row.names(singapore_sp) = locations$id[indx]
singapore_sp = SpatialPoints(singapore_sp)

# Define time points
time=prv_wide3$time

# data.frame with values
air_t=as.vector(t(prv_wide3[,-1]))
airdata=data.frame(air=air_t,time=rep(time,each=ncol(prv_wide3[,-1])))

# time is of length m
# data: data.frame with n*m rows corresponding to the observations

stfdf_air = STFDF(singapore_sp, time, data = airdata)

# lst = lapply(1:length(unique(prv@data$time)), function(i) { x = prv[,i]; x$time = i; rownames(x@coords) = NULL; x} )
# air_pts = do.call(rbind, lst)
# 
# library(gstat)
# v = variogram(air~time, air_pts[!is.na(prv$air),], dX=0)
# 
# # and plot it
# 
# vmod = fit.variogram(v, vgm(psill=100, model="Exp", range=200))
# plot(v, vmod)

vv = variogram(air~1, stfdf_air, width=20, 
               cutoff = 200,tlags=0:5
               )
plot(vv)
data(vv)

# At first, we try to fit a metric model with spatio-temporal anisotropy

metricVgm <- vgmST("metric",
                   joint=vgm(50,"Exp",100,0),
                   stAni=50)
metricVgm <- fit.StVariogram(vv, metricVgm)
attr(metricVgm, "optim")$value

plot(vv, metricVgm)

# Now, let us try to fit and plot a separable model

sepVgm <- vgmST("separable",
                space=vgm(0.9,"Exp", 123, 0.1),
                time =vgm(0.9,"Exp", 2.9, 0.1),
                sill=100)

sepVgm <- fit.StVariogram(vv, sepVgm, method = "L-BFGS-B",
                          lower = c(10,0,0.01,0,1),
                          upper = c(500,1,20,1,200))

attr(sepVgm, "optim")$value

plot(vv, list(sepVgm, metricVgm))

## ST kriging

# range of the spatial grid
stfdf_air@sp@bbox

x1 <- seq(from=1.25,to=1.45,by=.01)
x2 <- seq(from=103.61,to=103.99,by=.01)

singapore_gridded <- SpatialPoints(cbind(rep(x1,length(x2)), rep(x2,each=length(x1))), 
                            proj4string=CRS(proj4string(stfdf_air@sp)))

gridded(singapore_gridded) <- TRUE

singapore_pred <- STF(sp=as(singapore_gridded,"SpatialPoints"), time=stfdf_air@time)

singapore_kriged <- krigeST(air~1, data=stfdf_air, newdata=singapore_pred,
                     modelList=sepVgm)

gridded(singapore_kriged@sp) <- TRUE
stplot(singapore_kriged)

dim(singapore_kriged[,"2022-10-10 00:01:00"])
plot(singapore_kriged[,"2022-10-10 01:00:00"])

singapore_kriged[,"2022-10-10 00:01:00"] %>% as.data.frame %>%
  ggplot(aes(x=coords.x1, y=coords.x2)) + geom_tile(aes(fill=var1.pred)) + 
  #coord_equal() +
  labs(x="latitude",y="longitude")+
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  geom_point(data=air_loc,aes(x=latitude,y=longitude))+
  # geom_sf(data=world,aes(x=label_x,y=label_y))+
  # coord_sf(xlim = c(103.5, 104), ylim = c(1.24,1.45), expand = FALSE)+
  theme_bw()


## Better data visualization (work in progress)
df_plotkrige=as.data.frame(singapore_kriged[,"2022-10-10 00:01:00"])
colnames(df_plotkrige)=c("var1.pred","label_x","label_y")
ggplot(data = world) +
  geom_sf() +
  theme(panel.background = element_blank()) +
  coord_sf(xlim = c(103.5, 104), ylim = c(1.24,1.45), expand = FALSE)



ggplot()+
  geom_tile(data=df_plotkrige,
            aes(x=label_x, y=label_y,fill=var1.pred))+
  scale_fill_gradient(low = "yellow", high="red")+
  geom_point(data=air_loc,aes(x=latitude,y=longitude))

#
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
sites <- data.frame(longitude = c(103.6184, 103.98260), latitude = c(1.2500,1.44387))
ggplot(data = world2) +
  geom_sf() +
  geom_point(data = air_loc, aes(x = longitude, y = latitude), size = 2, 
             shape = 23, fill = "grey25") +
  coord_sf(xlim = c(103.5, 104), ylim = c(1.24,1.45), expand = FALSE)
  




