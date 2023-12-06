# Create object of class "sp"

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
#                     #, ID=IDs
# )
# 
# library(spacetime)
# # time is of length m
# # data: data.frame with n*m rows corresponding to the observations
# stfdf = STFDF(sp, timeexample, data = mydata)

##### air temp data replicate #####
# Create spatial object
# air_stations=intersect(colnames(air_temp),locations$id)

# only those elements present both in air_temp AND locations
# locations$id[locations$id%in%air_stations]
indx=locations$id[locations$id%in%air_stations]

singapore_sp =locations[locations$id%in%indx,c("latitude","longitude")]
row.names(singapore_sp) = indx
library(sp)
singapore_sp = SpatialPoints(singapore_sp)

# Define time points
time=air_temp$time

# data.frame with values
air_t=as.vector(t(air_temp[,-1]))
airdata=data.frame(air=air_t,time=rep(time,each=ncol(air_temp[,-1])))

library(spacetime)
# time is of length m
# data: data.frame with n*m rows corresponding to the observations

# CHECK ORDERING OF OBSERVATIONS TAKEN FROM airdata (I think it is made by column (time1 first, then time2...))
stfdf_air = STFDF(singapore_sp, time, data = airdata)

prv=stfdf_air[,"2022-10-10 00:02:00::2022-10-10 00:04:00"]

library(gstat)
v = variogram(air~time, prv[!is.na(prv$air),], dX=0)

vv = variogram(air~1, stfdf_air, width=20, 
               cutoff = 200, 
               tlags=0:1)

# and plot it

vmod = fit.variogram(v, vgm(100, "Exp", 200))
plot(v, vmod)
