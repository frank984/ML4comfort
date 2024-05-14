library(sp)
library(xts)
library(spacetime)
library(gstat)

# Change time zone to UTC+8 (Singapore)
Sys.setenv(TZ="Singapore")
Sys.timezone()
Sys.getenv(x="TZ", unset=NA)
Sys.time()
options(xts_check_TZ = FALSE)

# Load data
load("air_short.Rdata")

# NAs frequency for each col in air_short
na_count=apply(air_short[,-1],2,function(x) sum(is.na(x))/length(x))
na_count*10

# Preliminary (technical) steps -------------------------------------------

# Create STFDF object for air temperature data

# First, create SpatialPoints object for stations
load("locations.Rdata")
locations<- locations[,c("id","longitude","latitude")]
# Only stations in air_short
stats<- locations[locations$id %in% colnames(air_short)[-1],]
rownames(stats)=stats[,1]
stats<- stats[,-1]
stats=SpatialPoints(stats)

# Data must have times in columns and stations in rows
tair=t(air_short[,-1])

air_stfdf<- STFDF(stats, air_short$time, data.frame(air_temp = as.vector(tair)))

str(air_stfdf)

air_stfdf@endTime[1]


head(air_stfdf[1,])


# Empirical variogram -----------------------------------------------------

air_stfdf1=air_stfdf[,1:200]
dim(air_stfdf1)

library(geosphere)
# dstats=distm(stats, stats, fun = distHaversine)/1000
# diag(dstats)=NA
# mdstats=min(dstats,na.rm = T)
# Mdstats=max(dstats,na.rm = T)
# median(dstats,na.rm = T)

st=Sys.time()
vsta <- variogramST(air_temp ~ 1, air_stfdf1,
                    #cutoff = Mdstats,
                    #width=.01,
                    cores=3,tlags = 0:11,
                    na.omit = T)
en=Sys.time()
en-st

vsta

# default space-time variogram plot is the 2.5D plot
windows()
plot(vsta,
     ylab="separation (+hours)",
     main="Semivariance, air temperature")

# Second, parallel spatial variograms, one per time lag
windows()
plot(vsta, map = FALSE, 
     ylab = "Semivariance, air temperature")

# Third, a 3D wireframe plot, showing both space and time
library(lattice)

windows()
plot(vsta, wireframe=TRUE)


# The two marginal variograms can be estimated by the variograms at lag
# 0 in the remaining dimension

v.sp <- vsta[vsta$timelag==0,c("spacelag","gamma")]
v.t <- vsta[vsta$spacelag==0,c("timelag","gamma")]

# Since exponential models are used, the range parameter is specified as
# 1/3 of the effective range as estimated from the marginal variogram, i.e.,
# where the semivariance reaches 95% of the estimated sill.

# In this case the variogram is not monotonic wrt space distance, so we take the maximum range possible
s.sill=.9*max(v.sp$gamma,na.rm = T)
s.range=max(v.sp$spacelag)
t.sill=.9*max(v.t$gamma,na.rm = T)
t.range=
  as.numeric(
    v.t$timelag[which(v.t$gamma>=.9*max(v.t$gamma,na.rm = T))[1]]
  )/3

#max(vsta$spacelag,na.rm = T)/as.numeric(max(vsta$timelag,na.rm = T))

estiStAni(vsta,c(0,100))

vgm.sum.metric <- vgmST(stModel="sumMetric",
                        space=vgm(s.sill,
                                  "Exp", s.range,0),
                        time=vgm(t.sill,
                                 "Exp", t.range, 0),
                        joint=vgm(s.sill,
                                  "Exp",s.range, 0),
                        stAni=0.01)

# We again bound the parameters;
# these are in the order: spatial sill, range, nugget; temporal sill, range,
# nugget; joint sill, range, nugget; joint anisotropy.

# See page 50 of Rossiter
(sp.sill.lb <- 0.7 * max(v.sp$gamma, na.rm=TRUE))
#(sp.range.lb <- v.sp[which(v.sp > sp.sill.lb)[1]-1, "spacelag"])
sp.range.lb=min(v.sp$spacelag)
(t.sill.lb <- 0.7 * max(v.t$gamma, na.rm=TRUE))
#(t.range.lb <- v.t[which(v.t$gamma > t.sill.lb)[1]-1, "timelag"])
t.range.lb=
  as.numeric(
    v.t$timelag[which(v.t$gamma>=.7*max(v.t$gamma,na.rm = T))[1]]
  )/3


st=Sys.time()
vgmf.sum.metric <-
  fit.StVariogram(vsta, vgm.sum.metric,
                  method="L-BFGS-B",
                  control=list(maxit=500),
                  lower=c(sp.sill.lb, sp.range.lb,0,
                          t.sill.lb, t.range.lb,0,
                          0,1,0,
                          1))
en=Sys.time()
elapsed_time=en-st; elapsed_time

round(attr(vgmf.sum.metric, "optim.output")$par,3)
attr(vgmf.sum.metric, "optim.output")$value

windows()
plot(vsta, vgmf.sum.metric)

windows()
plot(vsta, vgmf.sum.metric, map=FALSE)

# Prediction

pred.grid=data.frame(latitude=mean(air_stfdf1@sp$latitude),
                     longitude=mean(air_stfdf1@sp$longitude))

x_gridded <- SpatialPoints(pred.grid)

grid=STF(sp=x_gridded, time=air_stfdf1@time)

stkgr.sum.metric <- krigeST(air_temp~1, data=air_stfdf1, newdata=grid,
                           modelList=vgmf.sum.metric,
                           computeVar = T)

stkgr.sum.metric@data

#gridded(stkgr.sum.metric@sp) <- TRUE

# Wrap up in a function

STkriging<-function(dat,vgm.mod,pred.grid){
  # pred.grid=data.frame(latitude=mean(air_stfdf1@sp$latitude),
  #                      longitude=mean(air_stfdf1@sp$longitude))
  
  names(dat@data)=c("z")

  x_gridded <- SpatialPoints(pred.grid)
  
  grid=STF(sp=x_gridded, time=dat@time)
  
  stkgr <- krigeST(z~1, data=dat, newdata=grid,
                              modelList=vgm.mod,
                              computeVar = T)
  
  return(kgrST=stkgr)
}

# Test the function
prv=STkriging(air_stfdf1,vgmf.sum.metric,pred.grid)

# Function extracting all but one station
extract_station<-function(dat,station){
  dat1=dat[,!dat@sp$id %in% station]
  return(dat1)
}




