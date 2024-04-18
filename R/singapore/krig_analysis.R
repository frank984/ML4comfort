library(sp)
library(xts)
library(spacetime)
library(gstat)


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

# Change time zone to UTC+8 (Singapore)
Sys.setenv(TZ="UTC+8")
Sys.timezone()
Sys.getenv(x="TZ", unset=NA)
Sys.time()
air_stfdf@endTime[1]

head(air_stfdf[1,])


# Empirical variogram -----------------------------------------------------

air_stfdf1=air_stfdf[,1:200]
dim(air_stfdf1)

library(geosphere)
dstats=distm(stats, stats, fun = distHaversine)/1000
diag(dstats)=NA
mdstats=min(dstats,na.rm = T)
Mdstats=max(dstats,na.rm = T)

st=Sys.time()
vsta <- variogramST(air_temp ~ 1, air_stfdf1,
                    cutoff = mdstats/2,
                    width=3,
                    cores=3,tlags = 0:11)
en=Sys.time()
en-st

vsta

# default space-time variogram plot is the 2.5D plot
windows()
plot(vsta, xlab="separation (km)", ylab="separation (+hours)",
     main="Semivariance, air temperature")

# Second, parallel spatial variograms, one per time lag
windows()
plot(vsta, map = FALSE, xlab="separation (km)",
     ylab = "Semivariance, air temperature")

# Third, a 3D wireframe plot, showing both space and time
library(lattice)

windows()
plot(vsta, wireframe=TRUE)


# The two marginal variograms can be estimated by the variograms at lag
# 0 in the remaining dimension

(v.sp <- vsta[vsta$timelag==0,c("spacelag","gamma")])
(v.t <- vsta[vsta$spacelag==0,c("timelag","gamma")])

