
# Intro -------------------------------------------------------------------

# Load packages
library(sp)
library(xts)
library(spacetime)
library(gstat)

# Load data
# air quality in rural areas over much of Germany from the European Air
# Quality Database
data("air")
ls()
class(air)
str(air)
dim(air)
head(rownames(air))

summary(stations)
summary(DE_NUTS1)

# Update 
proj4string(stations)
proj4string(DE_NUTS1)
proj4string(stations) <- CRS(proj4string(stations))
proj4string(DE_NUTS1) <- CRS(proj4string(DE_NUTS1))
proj4string(stations)
proj4string(DE_NUTS1)


# STDF object -------------------------------------------------------------

# Combine the spatial and temporal information into one
# data structure

# The number of spatial points and dates must match the number of rows
# and columns, respectively, in the matrix

rural <- STFDF(stations, dates, data.frame(PM10 = as.vector(air)))

# Set the time zone for the R session in UTC cause dates in rural are expressed in UTC
Sys.setenv(TZ="UTC")
Sys.timezone()
Sys.getenv(x="TZ", unset=NA)
Sys.time()
rural@endTime[1]

# Long term analysis ------------------------------------------------------

# Find the longest time-series in the rural object
max.not.na <- 0; longest.station <- 1
for (station in 1:dim(rural)["space"]) {
  ts <- rural[station,][,"PM10"]
  (ix <- sum(!is.na(ts)))
  if (ix > max.not.na) { max.not.na <- ix; longest.station <- station }
}
longest.station.name <- row.names(rural@sp@coords)[longest.station]
print(paste0("Station ", longest.station, " (",
             longest.station.name,
             ") has ", max.not.na, " PM10 readings"))
long.ts <- rural[longest.station,]
rm(max.not.na, longest.station, station)

# Change plot language to english
Sys.setlocale(category = "LC_ALL", locale = "english")

# Display the time-series of PM10 at this station
windows()
spacetime::plot(long.ts$PM10, lwd=0.5,
                main=paste("PM10 at",longest.station.name))


# Restrict to the time period from 2005 to 2010 inclusive

# The [] matrix selection operator has been adapted for STFDF objects: the
# first dimension represents space, and the second time.

# In this first case all times are selected for the first location, so the result
# is a time series.
class(rural[1,])

# In this second case all locations are selected for the first
# time, so the result is a SpatialPointsDataFrame
class(rural[,1])

# The second (time) dimension accepts any valid POSIX date as a character
# string, so to select years we can specify the year range using the :: range
# operator

rr <- rural[, "2005::2010"]
class(rr)


# Remove stations that only have missing values ---------------------------

# rr as matrix (rows are times, columns are stations)
# as(rr, "xts")

(na.stations <- which(apply(as(rr, "xts"), 2, function(x) all(is.na(x)))))

r5to10 <- rr[-na.stations,]
rm(na.stations)
dim(r5to10)
dim(rr)

# Summary statistics-----------------------------------------------------------------

summary(r5to10)

station.ids <- attributes(r5to10@sp@coords)$dimnames[[1]]

# For each string, save characters from the 3rd to the 7th
station.ids <- substr(station.ids, start=3, stop=7)  


# Plot locations of each station along with its label ---------------------

(aspect.ratio <- cos(median(coordinates(r5to10@sp)[,2])))

windows()
plot(coordinates(r5to10@sp), pch=3, col="red",
     cex=0.5, asp=1/aspect.ratio,
     xlab="E", ylab="N")
grid()
text(coordinates(r5to10@sp), labels=station.ids, pos=4, cex=0.8)

# overlaying the boundary of Germany
# Country boundaries are provided by the worldHires dataset provided
# with the mapdata package

library(mapdata)
tmp <- map('worldHires', 'Germany', fill=TRUE, plot=FALSE)
# install.packages("maptools", repos = "https://packagemanager.posit.co/cran/2023-10-13")
library(maptools)
de.boundary <-
  map2SpatialPolygons(tmp, IDs=tmp$names,
                      proj4string=CRS(proj4string(rr@sp)))
plot(de.boundary, axes=T)
points(coordinates(r5to10@sp), pch=3, cex=0.5, col="red")
grid()
text(coordinates(r5to10@sp), labels=substr(station.ids,1,2), pos=4, cex=0.5)


# Local temporal structure -----------------------------------------------

# Select observations from a given region (NRW)
(ix <- (substr(station.ids, start=1, stop=2)=="NW"))

r5to10nrw <- r5to10[ix,]
dim(r5to10nrw)
(station.ids.nrw <- substr(station.ids[ix], 3, 5))
attributes(r5to10nrw@sp@coords)$dimnames[[1]] <- station.ids.nrw

# Autocorrelation
r5to10nrw.1.ts <- as.ts(na.omit(r5to10nrw[1,]))[,"PM10"]
acf(r5to10nrw.1.ts,
    main=paste("Autocorrelation, station", station.ids.nrw[1]))
acf(r5to10nrw.1.ts, plot=FALSE)
pacf(r5to10nrw.1.ts,plot = F)

# The partial autocorrelation is about +0.65 at a lag of +1 day; this is
# the same as the autocorrelation for lag +1. For longer lags it is not provable
# different from 0, i.e., all of the autocorrelation can be explained by the single
# previous days values.

# Compute and plot the direct and cross-correlations between
# the stations in NRW

windows()
acf(na.omit(as(r5to10nrw, "xts")), xlab="", ylab="")

# The lag-0 cross-correlations do not have to equal 1 (perfect correlation),
# and rarely do

# Compute the distances between the NRW stations

# Note that even though the coordinates are given in longitude and latitude,
# the distances are given in metric coördinates, namely km, because
# of the optional TRUE value of the longlat argument
print(spDists(r5to10nrw@sp, longlat=TRUE), digits=3)


# (Only)-spatial prediction -------------------------------------------------

# Anomaly detection
ix <- which.max(r5to10$PM10)
pm.max <- r5to10$PM10[ix]
station.id <- ix%%dim(r5to10)[1]
station.name <- attributes(r5to10@sp@coords)$dimnames[[1]][station.id]
station.xts <- r5to10[station.id,]
date.xts <- station.xts[which.max(station.xts$PM10)]
date.ix <- index(date.xts)
date.ix <- as.POSIXct(date.ix)
date.ix <- as(date.ix,"character")
r.date <- r5to10[,date.ix]
r.date <- r.date[-station.id,]
windows()
plot(coordinates(r.date),
     cex=3*r.date$PM10/max(na.omit(r.date$PM10)),
     col="red", asp=1/aspect.ratio, xlab="Longitude", ylab="Latitude",
     main=paste("PM10 on",date.ix))
text(coordinates(r.date), labels=round(r.date$PM10,1), pos=2)
grid()
date.ix <- as.POSIXct(date.ix)

# Fit a variogram --------------------------------------------------

# We use the variogram method of the gstat package
# This can not deal
# with missing values, so we first remove them.

sum(is.na(r.date$PM10))
vo <- variogram(PM10 ~ 1, r.date[!is.na(r.date$PM10),])
plot(vo, plot.numbers=T, main=paste("PM10 on",date.ix))
# plot.numbers=T adds the number of pairs to each bin
(vom <- fit.variogram(vo, model=vgm(50, "Exp", 300/3, 0)))
vom2 <- fit.variogram(vo, model=vgm(NA, "Exp", NA, NA)); vom2
plot(vo, plot.numbers=T, model=vom, main=paste("PM10 on",date.ix))

# Construct a grid for prediction -----------------------------------------

bbox(de.boundary)
x1 <- seq(from=5.75,to=15.25,by=0.25)
x2 <- seq(from=47.25,to=55.25,by=0.25)
de.bbox.grid <- SpatialPoints(cbind(rep(x1,length(x2)),
                                    rep(x2,each=length(x1))),
                              proj4string=CRS(proj4string(r5to10@sp)))
gridded(de.bbox.grid) <- TRUE
summary(de.bbox.grid)

de.grid <- de.bbox.grid[!is.na(over(de.bbox.grid, de.boundary)),]
summary(de.grid)

windows()
plot(de.boundary, axes=T)
points(coordinates(de.bbox.grid), pch=3)
points(coordinates(de.grid), pch=1, col="red")
points(coordinates(r5to10@sp), pch=21, bg="blue")
grid(lty=1, col="blue")


# Prediction --------------------------------------------------------------

proj4string(r.date) <- CRS(proj4string(r.date))
k.one <- krige(PM10 ~ 1, loc=r.date[!is.na(r.date$PM10),],
               newdata=de.grid, model=vom)
spplot(k.one, zcol="var1.pred", col.regions=bpy.colors(64),
       main=paste("PM10 on",date.ix),
       sub="single day variogram model")


# Spatio-temporal prediction ----------------------------------------------

# Missing values present in the following object. The function can somehow handle them
r5to10[,1:200]

# Too time-consuming, consider using a subset
vst <- variogramST(PM10 ~ 1, r5to10[,1:200])

# default space-time variogram plot is the 2.5D plot
windows()
plot(vst, xlab="separation (km)", ylab="separation (+days)",
     main="Semivariance, PM10")

# Second, parallel spatial variograms, one per time lag
windows()
plot(vst, map = FALSE, xlab="separation (km)",
     ylab = "Semivariance, PM10")

# Third, a 3D wireframe plot, showing both space and time
library(lattice)

windows()
plot(vst, wireframe=TRUE)


# Metric model ------------------------------------------------------------

# 1) METRIC. Simplest model: Time is considered as
# another dimension, which must be re-scaled to match the spatial
# dimension

# The critical parameter here is the time anisotropy.
# This is the ratio of the time scale to the spatial scale.

# One way to estimate this is to compute the anisotropy ratio (distance/time) 
# for all the variogram bins within a certain range of semivariances, and then summarize

dim(tmp <- vst[(vst$gamma > 70) &
                 (vst$gamma < 80) &
                 (vst$timelag !=0),])

summary(metric.aniso <- tmp$spacelag/as.numeric(tmp$timelag))

# Build metric model
# (vgm.metric <- vgmST(stModel="metric",joint=vgm(50,"Exp",100,0),nugget=10,stAni=mean(metric.aniso)))
(vgm.metric <- vgmST(stModel="metric",joint=vgm(50,"Exp",100,0),nugget=10,stAni=mean(metric.aniso)))
vgmf.metric <- fit.StVariogram(vst, vgm.metric,
                               method="L-BFGS-B",
                               control=list(maxit=1024))

# Plot
windows()
plot(vst, vgmf.metric)
windows()
plot(vst, vgmf.metric, map=FALSE)


# Separable model ---------------------------------------------------------

# 2)SEPARABLE. Separate spatial and temporal
# structures, which are considered to interact only multiplicatively
# and so can be fit independently but with a common sill.

# The overall sill can be estimated from the empirical variogram as some
# proportion of the maximum semivariance in the space-time variogram

(estimated.sill <- quantile(na.omit(vst$gamma), .8))

# The partial sill arguments in the space and time variograms are ignored
# in favour of the specified overall sill

(vgm.sep <- vgmST(stModel="separable",
                  space=vgm(0.9,"Exp", 300,0.1),
                  time=vgm(0.95,"Exp", 2, 0.05),
                  sill=estimated.sill))

# we have to ensure
# that the optim “optimize” function called by fit.StVariogram does not
# try to use un-physical values, in particular, negative ranges or semivariances.

vgmf.sep <- fit.StVariogram(vst, vgm.sep,
                            method="L-BFGS-B",
                            lower=c(100,0.001,1,0.001,40),
                            control=list(maxit=500))

attr(vgmf.sep, "optim.output")$par

# Value of the optimization function
attr(vgmf.sep, "optim.output")$value

# Plot
windows()
plot(vst, vgmf.sep)
windows()
plot(vst, vgmf.sep, map=FALSE)

# sumMetric model ---------------------------------------------------------

# 3) sumMETRIC. separate terms for the spatial
# and temporal components, and an interaction component for the
# residuals not accounted for by these two.

# The interaction term allows geometric anisotropy, i.e., same structure,
# nugget and partial sill but the range can vary in different dimensions
# (here, the time vs. space dimensions)

# 10 parameters:
# a) the spatial marginal variogram, 3 parameters: partial sill, nugget, range
# b) the temporal marginal variogram, 3 parameters: partial sill, nugget, range
# c) space-time residual variogram, 4 parameters: partial sill, nugget, range of residuals, anisotropy ratio (the ratio between the space and time and ranges)

# The two marginal variograms can be estimated by the variograms at lag
# 0 in the remaining dimension

(v.sp <- vst[vst$timelag==0,c("spacelag","gamma")])
(v.t <- vst[vst$spacelag==0,c("timelag","gamma")])

# Since exp models are used, the range is 1/3 of the effective range as estimated from the marginal variogram, i.e.,
# where the semivariance reaches 95% of the estimated sill
#(i.e. 240 km and 6 days)

# The anisotropy parameter stAni is estimated as the ratio of these two
# 240=6
# The gstat package version 1.0-25 and later now has a function
# estiStAni to estimate this parameter

# Inspecting empirical variogram we can find
# range at which there seems to be no more interaction between
# space and time, that is, where the single-time variograms have the
# same shape, but with different total sills

# We assume the nugget effects are in either space or time but not
# their interaction.

# The partial sill is estimated as the semivariance at zero
# time lag at this estimated range; here this appears to be about 30



vgm.sum.metric <- vgmST(stModel="sumMetric",
                        space=vgm(0.9*max(v.sp$gamma, na.rm=TRUE),
                                  "Exp", 250/3,0),
                        time=vgm(0.9*max(v.t$gamma, na.rm=TRUE),
                                 "Exp", 6/3, 0),
                        joint=vgm(30,
                                  "Exp",50/3, 0),
                        stAni=240/6)

# We again bound the parameters;
# these are in the order: spatial sill, range, nugget; temporal sill, range,
# nugget; joint sill, range, nugget; joint anisotropy.

# See page 50 of Rossiter
(sp.sill.lb <- 0.7 * max(v.sp$gamma, na.rm=TRUE))
(sp.range.lb <- v.sp[which(v.sp > sp.sill.lb)[1]-1, "spacelag"])
(t.sill.lb <- 0.7 * max(v.t$gamma, na.rm=TRUE))
(t.range.lb <- v.t[which(v.t$gamma > t.sill.lb)[1]-1, "timelag"])

system.time(vgmf.sum.metric <-
              fit.StVariogram(vst, vgm.sum.metric,
                              method="L-BFGS-B",
                              control=list(maxit=500),
                              lower=c(sp.sill.lb, sp.range.lb/3,0,
                                      t.sill.lb, t.range.lb/3,0,
                                      0,1,0,
                                      1)))

round(attr(vgmf.sum.metric, "optim.output")$par,3)

# Compare goodness-of-fit of the three models -----------------------------

attr(vgmf.metric, "optim.output")$value
attr(vgmf.sep, "optim.output")$value
attr(vgmf.sum.metric, "optim.output")$value


# Spatio-temporal kriging -------------------------------------------------

# We have a spatial grid; to do space-time kriging the grid must have both
# a space and time component

rr.t <- r5to10[,"2006-06-21/2006-06-26"]

windows()
stplot(rr.t, col.regions=bpy.colors(64),
       sp.layout=list(list("sp.polygons",de.boundary)))

# Create an interpolation grid over the entire bounding box,
# and just for the portion (partially) covered by Germany

rr.t <- as(rr.t,"STSDF")
de.bbox.grid <- STF(sp=as(de.bbox.grid,"SpatialPoints"), time=rr.t@time)
de.grid <- STF(sp=as(de.grid,"SpatialPoints"), time=rr.t@time)

# Predict over the 6-day space-time grid of Germany by kriging
# with the three fitted space-time models

k.de.sum.metric <- krigeST(PM10~1, data=rr.t, newdata=de.grid,
                           modelList=vgmf.sum.metric)
gridded(k.de.sum.metric@sp) <- TRUE

# Display resulting map
windows()
plot.zlim <- seq(floor(min(
  # k.de.met$var1.pred,
  #                          k.de.sep$var1.pred,
                           k.de.sum.metric$var1.pred)),
                 ceiling(max(
                   # k.de.met$var1.pred,
                   #           k.de.sep$var1.pred,
                             k.de.sum.metric$var1.pred)),
                 by = 0.5)
stplot(k.de.sum.metric, main="PM10, Germany",
       sub="Sum-metric space-time model",
       col.regions=bpy.colors(length(plot.zlim)),
       at=plot.zlim)


