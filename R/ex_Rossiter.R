
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

r5to10[,1:200]
vst <- variogramST(PM10 ~ 1, r5to10[,1:200])

