# We will work with a data set with air quality (PM10) 
# measurements over germany, taken from rural background 
# stations available in the data sets provided
# by the European Environmental Agency
# source: https://cran.r-project.org/web/packages/gstat/vignettes/st.pdf

####

library(spacetime)
rm(list = ls())
data(air)
ls()

# We will look into a subset of the data, ranging from 2005 to 2010, and remove
# stations that have only missing values in this period

if (!exists("rural"))rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
rr = rural[,"2005::2010"]
unsel = which(apply(as(rr, "xts"), 2, function(x) all(is.na(x))))
r5to10 = rr[-unsel,]
summary(r5to10)
class(r5to10)

# Next, we will (rather arbitrarily) select four stations, 
# which have the following labels

rn = row.names(r5to10@sp)[4:7]

# we will sample 100 time instances randomly
rs = sample(dim(r5to10)[2], 100)

# we select these instances as a SpatialPointsDataFrame and add a time index
# to them. After this we bind them together in a single SpatialPointsDataFrame
# which has a time index ti

lst = lapply(rs, function(i) { x = r5to10[,i]; x$ti = i; rownames(x@coords) = NULL; x} )
pts = do.call(rbind, lst)

# Then, we can compute the pooled variogram

library(gstat)
v = variogram(PM10~ti, pts[!is.na(pts$PM10),], dX=0)

# and plot it

vmod = fit.variogram(v, vgm(100, "Exp", 200))
plot(v, vmod)

vmod

# We can fit a spatio-temporal variogram the usual way, by passing an object
# of class STFDF

vv = variogram(PM10~1, r5to10, width=20, 
               cutoff = 200, 
               tlags=0:5)
data(vv)
plot(vv)
plot(vv, map = FALSE)

# At first, we try to fit a metric model with spatio-temporal anisotropy

metricVgm <- vgmST("metric",
                   joint=vgm(50,"Exp",100,0),
                   stAni=50)
metricVgm <- fit.StVariogram(vv, metricVgm)

# As numerical criterion to judge the goodness of fit of model and sample variogram, 
# the root-mean-squared-difference between the surfaces can be obtained by

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

# RMS differences between between the two surfaces

attr(sepVgm, "optim")$value

plot(vv, list(sepVgm, metricVgm))

library(lattice)
plot(vv, list(sepVgm, metricVgm), all=T, wireframe=T, zlim=c(0,120),
       zlab=NULL,
       xlab=list("distance (km)", rot=30),
       ylab=list("time lag (days)", rot=-35),
       scales=list(arrows=F, z = list(distance = 5)))


#################
library(sp)
library(spacetime)

# Data
data(air)
suppressWarnings(proj4string(stations) <- CRS(proj4string(stations)))
rural = STFDF(stations, dates, data.frame(PM10 = as.vector(air)))
rr <- rural[,"2005-06-01/2005-06-03"]

#
rr <- as(rr,"STSDF")
class(rr) 
#"STSDF"


# Model definition
sumMetricVgm <- vgmST("sumMetric",
                      space = vgm( 4.4, "Lin", 196.6,  3),
                      time  = vgm( 2.2, "Lin",   1.1,  2),
                      joint = vgm(34.6, "Exp", 136.6, 12),
                      stAni = 51.7)



x1 <- seq(from=6,to=15,by=1)
x2 <- seq(from=48,to=55,by=1)

DE_gridded <- SpatialPoints(cbind(rep(x1,length(x2)), rep(x2,each=length(x1))), 
                            proj4string=CRS(proj4string(rr@sp)))
gridded(DE_gridded) <- TRUE
DE_pred <- STF(sp=as(DE_gridded,"SpatialPoints"), time=rr@time)

DE_kriged <- krigeST(PM10~1, data=rr, newdata=DE_pred,
                     modelList=sumMetricVgm)

gridded(DE_kriged@sp) <- TRUE
stplot(DE_kriged)
