# Strategy 1
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
load("weather_data_long.Rdata")

# Select time, station, air temp
air_data_long <- weather_data[, c("time", "station", "air_temperature")]

# Wide format with dplyr
air_data_wide <- air_data_long %>%
  spread(station, air_temperature)

# Plot
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(air_data_wide)){
  plot(x=air_data_wide$time,y=as.vector(unlist(air_data_wide[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_data_wide[,i]))
  title(main=colnames(air_data_wide)[i])
}
mtext("Air temperatures", side = 3, line = - 2, outer = TRUE)

# boxplot of air temperature
windows()
boxplot(air_data_wide[,2:ncol(air_data_wide)],las=2,ylab="Air temperature (°C)",xlab="Station")

# % of NAs
na=apply(air_data_wide,2,function(x) sum(is.na(x))/length(x));na*100

# Function to count consecutive gaps
count_gaps=function(x){
  gaps=list()
  gaps[[1]]=x[,1]
  for(i in 2:ncol(x)){
    d_na <- as.numeric(is.na(x[,i]))
    Nal=rle(d_na)
    tosave=Nal$values==1
    Nals=Nal$lengths[tosave]
    gaps[[i]]=Nals
  }
  names(gaps)=colnames(x)
  return(gaps)
}

# Gap lengths for air_data_wide as % of total length
NA_count=count_gaps(air_data_wide)
lapply(NA_count[-1],function(x) round(x/dim(air_data_wide)[1]*100,1))

# Save only few stations for a given time window --------------------------

# Shorter time window
Tnew=3409:6408
air_short=air_data_wide[Tnew,]
TT=dim(air_short)[1]
# Select only few stations, like S50, S60, S44, S43, S24, S104, S107, S111
air_short=subset(air_short,select=c("time","S50","S60","S44","S43","S24","S104","S107","S111"))

# NA 
na=apply(air_short,2,function(x) sum(is.na(x))/length(x));na*100


# Count NAs in air_short
Na_count=count_gaps(air_short)
Na_count

fill_gaps=function(x,period){
  for(i in 2:ncol(x)){
    fit_air=arima(x=x[,i], order=c(1,0,1),
                  seasonal = list(order=c(1,0,1)
                                  ,period=period
                                  ,include.mean =T
                                  ,method="ML"
                  ))
    kr_sm=KalmanSmooth(as.numeric(unlist(x[,i])),
                       fit_air$model)
    id.na <- which(is.na(x[,i]))
    y_sm=as.numeric(unlist(x[,i]))
    for (j in id.na){
      y_sm[j] <- kr_sm$smooth[j,1]
    }
    x[,i]=y_sm
  }
  
  return(x)
  
}

# Fill small gaps
air_short=fill_gaps(air_short,period = 24)

# Plot
windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_short)){
  plot(x=air_short$time,y=as.vector(unlist(air_short[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_short[,i]))
  title(main=colnames(air_short)[i])
}

#save(air_short,file="air_short.Rdata")
load("locations.Rdata")

# Long format with dplyr
air_data_long <- air_short %>%
  gather(key = "station", value = "air_temperature", -time)

# Extract latitude an longitude of stations in air_short from locations
locations_short=locations[locations$id %in% colnames(air_short),]

# Map locations of stations in air_short
map=leaflet() %>% addTiles() %>%
  addCircleMarkers(data=locations_short,
                   radius = 4,
                   color = 'red',
                   stroke = FALSE, fillOpacity = 1,
                   popup = ~paste("<br>Lat:",latitude, "<br>Lon:", longitude
                                  , "<br>Station:", id
                   )
  )

map


# NA imputation -----------------------------------------------------------

# We artificially introduce NAs for a randomly select time window long 5, 10, and 20% observations of the original time series length
TT=dim(air_short)[1]
na_len=TT*c(.05,.1,.2)

na_start=rep(0,3)
set.seed(1)
na_start[1]=sample(1:(TT-na_len[1]),1)
na_start[2]=sample(1:(TT-na_len[2]),1)
na_start[3]=sample(1:(TT-na_len[3]),1)
na_start

air_5=air_short
air_10=air_short
air_20=air_short

air_5[na_start[1]:(na_start[1]+na_len[1]),2:ncol(air_5)]=NA
air_10[na_start[2]:(na_start[2]+na_len[2]),2:ncol(air_10)]=NA
air_20[na_start[3]:(na_start[3]+na_len[3]),2:ncol(air_20)]=NA

# Plot
windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air_5[,i])),type="l",col="grey",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air_10[,i])),type="l",col="grey40",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air_20[,i])),type="l",col="grey20",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs", side = 3, line = - 2, outer = TRUE)

save(air_5,air_10,air_20,file="air_NAs.Rdata")


# Imputation --------------------------------------------------------------
load("air_NAs.Rdata")
load("air_short.Rdata")

# Performance evaluation

# RMSE
rmse=function(x,y){
  # x is the true data, y is the imputed data
  # For both, the first column is time
  
  D=dim(x)[2]-1
  RMSEs=data.frame(matrix(0,ncol=D,nrow=1))
  names(RMSEs)=colnames(x)[-1]
  
  for(i in 1:D){
      RMSEs[i]=sqrt(mean((x[,(i+1)]-y[,(i+1)])^2))
  }
  
  return(RMSEs)
}

# Mean absolute bias error
mabe=function(x,y){
  # x is the true data, y is the imputed data
  # For both, the first column is time
  
  D=dim(x)[2]-1
  MABEs=data.frame(matrix(0,ncol=D,nrow=1))
  names(MABEs)=colnames(x)[-1]
  
  for(i in 1:D){
    MABEs[i]=mean(abs(x[,(i+1)]-y[,(i+1)]))
  }
  
  return(MABEs)
}

# SARIMA ------------------------------------------------------------------

fill_sarima=function(x,period){
  for(i in 2:ncol(x)){
    fit_air=arima(x=x[,i], order=c(1,0,1),
                  seasonal = list(order=c(1,0,1)
                                  ,period=period
                                  ,include.mean =T
                                  ,method="ML"
                  ))
    kr_sm=KalmanSmooth(as.numeric(unlist(x[,i])),
                       fit_air$model)
    id.na <- which(is.na(x[,i]))
    y_sm=as.numeric(unlist(x[,i]))
    for (j in id.na){
      y_sm[j] <- kr_sm$smooth[j,1]
    }
    x[,i]=y_sm
  }
  
  return(x)
  
}

air_5_sarima=fill_sarima(air_5,period = 24)
air_10_sarima=fill_sarima(air_10,period = 24)
air_20_sarima=fill_sarima(air_20,period = 24)

# Plot results in red, and overlap dataset with missings in black
windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air_5_sarima[,i])),col="red"
    ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  lines(x=air_5$time,y=as.vector(unlist(air_5[,i])),col="black")
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs - SARIMA", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air_10_sarima[,i])),col="red"
    ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  lines(x=air_10$time,y=as.vector(unlist(air_10[,i])),col="black")
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs - SARIMA", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air_20_sarima[,i])),col="red"
    ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - SARIMA", side = 3, line = - 2, outer = TRUE)

# RMSE
rmse(air_short,air_5_sarima)
rmse(air_short,air_10_sarima)
rmse(air_short,air_20_sarima)

# MABE
mabe(air_short,air_5_sarima)
mabe(air_short,air_10_sarima)
mabe(air_short,air_20_sarima)


# 1-dim kriging -----------------------------------------------------------

temp_krige=function(x){
  x_sp=x
  
  temp_grid=1:nrow(x)
  temp_grid=expand.grid(temp_grid,1)
  colnames(temp_grid)=c("indx","knot")
  gridded(temp_grid)=~indx+knot
  
  for(i in 2:ncol(x)){
    #i=17
    x_sp=x[,c(1,i)]
    ind_fill=which(is.na(x_sp[,2]))
    NAcount=sum(is.na(x_sp[,2]))
    
    if(NAcount>0){
      colnames(x_sp)=c("time","z")
      x_sp$indx=1:nrow(x_sp)
      x_sp$knot=1
      x_sp=drop_na(x_sp)
      coordinates(x_sp) <- ~ indx + knot
      
      onedim_krig = autoKrige(z~1, x_sp,temp_grid)
      x[ind_fill,i]=onedim_krig$krige_output$var1.pred[ind_fill]
    }
  }
  return(x)
}

air_5_tkr=air_5
air_5_tkr$knot=1
air_5_tkr$indx=1:nrow(air_5_tkr)
air_5_tkr=temp_krige(air_5_tkr)

air_10_tkr=air_10
air_10_tkr$knot=1
air_10_tkr$indx=1:nrow(air_10_tkr)
air_10_tkr=temp_krige(air_10_tkr)

air_20_tkr=air_20
air_20_tkr$knot=1
air_20_tkr$indx=1:nrow(air_20_tkr)
air_20_tkr=temp_krige(air_20_tkr)


# Plot as above
windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air_5_tkr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  lines(x=air_5$time,y=as.vector(unlist(air_5[,i])),col="black")
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs - temporal kriging", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air_10_tkr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  lines(x=air_10$time,y=as.vector(unlist(air_10[,i])),col="black")
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs - temporal kriging", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(3,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air_20_tkr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - temporal kriging", side = 3, line = - 2, outer = TRUE)

# Multiple imputation -----------------------------------------------------


# Other -------------------------------------------------------------------



# Trend and seasonal components -------------------------------------------

decompose_ts=function(tsx,sw,tw,start){
  # tsx is vector of observations
  
  # Transform tsx in a ts object, first turing it into a matrix
  tsx=ts(tsx,
         start=start,
         frequency = sw)
  
  ts.stl <- stl(tsx, 
                s.window=sw,
                t.window=tw,
                na.action = na.approx,
                robust=T)
  res=ts.stl$time.series[,"remainder"]
  trend=ts.stl$time.series[,"trend"]
  seasonal=ts.stl$time.series[,"seasonal"]
  return(list(ts.stl=ts.stl,res=res,trend=trend,seasonal=seasonal))
}

# Shorter time window
Tnew=250
air_data_short=air_data_wide[1:250,]

# Apply to each station 
sw=24
tw=6
air_data_wide_decomp=list()
for(i in 2:ncol(air_data_wide)){
  air_data_wide_decomp[[i-1]]=decompose_ts(air_data_short[,i],sw,tw)
}

# Extract length of residuals for each element of the list
unlist(lapply(air_data_wide_decomp,function(x) length(x$res)))

plot(air_data_wide_decomp[[4]]$ts.stl)

# Plot  residuals
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 1:length(air_data_wide_decomp)){
  plot(x=air_data_short$time,y=air_data_wide_decomp[[i]]$res,type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_data_wide)[i+1])
}
mtext("Residuals", side = 3, line = - 2, outer = TRUE)

windows()
plot(air_data_wide_decomp[[1]]$ts.stl)
plot(air_data_wide_decomp[[1]]$res,type='l')

1-length(air_data_wide_decomp[[1]]$res)/dim(air_data_wide)[1]
