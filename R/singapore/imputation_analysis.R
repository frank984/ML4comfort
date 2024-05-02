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
tt=air_data_wide$time[2500];tt
tt=2500
tt_end=(tt+1414-1)
air_data_wide$time[c(tt,tt_end)]
Tnew=tt:(tt+1414-1)
air_short=air_data_wide[Tnew,]

na=apply(air_short,2,function(x) sum(is.na(x))/length(x));na*100

TT=dim(air_short)[1]
# Select only few stations, like S100, S104, S107, S108, S115, S116, S121, S43,S44, S50, S60
air_short=air_short[,c(1,which(colnames(air_short) %in% c("S100","S104","S107","S108","S115","S116","S121","S43","S44","S50","S60")))]


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
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_short)){
  plot(x=air_short$time,y=as.vector(unlist(air_short[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_short[,i]))
  title(main=colnames(air_short)[i])
}

#save(air_short,file="air_short.Rdata")
load("air_short.Rdata")
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

# na_start=rep(0,3)
# set.seed(1994)
# na_start[1]=sample(1:(TT-na_len[1]),1)
# na_start[2]=sample(1:(TT-na_len[2]),1)
# na_start[3]=sample(1:(TT-na_len[3]),1)
# na_start

na_start=rep(500,3)

air_5=air_short
air_10=air_short
air_20=air_short

air_5[na_start[1]:(na_start[1]+na_len[1]),2:ncol(air_5)]=NA
air_10[na_start[2]:(na_start[2]+na_len[2]),2:ncol(air_10)]=NA
air_20[na_start[3]:(na_start[3]+na_len[3]),2:ncol(air_20)]=NA

# Plot
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air_5[,i])),type="l",col="grey",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air_10[,i])),type="l",col="grey40",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
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

#save(air_5_sarima,air_10_sarima,air_20_sarima,file="air_NAs.Rdata")

# Plot results in red, and overlap dataset with missings in black
  windows()
  par(mfrow=c(4,3),mar=c(2,2,6,2))
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
  par(mfrow=c(4,3),mar=c(2,2,6,2))
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
  par(mfrow=c(4,3),mar=c(2,2,6,2))
  for(i in 2:ncol(air_20)){
    plot(x=air_20$time,y=as.vector(unlist(air_20_sarima[,i])),col="red"
      ,type="l",
         xlab=" ",ylab=" ",
         main=colnames(air_20[,i]))
    lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
    title(main=colnames(air_20)[i])
  }
  mtext("Air temperatures - 20% NAs - SARIMA", side = 3, line = - 2, outer = TRUE)


# 1-dim kriging -----------------------------------------------------------

temp_krige=function(x){
  x$knot=1
  x$indx=1:nrow(x)
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
par(mfrow=c(4,3),mar=c(2,2,6,2))
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
par(mfrow=c(4,3),mar=c(2,2,6,2))
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
par(mfrow=c(4,3),mar=c(2,2,6,2))
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

# Not feasible cause we should do kriging on each imputed dataset


# EM (Junger 2015) -------------------------------------------------------------



# SDEM and naive MA -------------------------------------------------------------------

MA_imp=function(x_data){
  
  # x_data is a data frame with time as first column. Each column is a station
  
  names(x_data)[1]="time"
  overall_mean=mean(as.matrix(x_data[,-1]),na.rm=T)
  
  #as.factor(weekdays(x_data$time))
  x_data$hour=as.factor(hour(x_data$time))
  #week(x_data$time)
  #month(x_data$time)
  
  xsw_bar=x_data%>%
    group_by(hour)%>%
    summarise_if(is.numeric,mean,na.rm=T)
  colnames(xsw_bar)[-1]=paste0(colnames(xsw_bar)[-1],"mean")
  
  xsw_bar2=merge(xsw_bar,x_data[,c("time","hour")],by="hour")
  imput.naive=xsw_bar2[order(xsw_bar2$time),]
  
  xsw=(xsw_bar[,-1]-rowMeans(xsw_bar[,-1]))/2
  xsw$hour=xsw_bar$hour
  
  xsw=merge(xsw,x_data[,c("time","hour")],by="hour")
  
  # sort by time
  xsw=xsw[order(xsw$time),]
  
  imput.SDEM=overall_mean+select(xsw,-c(time,hour))
  imput.SDEM=data.frame(time=xsw$time,imput.SDEM)
  
  x_data=subset(x_data,select=-hour)
  x_data.SDEM=x_data
  x_data.naive=x_data
  for(i in 2:ncol(x_data)){
    indx=which(is.na(x_data[,i]))
    x_data.SDEM[indx,i]=imput.SDEM[indx,i]
    x_data.naive[indx,i]=imput.naive[indx,i]
  }
  
  return(list(SDEM=x_data.SDEM,naive=x_data.naive))
  
}


air_sdem5=MA_imp(air_5)
air5_SDEM=air_sdem5$SDEM
air5_naive=air_sdem5$naive
# Plot results in red, and overlap dataset with missings in black
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air5_SDEM[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  lines(x=air_5$time,y=as.vector(unlist(air_5[,i])),col="black")
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs - SDEM", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air5_naive[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  lines(x=air_5$time,y=as.vector(unlist(air_5[,i])),col="black")
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs - Naive", side = 3, line = - 2, outer = TRUE)

air_sdem10=MA_imp(air_10)
air10_SDEM=air_sdem10$SDEM
air10_naive=air_sdem10$naive

# Plot results in red, and overlap dataset with missings in black
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air10_SDEM[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  lines(x=air_10$time,y=as.vector(unlist(air_10[,i])),col="black")
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs - SDEM", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air10_naive[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  lines(x=air_10$time,y=as.vector(unlist(air_10[,i])),col="black")
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs - Naive", side = 3, line = - 2, outer = TRUE)

air_sdem20=MA_imp(air_20)
air20_SDEM=air_sdem20$SDEM
air20_naive=air_sdem20$naive

# Plot results in red, and overlap dataset with missings in black
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air20_SDEM[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - SDEM", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air20_naive[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - Naive", side = 3, line = - 2, outer = TRUE)


# Comparison --------------------------------------------------------------

# RMSE

mean(unlist(as.vector(rmse(air_short,air_5_sarima))))
mean(unlist(as.vector(rmse(air_short,air_5_tkr))))
mean(unlist(as.vector(rmse(air_short,air_10_sarima))))
mean(unlist(as.vector(rmse(air_short,air_10_tkr))))
mean(unlist(as.vector(rmse(air_short,air_20_sarima))))
mean(unlist(as.vector(rmse(air_short,air_20_tkr))),na.rm = T)

mean(unlist(as.vector(rmse(air_short,air5_SDEM))))
mean(unlist(as.vector(rmse(air_short,air5_naive))))
mean(unlist(as.vector(rmse(air_short,air10_SDEM))))
mean(unlist(as.vector(rmse(air_short,air10_naive))))
mean(unlist(as.vector(rmse(air_short,air20_SDEM))))
mean(unlist(as.vector(rmse(air_short,air20_naive))))

# MABE
mabe(air_short,air_5_sarima)
mabe(air_short,air_5_tkr)
mabe(air_short,air_10_sarima)
mabe(air_short,air_10_tkr)
mabe(air_short,air_20_sarima)
mabe(air_short,air_20_tkr)


# The impact of preliminary trend and seasonal decomposition --------------


# 1) Decompose data with missing values to estimate trend and seas --------

# LOESS
LOESS.Decomp=function(tsx,sw=24,tw=6){
  # tsx is vector of observations
  
  # Transform tsx in a ts object, first turing it into a matrix
  tsx=ts(tsx,frequency = sw)
  
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

LOESS.df=function(data,sw=24,tw=6){
  
  # This function takes a data frame "data" with time as first column and returns a list with three data frames:
  # trend, level, and seasonal components
  # sw is the window for seasonal decomposition (default 24 hours)
  # tw is the window for trend decomposition (default 6 hours)
  
  colnames(data)[1]="time"
  trend=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  #level=data.frame()
  season=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  residuals=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  for(i in 2:ncol(data)){
    loess=LOESS.Decomp(data[,i],sw,tw)
    trend[,(i-1)]<-loess$trend
    #level[,(i-1)]<-loess$seasonal
    season[,(i-1)]<-loess$seasonal
    residuals[,(i-1)]<-loess$res
  }
  trend=data.frame(time=data$time,trend)
  #level=data.frame(time=data$time,level)
  season=data.frame(time=data$time,season)
  residuals=data.frame(time=data$time,residuals)
  colnames(trend)=colnames(data)
  #colnames(level)=colnames(data)
  colnames(season)=colnames(data)
  colnames(residuals)=colnames(data)
  
  return(list(trend=trend,season=season,residuals=residuals))
}

# Holt-Winters (better)
HoltWintersDecomposition <- function(x
                                     ,period=24
                                     ) {
  x=ts(x,frequency = period)
  xhw=HoltWinters(x)
  res=x-xhw$fitted[,"xhat"]
  return(list(HW=xhw,
              level=xhw$fitted[,"level"],
              trend=xhw$fitted[,"trend"],
              season=xhw$fitted[,"season"],
              residuals=res))
}


HoltWint.df=function(data,period=24){
  
  # This function takes a data frame "data" with time as first column and returns a list with four data frames:
  # trend, level, seasonal, and residuals components
  # period is the frequency of the time series
  
  # Remember that using Holt-Winter method we loose the first "period" observations
  
  colnames(data)[1]="time"
  time=data$time[-(1:period)]
  trend=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  level=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  season=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  residuals=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  for(i in 2:ncol(data)){
    hw=HoltWintersDecomposition(data[,i],period)
    trend[,(i-1)]<-hw$trend
    level[,(i-1)]<-hw$level
    season[,(i-1)]<-hw$season
    residuals[,(i-1)]<-hw$residuals
  }
  trend=data.frame(time=time,trend)
  level=data.frame(time=time,level)
  season=data.frame(time=time,season)
  residuals=data.frame(time=time,residuals)
  colnames(trend)=colnames(data)
  colnames(level)=colnames(data)
  colnames(season)=colnames(data)
  colnames(residuals)=colnames(data)
  
  return(list(trend=trend,level=level,season=season,residuals=residuals))
}

# 2) Apply LOESS and Holt-Winters decomposition to imputed data --------------

## LOESS
# air 5%
air5_loess_sarima=LOESS.df(air_5_sarima)
air5_loess_tkr=LOESS.df(air_5_tkr)
air5_loess_SDEM=LOESS.df(air5_SDEM)
air5_loess_naive=LOESS.df(air5_naive)
# air 10%
air10_loess_sarima=LOESS.df(air_10_sarima)
air10_loess_tkr=LOESS.df(air_10_tkr)
air10_loess_SDEM=LOESS.df(air10_SDEM)
air10_loess_naive=LOESS.df(air10_naive)
# air 20%
air20_loess_sarima=LOESS.df(air_20_sarima)
air20_loess_tkr=LOESS.df(air_20_tkr)
air20_loess_SDEM=LOESS.df(air20_SDEM)
air20_loess_naive=LOESS.df(air20_naive)

## HW
# air 5%
air5_hw_sarima=HoltWint.df(air_5_sarima,24)
air5_hw_tkr=HoltWint.df(air_5_tkr,24)
air5_hw_SDEM=HoltWint.df(air5_SDEM,24)
air5_hw_naive=HoltWint.df(air5_naive,24)
# air 10%
air10_hw_sarima=HoltWint.df(air_10_sarima,24)
air10_hw_tkr=HoltWint.df(air_10_tkr,24)
air10_hw_SDEM=HoltWint.df(air10_SDEM,24)
air10_hw_naive=HoltWint.df(air10_naive,24)
# air 20%
air20_hw_sarima=HoltWint.df(air_20_sarima,24)
####
#FAILED
air20_hw_tkr=HoltWint.df(air_20_tkr,24)
####
####
air20_hw_SDEM=HoltWint.df(air20_SDEM,24)
air20_hw_naive=HoltWint.df(air20_naive,24)

# Plot 
plot(air10_hw_tkr$level$S100,type='l')
plot(air20_hw_naive$level$S100,type='l')
plot(air20_hw_naive$residuals$S100,type='l')
