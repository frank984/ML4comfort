library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

load("weather_data_long.RData")

rh_long=weather_data[,c("time","station","relative_humidity")]

rh_wide=spread(rh_long,station,relative_humidity)

# Count NAs
rh_wide %>% summarise_all(~(sum(is.na(.))))/dim(rh_wide)[1]*100

#rh_short=rh_wide[]

# same for rh
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(rh_wide)){
  plot(x=rh_wide$time,y=as.vector(unlist(rh_wide[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rh_wide[,i]))
  title(main=colnames(rh_wide)[i])
}
mtext("Relative humidity", side = 3, line = - 2, outer = TRUE)

# boxplot of relative humidity
windows()
boxplot(rh_wide[,2:ncol(rh_wide)],las=2,ylab="Relative humidity (%)",xlab="Station")

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

# Gap lengths for rh_wide as % of total length
NA_count=count_gaps(rh_wide)
lapply(NA_count[-1],function(x) round(x/dim(rh_wide)[1]*100,1))

# Shorter time window
tt=rh_wide$time[2500];tt

tt=2500
tt_end=(tt+1414-1)
rh_wide$time[c(tt,tt_end)]
Tnew=tt:(tt+1414-1)
rh_short=rh_wide[Tnew,]

na=apply(rh_short,2,function(x) sum(is.na(x))/length(x));na*100

TT=dim(rh_short)[1]
# Select only few stations, like S100, S104, S107, S108, S115, S116, S121, S43,S44, S50, S60
rh_short=rh_short[,c(1,which(colnames(rh_short) %in% c("S100","S104","S107","S108","S115","S116","S121","S43","S44","S50","S60")))]

# NA 
na=apply(rh_short,2,function(x) sum(is.na(x))/length(x));na*100


# Count NAs in rh_short
Na_count=count_gaps(rh_short[,-1])
Na_count

# Plot
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(rh_short)){
  plot(x=rh_short$time,y=as.vector(unlist(rh_short[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rh_short[,i]))
  title(main=colnames(rh_short)[i])
}

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
rh_short=fill_gaps(rh_short,period = 24)

# Plot
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(rh_short)){
  plot(x=rh_short$time,y=as.vector(unlist(rh_short[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rh_short[,i]))
  title(main=colnames(rh_short)[i])
}

save(rh_short,file="rh_short.Rdata")


# Rainfall

rain_long=weather_data[,c("time","station","rainfall")]

rain_wide=spread(rain_long,station,rainfall)

# Count NAs
rain_wide %>% summarise_all(~(sum(is.na(.))))/dim(rain_wide)[1]*100

# same for rh
windows()
par(mfrow=c(3,5),mar=c(2,2,6,2))
for(i in 2:ncol(rain_wide)){
  plot(x=rain_wide$time,y=as.vector(unlist(rain_wide[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(rain_wide[,i]))
  title(main=colnames(rain_wide)[i])
}
mtext("Rainfall", side = 3, line = - 2, outer = TRUE)


# Select same time window and stations as for relative humidity
rain_short=rain_wide[Tnew,]

na=apply(rain_short,2,function(x) sum(is.na(x))/length(x));na*100

rain_short=rain_short[,c(1,which(colnames(rain_short) %in% c("S100","S104","S107","S108","S115","S116","S121","S43","S44","S50","S60")))]

# Count gaps length
Na_count=count_gaps(rain_short[,-1])
Na_count

# wind speed

wind_long=weather_data[,c("time","station","wind_speed")]

wind_wide=spread(wind_long,station,wind_speed)

# Count NAs
wind_wide %>% summarise_all(~(sum(is.na(.))))/dim(wind_wide)[1]*100

wind_short=wind_wide[Tnew,]

# Same stations as in rh_short
wind_short=wind_short[,c(1,which(colnames(wind_short) %in% c("S100","S104","S107","S108","S115","S116","S121","S43","S44","S50","S60")))]

na=apply(wind_short,2,function(x) sum(is.na(x))/length(x));na*100

# count gaps
Na_count=count_gaps(wind_short[,-1])
Na_count

wind_short_old=wind_short
wind_short=wind_short[1:(5*24),]

  # plot
  windows()
  par(mfrow=c(4,3),mar=c(2,2,6,2))
  for(i in 2:ncol(wind_short)){
    plot(x=wind_short$time,y=as.vector(unlist(wind_short[,i])),type="l",col="black",
         xlab=" ",ylab=" ",
         main=colnames(wind_short[,i]))
    title(main=colnames(wind_short)[i])
  }
  mtext("Wind speed", side = 3, line = - 2, outer = TRUE)