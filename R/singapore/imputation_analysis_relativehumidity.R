library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

load("weather_data_long.RData")

rh_long=weather_data[,c("time","station","relative_humidity")]

rh_wide=spread(rh_long,station,relative_humidity)

# Count NAs
rh_wide %>% summarise_all(~(sum(is.na(.))))/dim(rh_wide)[1]*100

rh_short=rh_wide[]

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


# Count NAs in air_short
Na_count=count_gaps(air_short)
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
