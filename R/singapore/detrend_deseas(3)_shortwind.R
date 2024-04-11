# Deseasonalized data (shorter window)

# 1. Load the data
load("air_temp3_smallgaps_filled.Rdata")
#load("cleaned_data.RData")
load("locations.RData")

library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(lubridate)
library(htmltools)

# Plot all stations in air_temp3_new
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3_new)){
  plot(x=air_temp3_new$time,y=as.vector(unlist(air_temp3_new[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp3_new[,i]))
  # # add vertical lines indicating the split as defined by last_date_first_wind and first_date_second_wind
  # abline(v=last_date_first_wind,col="red")
  # abline(v=first_date_second_wind,col="red")
  title(main=colnames(air_temp3_new)[i])
}

# 2. Select the data
t_length=1:480 
air_short=air_temp3_new[t_length,]

#How many NAs for each station?
na_count=apply(air_short,2,function(x) sum(is.na(x)))
na_count

# Drop S108
air_short=subset(air_short,select=-c(S108))

# 3. Plot data
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_short)){
  plot(x=air_short$time,y=as.vector(unlist(air_short[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_short[,i]))
  # # add vertical lines indicating the split as defined by last_date_first_wind and first_date_second_wind
  # abline(v=last_date_first_wind,col="red")
  # abline(v=first_date_second_wind,col="red")
  title(main=colnames(air_short)[i])
}

# Remove trend (non-parametric) ------------------------------------------------------------

# 1-day moving averages 

# period
k=24*7
library(zoo)
#prv=zoo::rollmean(x,k,align = "left", na.pad = TRUE)
# Check if possible to weight rollmean based on the number of observations available for that specific hour quarter
# Check stats::filter 

# Verify number of NAS compared to airtemp3
MAs <- apply(air_short[,-1],2,
             function(x){zoo::rollmean(x,k,align = "center", fill = NA)})

MAs=data.frame(time=air_short$time,MAs)

# Plot one station
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(air_short$time,air_short$S60,type="l",col="darkgrey",main="S60")
lines(air_short$time,MAs$S60,type="l",col="red")


# Detrend
air_detrend=air_short[,-1]-MAs[,-1]
air_detrend=data.frame(time=air_short$time,air_detrend)

# Sort by time
air_detrend=air_detrend[order(air_detrend$time),]

# Plot station S60
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(air_detrend$time,air_detrend$S60,type="l",col="darkgrey",main="S60")

summary(air_detrend$S60)
summary(air_short$S60)
summary(MAs$S60)

# Plot detrended series
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_short)){
  plot(x=air_detrend$time,
       y=as.vector(unlist(air_detrend[,i])),type="l",col="darkgrey",
       xlab=" ",ylab=" ",
       main=colnames(air_detrend[,i]))
  title(main=colnames(air_detrend)[i])
}

# Remove seasonal component -----------------------------------------------
library(dplyr)
load("locations.Rdata")
names(locations)[2]="station"
#period=24*4 #(4 quarti d'ora ogni ora ogni giorno)
period=24 #(24 ore ogni giorno)

wdnw=(1:period)

air_seas=data.frame(air_detrend,period=wdnw)
#air_seas=data.frame(air_short[,-1],period=wdnw)

air_seas=air_seas%>%group_by(period) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 

# air_seas in long format
air_seas_long=air_seas %>%
  pivot_longer(!c(period), names_to = "station", values_to = "seasonal")

# Save seasonal component in long format (needed after kriging to add it back to the data)
air_seas_long=merge(locations,air_seas_long,by="station")

# Plot S60 in air_seas
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(air_seas$period,air_seas$S60,type="l",col="black")


# Create dataframe with seasonal components to be subtracted from the original data
n <- dim(air_detrend)[1]/period
#n <- dim(air_temp3)[1]/period
air_seas=do.call("rbind", replicate(n, air_seas, simplify = FALSE))

# Remove seasonal component
air_deseas=air_short[,-1]-air_seas[,-1]
air_deseas=data.frame(time=air_short$time,air_deseas)

# Sort by time
air_deseas=air_deseas[order(air_deseas$time),]

# We will later use average seasonal component when doing prediction

# Plot deseasonalized series
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_short)){
  plot(x=air_deseas$time,
       y=as.vector(unlist(air_deseas[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(air_deseas[,i]))
  title(main=colnames(air_deseas)[i])
}
