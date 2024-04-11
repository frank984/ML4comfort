#load("air_temp.RData")
load("air_temp_1hour.Rdata")
load("cleaned_data.RData")
load("locations.RData")

library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(tidyr)
library(lubridate)
library(htmltools)

# Percentage of NA for each station
na=apply(air_temp3[,-1],2,function(x){sum(is.na(x))/length(x)}); na*100

# Plot stations in air_temp
air_stat=locations$id%in%colnames(air_temp3)
air_stat=locations[locations$id[air_stat],]

# Plot participants locations
participants_loc=cozie_train[,c("ws_latitude","ws_longitude","id_participant")]
participants_loc=participants_loc[complete.cases(participants_loc),]
names(participants_loc)[1:2]=c("latitude","longitude")

# On the same map, plot small black circles for participants_loc and bigger red circles for air_stat
map=leaflet() %>% addTiles() %>%
  addCircleMarkers(data=participants_loc,
                   radius = 2,
                   color = 'black',
                   stroke = FALSE, fillOpacity = 1,
                   popup = ~paste("<br>Lat:",latitude, "<br>Lon:", longitude
                                  , "<br>Participant:", id_participant
                   )
  ) %>%
  addCircleMarkers(data=air_stat,
                   radius = 4,
                   color = 'red',
                   stroke = FALSE, fillOpacity = 1,
                   popup = ~paste("<br>Lat:",latitude, "<br>Lon:", longitude
                                  , "<br>Station:", id
                   )
  )

map=map %>% addLegend("bottomright", 
                      colors = c("red","black"), 
                      labels = c("Stations","Participants"), title = "Legend")

map

# Remove stations with more than 40% of missing (S102, S117)
air_temp3=subset(air_temp3,select=-c(S102,S117))


# Fill small gaps ---------------------------------------------------------

# Plot stations in air_temp
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_temp3$time,y=as.vector(unlist(air_temp3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp3[,i]))
  # # add vertical lines indicating the split as defined by last_date_first_wind and first_date_second_wind
  # abline(v=last_date_first_wind,col="red")
  # abline(v=first_date_second_wind,col="red")
  title(main=colnames(air_temp3)[i])
}

# Function to count gaps
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

Na_count=count_gaps(air_temp3)
Na_count

# Use SARIMA model 
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

# Fill ALL gaps
air_temp3_filled=fill_gaps(air_temp3,period = 24)

# save(air_temp3_filled,file="air_temp3_filled.Rdata")
load("air_temp3_filled.Rdata")

# Plot sarima results
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_temp3$time,y=as.vector(unlist(air_temp3_filled[,i])),col="red"
    ,type='l',
       xlab=" ",ylab=" ",
       main=colnames(air_temp3[,i]))
  lines(x=air_temp3$time,y=as.vector(unlist(air_temp3[,i])),type="l",col="black")
  title(main=colnames(air_temp3)[i])
}

# air_temp3_filled in long format
air_temp3_filled_long=air_temp3_filled %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temp_filled")

# Sort by time
air_temp3_filled_long=air_temp3_filled_long[order(air_temp3_filled_long$time),]

# air_temp3 in long format
air_temp3_long=air_temp3 %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temperature")

# Sort by time
air_temp3_long=air_temp3_long[order(air_temp3_long$time),]

# Join air_temp3_filled_long with air_temp3_long
air_temp3_filled_long=merge(air_temp3_filled_long,
                            air_temp3_long,
                            by=c("time","station"))

# Sort by time
air_temp3_filled_long=air_temp3_filled_long[order(air_temp3_filled_long$time),]

# Add column that indicates if the value was filled
air_temp3_filled_long$filled=ifelse(is.na(air_temp3_filled_long$air_temperature),1,0)

# Add column that counts consecutive NAs
air_temp3_filled_long$consecutive_na=0

for(j in unique(air_temp3_filled_long$station)){
  id=which(air_temp3_filled_long$station==j)
  for(i in 1:length(id)){
    if(air_temp3_filled_long$filled[id[i]]==1){
      air_temp3_filled_long$consecutive_na[id[i]]=air_temp3_filled_long$consecutive_na[id[i-1]]+1
    }
  }
}
# 

# Create a column that reports the block length of consecutive NAs

air_stat=unique(air_temp3_filled_long$station)
for(i in 1:length(air_stat)){
  df=air_temp3_filled_long$consecutive_na[which(air_temp3_filled_long$station==air_stat[i])]
  df=data.frame(x=df)
  # Convert to data.table
  setDT(df)
  
  # Create a helper column to identify consecutive sequences
  df[, grp := cumsum(x == 0)]
  
  # Calculate max within each group, assign 0 to z where x is 0
  df[, z := ifelse(x == 0, 0, max(x)), by = grp]
  
  # Remove the helper column
  df[, grp := NULL]
  
  df=as.data.frame(df)
  air_temp3_filled_long$consecutive_na[which(air_temp3_filled_long$station==air_stat[i])]=df$z
}

# Keep only "filled" gaps where there were consecutive NAs for at most 24 observations (1 day)
air_temp3_filled_long$air_temp_filled[air_temp3_filled_long$consecutive_na>=24]=NA

# From now on, we will use air_temp3_filled_merged. Let's transform it back to wide format
air_temp3_new=air_temp3_filled_long[,c("time","station","air_temp_filled")]

air_temp3_new=air_temp3_new %>%
  pivot_wider(names_from = station, values_from = air_temp_filled)

# Save air_temp3_new
#save(air_temp3_new,file="air_temp3_smallgaps_filled.Rdata")

# Check filled data -------------------------------------------------------

dim(air_temp3)
dim(air_temp3_new)

# Check percentage of NA for each station and compare with original data
na_new=apply(air_temp3_new[,-1],2,function(x){sum(is.na(x))/length(x)}); na_new*100
na*100

# Check NAs block lengths
Na_count_new=count_gaps(air_temp3_new)
Na_count_new$S108
Na_count$S108

# Plot S108 in air_temp3 in black and S108 in air_temp3_new in red
zoom=600:1000
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(air_temp3$time[zoom],air_temp3_new$S108[zoom],type="l",col="red",main="S108")
lines(air_temp3$time[zoom],air_temp3$S108[zoom],col="black")

air_temp3_old=air_temp3
air_temp3=air_temp3_new

# Remove trend (non-parametric) ------------------------------------------------------------


# 1-day moving averages 

# period
k=24
library(zoo)
#prv=zoo::rollmean(x,k,align = "left", na.pad = TRUE)
# Check if possible to weight rollmean based on the number of observations available for that specific hour quarter
# Check stats::filter 

# Verify number of NAS compared to airtemp3
MAs <- apply(air_temp3[,-1],2,
             function(x){zoo::rollmean(x,k,align = "center", fill = NA)})

MAs=data.frame(time=air_temp3$time,MAs)

# Plot one station
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
zoom=500:1000
plot(air_temp3$time[zoom],air_temp3$S60[zoom],type="l",col="darkgrey",main="S60")
lines(air_temp3$time[zoom],MAs$S60[zoom],type="l",col="red")


# Detrend
air_detrend=air_temp3[,-1]-MAs[,-1]
air_detrend=data.frame(time=air_temp3$time,air_detrend)

# Sort by time
air_detrend=air_detrend[order(air_detrend$time),]

# Plot station S60
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(air_detrend$time,air_detrend$S60,type="l",col="darkgrey",main="S60")

summary(air_detrend$S60)
summary(air_temp3$S60)
summary(MAs$S60)

# Plot detrended series
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_detrend$time,
       y=as.vector(unlist(air_detrend[,i])),type="l",col="darkgrey",
       xlab=" ",ylab=" ",
       main=colnames(air_detrend[,i]))
  title(main=colnames(air_detrend)[i])
}

# Zoom
zoom=500
i=2
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(x=air_detrend$time[1:zoom],
     y=as.vector(unlist(air_detrend[1:zoom,i])),type="l",col="black",
     xlab=" ",ylab=" ",
     main=colnames(air_detrend[,i]))

# Remove seasonal component -----------------------------------------------
library(dplyr)
load("locations.Rdata")
names(locations)[2]="station"
#period=24*4 #(4 quarti d'ora ogni ora ogni giorno)
period=24 #(24 ore ogni giorno)
  
wdnw=(1:period)

#air_seas=data.frame(air_detrend,period=wdnw)
air_seas=data.frame(air_temp3[,-1],period=wdnw)

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
plot(air_temp3$time,air_seas$S60,type="l",col="black")


# Create dataframe with seasonal components to be subtracted from the original data
n <- dim(air_detrend)[1]/period
#n <- dim(air_temp3)[1]/period
air_seas=do.call("rbind", replicate(n, air_seas, simplify = FALSE))

# Remove seasonal component
air_deseas=air_temp3[,-1]-air_seas[,-1]
air_deseas=data.frame(time=air_temp3$time,air_deseas)

# Sort by time
air_deseas=air_deseas[order(air_deseas$time),]

# We will use average seasonal component when doing prediction

# # Remove (all-stations average) seasonal component
# air_av_seas=air_seas_long[,c("period","seasonal")]%>%group_by(period) %>%
#   summarise_if(is.numeric, mean, na.rm = TRUE) 
# # air_av_seas$time=air_temp3$time[1:period]
# n <- dim(air_detrend)[1]/period
# air_seas=do.call("rbind", replicate(n, air_av_seas, simplify = FALSE))
# air_deseas=apply(air_temp3[,-1],2,function(x){x-air_seas$seasonal})
# air_deseas=data.frame(time=air_temp3$time,air_deseas)

# Plot deseasonalized series
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_deseas$time,
       y=as.vector(unlist(air_deseas[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(air_deseas[,i]))
  title(main=colnames(air_deseas)[i])
}

# Residual seasonality, we need to fill small gaps so we get an unbiased estimate of the seasonal component
# To correctly estimate seasonal component we need complete series (96 obs)
# FILL SMALL GAPS BEFORE COMPUTING MOVING AVERAGES FOR NON PARAMETRIC TREND

zoom=1:200
i=2
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(x=air_deseas$time[zoom],
     y=as.vector(unlist(air_deseas[zoom,i])),type="l",
     main=colnames(air_deseas)[i],col="black")

# Plot 
windows()
par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(x=air_deseas$time[-1],
     y=diff(as.vector(unlist(air_deseas[,i]))),type="l",
     main=colnames(air_deseas)[i],col="black")

# Parametric trend (remove)  -------------------------------------------------------

# Fit a linear model to the deseasonalized series with covariates time, latitude and longitude

#load("locations.Rdata")

# Long format 
air_deseas$time_ind=1:dim(air_deseas)[1]

library(tidyr)
air_deseas_long=air_deseas %>%
  pivot_longer(!c(time,time_ind), names_to = "station", values_to = "air_temperature")

# Retrieve lat and long from location based on station id
#colnames(locations)[2]="station"
air_deseas_long=merge(air_deseas_long,locations,by="station")

# Order by time
air_deseas_long=air_deseas_long[order(air_deseas_long$time),]

# IDEA: fit VARMA model with covariates (long and lat) here instead of linear regression (no function on R)

# Do we need lat and long as covariates? Singapore is small, maybe not
linreg=lm(air_temperature~time_ind+latitude+longitude,data=air_deseas_long)
summary(linreg)

# Detrend all stations

air_detren_reg=air_deseas_long$air_temperature - predict(linreg,air_deseas_long)

air_detr_deseas=data.frame(time=air_deseas_long$time,station=air_deseas_long$station,air_detren_reg)

# Sort by time
air_detr_deseas=air_detr_deseas[order(air_detr_deseas$time),]

# wide format
air_detr_deseas=air_detr_deseas %>%
  pivot_wider(names_from = station, values_from = air_detren_reg)

# plot results
windows()
par(mfrow=c(3,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_detr_deseas)){
  plot(x=air_detr_deseas$time,
       y=as.vector(unlist(air_detr_deseas[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_detr_deseas[,i]))
  title(main=colnames(air_detr_deseas)[i])
}



# # Use TBATS ---------------------------------------------------------------
# 
# library(forecast)
# prv=tbats(air_temp3$S60,use.parallel = TRUE, 
#           use.trend = T,
#           seasonal.periods = period)
# plot(prv)
# par(mfrow=c(1,1))
# plot(prv$errors,type="l")
