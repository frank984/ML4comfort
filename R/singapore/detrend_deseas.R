load("air_temp.RData")


# Remove trend (non-parametric) ------------------------------------------------------------

# 4-weeks moving averages (4*24*28=2688 quarti d'ora)
k=4*24*28
library(zoo)
#prv=zoo::rollmean(x,k,align = "left", na.pad = TRUE)
MAs <- apply(air_temp3[,-1],2,
             function(x){zoo::rollmean(x,k,align = "left", na.pad = TRUE)})

MAs=data.frame(time=air_temp3$time,MAs)

# Average of the moving averages
MAs_sum=rowMeans(MAs[,-1],na.rm = T)
# Linear interpolation of the moving averages to fill the missing values
MAs_int=na.interp(MAs_sum)
MAs_data=data.frame(time=MAs$time,MAs_sum, MAs_int)

par(mfrow=c(1,1),mar=c(2,2,2,2))
plot(x=MAs_data$time,y=as.vector(unlist(MAs_data[,3])),type="l",col="red",
     xlab=" ",ylab=" ",
     main="2-weeks moving averages")
lines(x=MAs_data$time,y=as.vector(unlist(MAs_data[,2])),col="black")

# Remove the trend
air_detrend=apply(air_temp3[,-1],2,function(x){x-MAs_data$MAs_int})
air_detrend=data.frame(time=air_temp3$time,air_detrend)

summary(air_detrend)
summary(air_temp3)

# Plot detrended series
par(mfrow=c(4,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_detrend$time,
       y=as.vector(unlist(air_detrend[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_detrend[,i]))
  title(main=colnames(air_detrend)[i])
}

# Remove seasonal component -----------------------------------------------

period=24*4 #(4 quarti d'ora ogni ora ogni giorno)

wdnw=(1:period)

temp=data.frame(air_detrend,period=wdnw)

temp=temp%>%group_by(period) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 

n <- dim(air_detrend)[1]/period
air_seas=do.call("rbind", replicate(n, temp, simplify = FALSE))

air_deseas=air_temp3[,-1]-air_seas[,-1]
air_deseas=data.frame(time=air_temp3$time,air_deseas)

# Plot deseasonalized series
windows()
par(mfrow=c(4,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_deseas$time,
       y=as.vector(unlist(air_deseas[,i])),type="l",col="blue",
       xlab=" ",ylab=" ",
       main=colnames(air_deseas[,i]))
  title(main=colnames(air_deseas)[i])
}

# Parametric trend  -------------------------------------------------------

# Fit a linear model to the deseasonalized series with covariates time, latitude and longitude

load("locations.Rdata")

# Long format 

air_deseas$time_ind=1:dim(air_deseas)[1]

air_deseas_long=air_deseas %>%
  pivot_longer(!c(time,time_ind), names_to = "station", values_to = "air_temperature")

# Retrieve lat and long from location based on station id
colnames(locations)[2]="station"
air_deseas_long=merge(air_deseas_long,locations,by="station")

linreg=lm(air_temperature~time_ind+latitude+longitude,data=air_deseas_long)
summary(linreg)

air_res=
  air_deseas_long$air_temperature-
  (linreg$coefficients[1]+linreg$coefficients[2]*air_deseas_long$time_ind+
                                    linreg$coefficients[3]*air_deseas_long$latitude+
                                    linreg$coefficients[4]*air_deseas_long$longitude)

air_res=data.frame(time=air_deseas_long$time,station=air_deseas_long$station,air_res)

# Transform long format to wide format
air_res_wide=air_res%>%pivot_wider(names_from = station,values_from = air_res)

# Sort by time
air_res_wide=air_res_wide[order(air_res_wide$time),]

# Plot residuals
windows()
par(mfrow=c(4,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_res_wide)){
  plot(x=air_res_wide$time,
       y=as.vector(unlist(air_res_wide[,i])),type="l",col="darkgreen",
       xlab=" ",ylab=" ",
       main=colnames(air_res_wide[,i]))
  title(main=colnames(air_res_wide)[i])
}
