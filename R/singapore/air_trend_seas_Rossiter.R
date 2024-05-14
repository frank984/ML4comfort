library(dplyr)

load("weather_data_long.RData")

# Select air temp
air_temp=subset(weather_data,
                select=c("time","station","air_temperature"))

# Wide format with dplyr
air_temp_wide=air_temp %>% 
  spread(station,air_temperature)

decompose_ts=function(tsx,period=24,t.win=6){
  # tsx is a vector
  
  # Transform ts to time series
  # ts=matrix(ts,ncol=period,byrow = T)
  # colnames(ts)=0:(period-1)
  # ts=as.ts(ts)
  
  tsx=ts(tsx,frequency=period)
  
  ts.stl <- stl(tsx, 
                s.window=period,
                t.window = t.win,
                na.action = na.approx)
  res=ts.stl$time.series[,"remainder"]
  trend=ts.stl$time.series[,"trend"]
  seasonal=ts.stl$time.series[,"seasonal"]
  return(list(ts.stl=ts.stl,res=res,trend=trend,seasonal=seasonal))
}

S100_dec=decompose_ts(air_temp_wide$S100[1:100])
plot(S100_dec$ts.stl)
