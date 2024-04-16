gw <- scan("anatolia_hati.txt")
str(gw)

# Impute missing values
set.seed(123)
gw[sample(1:length(gw), 10)] <- NA

gw <- ts(gw, start=1975, frequency=12)
str(gw)
frequency(gw)
head(gw)
print(gw)

# # Cycle
# tapply(gw, cycle(gw), mean,na.rm=T)
# 
# # Adjust for seasonality
# head(gw-rep(tapply(gw, cycle(gw), mean),
#             length(gw)/frequency(gw)), 2*frequency(gw))
# 
# windows()
# par(mfrow=c(2,1))
# plot(gw, ylab="depth to groundwater", main="Original series")
# plot(gw-rep(tapply(gw, cycle(gw), mean), length(gw)/frequency(gw)),
#      ylab="difference from cycle mean", main="Seasonally-corrected series")
# abline(h=0, lty=2)
# par(mfrow=c(2,1))

# Adjust for trend and seasonal components
gw.stl <- stl(gw, 
              s.window="periodic",
              na.action = na.approx
              )
#m For the periodic series (s.window="periodic"), the default is 1.5 times the cycle
str(gw.stl)

plot(gw.stl)

# Extract components
res=gw.stl$time.series[,"remainder"]
trend=gw.stl$time.series[,"trend"]
seasonal=gw.stl$time.series[,"seasonal"]

# res as a vector
res=as.vector(res)

# Wrap up in a function
decompose_ts=function(ts){
  # ts is a time series object 
  ts.stl <- stl(ts, 
                s.window="periodic",
                na.action = na.approx)
  res=ts.stl$time.series[,"remainder"]
  trend=ts.stl$time.series[,"trend"]
  seasonal=ts.stl$time.series[,"seasonal"]
  return(list(res=res,trend=trend,seasonal=seasonal))
}

