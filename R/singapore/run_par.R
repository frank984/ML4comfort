# 1) Load short window ---------------------------------------------------------

source("Utils.R")
load("air_short.Rdata")
#air_short=air_short[251:750,]

load("locations.Rdata")
locations2<- locations[,c("id","longitude","latitude")]
locations2=locations2[locations2$id %in% colnames(air_short),]
# target="S115"

# 1.1) Fill with NAs ------------------------------------------------------

TT=dim(air_short)[1]
na_len=TT*c(.05,.1,.2)
na_start=rep(200,3)

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



# 2) Imputation without preliminary detrend-deseas ------------------------


# 2.1) SARIMA ------------------------------------------------------------------
air_5_sarima=fill_sarima(air_5,period = 24)
air_10_sarima=fill_sarima(air_10,period = 24)
air_20_sarima=fill_sarima(air_20,period = 24)

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



# 2.2) Temporal kriging --------------------------------------------------------

air_5_tkr=temp_krige(air_5)
air_10_tkr=temp_krige(air_10)
air_20_tkr=temp_krige(air_20)

# Plot
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



# 2.3) SDEM and NAIVE ----------------------------------------------------------

air_sdem5=MA_imp(air_5)
air_sdem10=MA_imp(air_10)
air_sdem20=MA_imp(air_20)

air5_SDEM=air_sdem5$SDEM
air10_SDEM=air_sdem10$SDEM
air20_SDEM=air_sdem20$SDEM

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
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air20_SDEM[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - SDEM", side = 3, line = - 2, outer = TRUE)


air5_naive=air_sdem5$naive
air10_naive=air_sdem10$naive
air20_naive=air_sdem20$naive


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


# Comparison
mean(unlist(as.vector(rmse(air_short,air_5_sarima))))
mean(unlist(as.vector(rmse(air_short,air_5_tkr))))
mean(unlist(as.vector(rmse(air_short,air5_SDEM))))
mean(unlist(as.vector(rmse(air_short,air5_naive))))

mean(unlist(as.vector(rmse(air_short,air_10_sarima))))
mean(unlist(as.vector(rmse(air_short,air_10_tkr))))
mean(unlist(as.vector(rmse(air_short,air10_SDEM))))
mean(unlist(as.vector(rmse(air_short,air10_naive))))

mean(unlist(as.vector(rmse(air_short,air_20_sarima))))
mean(unlist(as.vector(rmse(air_short,air_20_tkr))),na.rm = T)
mean(unlist(as.vector(rmse(air_short,air20_SDEM))))
mean(unlist(as.vector(rmse(air_short,air20_naive))))


# 3) Universal kriging (parallel) -----------------------------------------

indx=2:ncol(air_short)
#indx_list=expand.grid(indx=indx)

air5_sarima_full=CV_STkr(5,air_5_sarima,locations2,ordinary=F)
air5_tkr_full=CV_STkr(5,air_5_tkr,locations2,ordinary=F)
air5_SDEM_full=CV_STkr(5,air5_SDEM,locations2,ordinary=F)
air5_naive_full=CV_STkr(5,air5_naive,locations2,ordinary=F)

start = Sys.time()
air5_sarima_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_5_sarima,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_sarima_full=end-start

start = Sys.time()
air5_tkr_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_5_tkr,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_tkr_full=end-start

start = Sys.time()
air5_SDEM_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air5_SDEM,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_SDEM_full=end-start

start = Sys.time()
air5_naive_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air5_naive,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_naive_full=end-start

air10_sarima_full=CV_STkr(5,air_10_sarima,locations,ordinary=F)
air10_tkr_full=CV_STkr(5,air_10_tkr,locations,ordinary=F)
air10_SDEM_full=CV_STkr(5,air10_SDEM,locations,ordinary=F)
air10_naive_full=CV_STkr(5,air10_naive,locations,ordinary=F)

start = Sys.time()
air10_sarima_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_10_sarima,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_sarima_full=end-start

start = Sys.time()
air10_tkr_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_10_tkr,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_tkr_full=end-start

start = Sys.time()
air10_SDEM_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air10_SDEM,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_SDEM_full=end-start

start = Sys.time()
air10_naive_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air10_naive,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_naive_full=end-start

air20_sarima_full=CV_STkr(5,air_20_sarima,locations,ordinary=F)
air20_tkr_full=CV_STkr(5,air_20_tkr,locations,ordinary=F)
air20_SDEM_full=CV_STkr(5,air20_SDEM,locations,ordinary=F)
air20_naive_full=CV_STkr(5,air20_naive,locations,ordinary=F)

start = Sys.time()
air20_sarima_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_20_sarima,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_sarima_full=end-start

start = Sys.time()
air20_tkr_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air_20_tkr,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_tkr_full=end-start

start = Sys.time()
air20_SDEM_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air20_SDEM,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_SDEM_full=end-start

start = Sys.time()
air20_naive_full <- parallel::mclapply(indx,
                              function(x)CV_STkr(x,
                                                 air20_naive,
                                                 locations2,
                                                 ordinary=F),
                              mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_naive_full=end-start





