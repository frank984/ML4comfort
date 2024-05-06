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
TT/2-7
na_len=TT*c(.05,.1,.2)
na_start=rep(TT/2-7,3)

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

sarima_times=rep(0,3)
st=Sys.time()
air_5_sarima=fill_sarima(air_5,period = 24)
en=Sys.time()
sarima_times[1]=en-st

st=Sys.time()
air_10_sarima=fill_sarima(air_10,period = 24)
en=Sys.time()
sarima_times[2]=en-st

st=Sys.time()
air_20_sarima=fill_sarima(air_20,period = 24)
en=Sys.time()
sarima_times[3]=en-st

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

tkgr_times=rep(0,3)

st=Sys.time()
air_5_tkr=temp_krige(air_5)
en=Sys.time()
tkgr_times[1]=en-st

st=Sys.time()
air_10_tkr=temp_krige(air_10)
en=Sys.time()
tkgr_times[2]=en-st

st=Sys.time()
air_20_tkr=temp_krige(air_20)
en=Sys.time()
tkgr_times[3]=en-st

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

ma_times=rep(0,3)

st=Sys.time()
air_sdem5=MA_imp(air_5)
en=Sys.time()
ma_times[1]=en-st

st=Sys.time()
air_sdem10=MA_imp(air_10)
st=Sys.time()
ma_times[2]=en-st

st=Sys.time()
air_sdem20=MA_imp(air_20)
en=Sys.time()
ma_times[3]=en-st

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

save.image("~/Documents/git/ML4comfort/R/singapore/run_parallel.RData")

# 3.1) RMSE ---------------------------------------------------------------

# Function extracting RMSE from each element of lists above

# 4) Preliminary detrend-deseas -------------------------------------------


## LOESS
# air 5%
air5_loess_sarima=LOESS.df(air_5_sarima)

air_5_tkr=select(air_5_tkr,subset=-c(indx,knot))
air5_loess_tkr=LOESS.df(air_5_tkr)
air5_loess_SDEM=LOESS.df(air5_SDEM)
air5_loess_naive=LOESS.df(air5_naive)
# air 10%
air10_loess_sarima=LOESS.df(air_10_sarima)

air_10_tkr=select(air_10_tkr,subset=-c(indx,knot))
air10_loess_tkr=LOESS.df(air_10_tkr)
air10_loess_SDEM=LOESS.df(air10_SDEM)
air10_loess_naive=LOESS.df(air10_naive)
# air 20%
air20_loess_sarima=LOESS.df(air_20_sarima)
air_20_tkr=select(air_20_tkr,subset=-c(indx,knot))
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
air20_hw_tkr=HoltWint.df(air_20_tkr,24)
air20_hw_SDEM=HoltWint.df(air20_SDEM,24)
air20_hw_naive=HoltWint.df(air20_naive,24)

# 5) Ordinary kriging --------------------------------------------------------

#hw
indx=2:ncol(air_short)

start = Sys.time()
air5_sarima_hw_res <- parallel::mclapply(indx,
                                       function(x)CV_STkr(x,
                                                          air5_hw_sarima$residuals,
                                                          locations),
                                       mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_sarima_hw_res=end-start

start = Sys.time()
air5_tkr_hw_res <- parallel::mclapply(indx,
                                         function(x)CV_STkr(x,
                                                            air5_hw_tkr$residuals,
                                                            locations),
                                         mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_tkr_hw_res=end-start

start = Sys.time()
air5_SDEM_hw_res <- parallel::mclapply(indx,
                                      function(x)CV_STkr(x,
                                                         air5_hw_SDEM$residuals,
                                                         locations),
                                      mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_SDEM_hw_res=end-start

start = Sys.time()
air5_naive_hw_res <- parallel::mclapply(indx,
                                       function(x)CV_STkr(x,
                                                          air5_hw_naive$residuals,
                                                          locations),
                                       mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_naive_hw_res=end-start


start = Sys.time()
air10_sarima_hw_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air10_hw_sarima$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_sarima_hw_res=end-start

start = Sys.time()
air10_tkr_hw_res <- parallel::mclapply(indx,
                                       function(x)CV_STkr(x,
                                                          air10_hw_tkr$residuals,
                                                          locations),
                                       mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_tkr_hw_res=end-start

start = Sys.time()
air10_SDEM_hw_res <- parallel::mclapply(indx,
                                        function(x)CV_STkr(x,
                                                           air10_hw_SDEM$residuals,
                                                           locations),
                                        mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_SDEM_hw_res=end-start

start = Sys.time()
air10_naive_hw_res <- parallel::mclapply(indx,
                                         function(x)CV_STkr(x,
                                                            air10_hw_naive$residuals,
                                                            locations),
                                         mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_naive_hw_res=end-start


start = Sys.time()
air20_sarima_hw_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air20_hw_sarima$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_sarima_hw_res=end-start

start = Sys.time()
air20_tkr_hw_res <- parallel::mclapply(indx,
                                       function(x)CV_STkr(x,
                                                          air20_hw_tkr$residuals,
                                                          locations),
                                       mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_tkr_hw_res=end-start

start = Sys.time()
air20_SDEM_hw_res <- parallel::mclapply(indx,
                                        function(x)CV_STkr(x,
                                                           air20_hw_SDEM$residuals,
                                                           locations),
                                        mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_SDEM_hw_res=end-start

start = Sys.time()
air20_naive_hw_res <- parallel::mclapply(indx,
                                         function(x)CV_STkr(x,
                                                            air20_hw_naive$residuals,
                                                            locations),
                                         mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_naive_hw_res=end-start

#loess
start = Sys.time()
air5_sarima_loess_res <- parallel::mclapply(indx,
                                            function(x)CV_STkr(x,
                                                               air5_loess_sarima$residuals,
                                                               locations),
                                            mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_sarima_loess_res=end-start

start = Sys.time()
air5_tkr_loess_res <- parallel::mclapply(indx,
                                         function(x)CV_STkr(x,
                                                            air5_loess_tkr$residuals,
                                                            locations),
                                         mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_tkr_loess_res=end-start

start = Sys.time()
air5_SDEM_loess_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air5_loess_SDEM$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_SDEM_loess_res=end-start

start = Sys.time()
air5_naive_loess_res <- parallel::mclapply(indx,
                                           function(x)CV_STkr(x,
                                                              air5_loess_naive$residuals,
                                                              locations),
                                           mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_naive_loess_res=end-start


start = Sys.time()
air10_sarima_loess_res <- parallel::mclapply(indx,
                                             function(x)CV_STkr(x,
                                                                air10_loess_sarima$residuals,
                                                                locations),
                                             mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_sarima_loess_res=end-start

start = Sys.time()
air10_tkr_loess_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air10_loess_tkr$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_tkr_loess_res=end-start

start = Sys.time()
air10_SDEM_loess_res <- parallel::mclapply(indx,
                                           function(x)CV_STkr(x,
                                                              air10_loess_SDEM$residuals,
                                                              locations),
                                           mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_SDEM_loess_res=end-start

start = Sys.time()
air10_naive_loess_res <- parallel::mclapply(indx,
                                            function(x)CV_STkr(x,
                                                               air10_loess_naive$residuals,
                                                               locations),
                                            mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_naive_loess_res=end-start


start = Sys.time()
air20_sarima_loess_res <- parallel::mclapply(indx,
                                             function(x)CV_STkr(x,
                                                                air20_loess_sarima$residuals,
                                                                locations),
                                             mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_sarima_loess_res=end-start

start = Sys.time()
air20_tkr_loess_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air20_loess_tkr$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_tkr_loess_res=end-start

start = Sys.time()
air20_SDEM_loess_res <- parallel::mclapply(indx,
                                           function(x)CV_STkr(x,
                                                              air20_loess_SDEM$residuals,
                                                              locations),
                                           mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_SDEM_loess_res=end-start

start = Sys.time()
air20_naive_loess_res <- parallel::mclapply(indx,
                                            function(x)CV_STkr(x,
                                                               air20_loess_naive$residuals,
                                                               locations),
                                            mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_naive_loess_res=end-start

save.image("~/Documents/git/ML4comfort/R/singapore/run_parallel.RData")
