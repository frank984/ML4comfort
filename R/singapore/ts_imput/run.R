
# 1) Load short window ---------------------------------------------------------

source("Utils.R")
load("air_short.Rdata")
air_short=air_short[251:750,]

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


# 3) Universal kriging --------------------------------------------------

air5_sarima_full=CV_STkr(5,air_5_sarima,locations,ordinary=F)
air5_tkr_full=CV_STkr(5,air_5_tkr,locations,ordinary=F)
air5_SDEM_full=CV_STkr(5,air5_SDEM,locations,ordinary=F)
air5_naive_full=CV_STkr(5,air5_naive,locations,ordinary=F)

air10_sarima_full=CV_STkr(5,air_10_sarima,locations,ordinary=F)
air10_tkr_full=CV_STkr(5,air_10_tkr,locations,ordinary=F)
air10_SDEM_full=CV_STkr(5,air10_SDEM,locations,ordinary=F)
air10_naive_full=CV_STkr(5,air10_naive,locations,ordinary=F)

air20_sarima_full=CV_STkr(5,air_20_sarima,locations,ordinary=F)
air20_tkr_full=CV_STkr(5,air_20_tkr,locations,ordinary=F)
air20_SDEM_full=CV_STkr(5,air20_SDEM,locations,ordinary=F)
air20_naive_full=CV_STkr(5,air20_naive,locations,ordinary=F)

# step1=cvobj_STFDF(air5_naive,locations,6)
# step2=STkriging(step1$dat_stfdf,step1$loc_to_pred,ordinary = F)



# 3.1) RMSE ---------------------------------------------------------------

air5_sarima_full$RMSE
air5_tkr_full$RMSE
air5_SDEM_full$RMSE
plot(air5_SDEM_full$step2$stkgr@data$var1.pred,type='l',xlab="Time",ylab="Temperature")
stat=air5_SDEM_full$step1$stat_id
lines(unlist(as.vector(air_short[stat])),col='red')
legend("topright",legend=c("SDEM","True"),col=c("black","red"),lty=1:1,cex=.5)
air5_naive_full$RMSE

air10_sarima_full$RMSE
air10_tkr_full$RMSE
air10_SDEM_full$RMSE
plot(air10_SDEM_full$step2$stkgr@data$var1.pred,type='l',xlab="Time",ylab="Temperature")
stat=air10_SDEM_full$step1$stat_id
lines(unlist(as.vector(air_short[stat])),col='red')
legend("topright",legend=c("SDEM","True"),col=c("black","red"),lty=1:1,cex=.5)
air10_naive_full$RMSE

air20_sarima_full$RMSE
air20_tkr_full$RMSE
air20_SDEM_full$RMSE
air20_naive_full$RMSE



# 4) Preliminary detrend-deseas -------------------------------------------


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
air20_hw_tkr=HoltWint.df(air_20_tkr,24)
air20_hw_SDEM=HoltWint.df(air20_SDEM,24)
air20_hw_naive=HoltWint.df(air20_naive,24)

# 5) Ordinary kriging --------------------------------------------------------

#hw
air5_sarima_hw_res=CV_STkr(5,air5_hw_sarima$residual,locations)

#air5_hw_tkr$residuals=select(air5_hw_tkr$residuals,subset=-c(knot,indx))
air5_tkr_hw_res=CV_STkr(5,air5_hw_tkr$residuals,locations)

air5_SDEM_hw_res=CV_STkr(5,air5_hw_SDEM$residuals,locations)
air5_naive_hw_res=CV_STkr(5,air5_hw_naive$residuals,locations)

air10_sarima_hw_res=CV_STkr(5,air10_hw_sarima$residuals,locations)

#air10_hw_tkr$residuals=select(air10_hw_tkr$residuals,subset=-c(knot,indx))
air10_tkr_hw_res=CV_STkr(5,air10_hw_tkr$residuals,locations)

air10_SDEM_hw_res=CV_STkr(5,air10_hw_SDEM$residuals,locations)
air10_naive_hw_res=CV_STkr(5,air10_hw_naive$residuals,locations)

air20_sarima_hw_res=CV_STkr(5,air20_hw_sarima$residuals,locations)

#air20_hw_tkr$residuals=select(air20_hw_tkr$residuals,subset=-c(knot,indx))
air20_tkr_hw_res=CV_STkr(5,air20_hw_tkr$residuals,locations)

air20_SDEM_hw_res=CV_STkr(5,air20_hw_SDEM$residuals,locations)
air20_naive_hw_res=CV_STkr(5,air20_hw_naive$residuals,locations)

#loess
air5_sarima_loess_res=CV_STkr(5,air5_loess_sarima$residual,locations)

air5_loess_tkr$residuals=select(air5_loess_tkr$residuals,subset=-c(knot,indx))
air5_tkr_loess_res=CV_STkr(5,air5_loess_tkr$residuals,locations)

air5_SDEM_loess_res=CV_STkr(5,air5_loess_SDEM$residual,locations)
air5_naive_loess_res=CV_STkr(5,air5_loess_naive$residual,locations)

air10_sarima_loess_res=CV_STkr(5,air10_loess_sarima$residual,locations)

air10_loess_tkr$residuals=select(air10_loess_tkr$residuals,subset=-c(knot,indx))
air10_tkr_loess_res=CV_STkr(5,air10_loess_tkr$residuals,locations)

air10_SDEM_loess_res=CV_STkr(5,air10_loess_SDEM$residual,locations)
air10_naive_loess_res=CV_STkr(5,air10_loess_naive$residual,locations)

air20_sarima_loess_res=CV_STkr(5,air20_loess_sarima$residual,locations)

air20_loess_tkr$residuals=select(air20_loess_tkr$residuals,subset=-c(knot,indx))
air20_tkr_loess_res=CV_STkr(5,air20_loess_tkr$residuals,locations)

air20_SDEM_loess_res=CV_STkr(5,air20_loess_SDEM$residual,locations)
air20_naive_loess_res=CV_STkr(5,air20_loess_naive$residual,locations)

# 5.1) Recover original structure ---------------------------------------------------------------

time=air_short$time

S108_10_hw_SDEM=get_orig_series(air10_hw_SDEM,
                     kgrST.res=air10_SDEM_hw_res$step2$stkgr@data$var1.pred,
                     locations2,
                     target=air10_SDEM_hw_res$stat_id,
                     loess=F)

rmse_detrdeseas(reconstr_series=S108_10_hw_SDEM$result,
                true_series=air_short$S108[-(1:24)],
                time=time[-(1:24)],plot=T)

S108_10_loess_SDEM=get_orig_series(air10_loess_SDEM,
                                kgrST.res=air10_SDEM_loess_res$step2$stkgr@data$var1.pred,
                                locations2,
                                target=air10_SDEM_loess_res$stat_id,
                                loess=T)
rmse_detrdeseas(reconstr_series=S108_10_loess_SDEM$result,
                true_series=air_short$S108,
                time=time,plot=T)


