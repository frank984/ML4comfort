# 1) Load short window ---------------------------------------------------------

source("Utils.R")
load("air_short.Rdata")
load("rh_short.Rdata")
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


# 2.4) Linear regression -------------------------------------------------------

air5_lr=lin_reg_imp(air_5,rh_short)
air10_lr=lin_reg_imp(air_10,rh_short)
air20_lr=lin_reg_imp(air_20,rh_short)

#Plot
windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_5)){
  plot(x=air_5$time,y=as.vector(unlist(air5_lr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_5[,i]))
  lines(x=air_5$time,y=as.vector(unlist(air_5[,i])),col="black")
  
  title(main=colnames(air_5)[i])
}
mtext("Air temperatures - 5% NAs - Linear regression", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_10)){
  plot(x=air_10$time,y=as.vector(unlist(air10_lr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_10[,i]))
  lines(x=air_10$time,y=as.vector(unlist(air_10[,i])),col="black")
  title(main=colnames(air_10)[i])
}
mtext("Air temperatures - 10% NAs - Linear regression", side = 3, line = - 2, outer = TRUE)

windows()
par(mfrow=c(4,3),mar=c(2,2,6,2))
for(i in 2:ncol(air_20)){
  plot(x=air_20$time,y=as.vector(unlist(air20_lr[,i])),col="red"
       ,type="l",
       xlab=" ",ylab=" ",
       main=colnames(air_20[,i]))
  lines(x=air_20$time,y=as.vector(unlist(air_20[,i])),col="black")
  title(main=colnames(air_20)[i])
}
mtext("Air temperatures - 20% NAs - Linear regression", side = 3, line = - 2, outer = TRUE)

# Comparison
mean(unlist(as.vector(rmse(air_short,air_5_sarima))))
mean(unlist(as.vector(rmse(air_short,air_5_tkr))))
#mean(unlist(as.vector(rmse(air_short,air5_SDEM))))
mean(unlist(as.vector(rmse(air_short,air5_lr))))
mean(unlist(as.vector(rmse(air_short,air5_naive))))

mean(unlist(as.vector(rmse(air_short,air_10_sarima))))
mean(unlist(as.vector(rmse(air_short,air_10_tkr))))
#mean(unlist(as.vector(rmse(air_short,air10_SDEM))))
mean(unlist(as.vector(rmse(air_short,air10_lr))))
mean(unlist(as.vector(rmse(air_short,air10_naive))))


mean(unlist(as.vector(rmse(air_short,air_20_sarima))))
mean(unlist(as.vector(rmse(air_short,air_20_tkr))),na.rm = T)
#mean(unlist(as.vector(rmse(air_short,air20_SDEM))))
mean(unlist(as.vector(rmse(air_short,air20_lr))))
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


# 3) NEW Universal kriging ------------------------------------------------

start = Sys.time()
air5_linreg_full <- parallel::mclapply(indx,
                                       function(x)CV_STkr(x,
                                                          air5_lr,
                                                          locations2,
                                                          ordinary=F),
                                       mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_lr_full=end-start

start = Sys.time()
air10_linreg_full <- parallel::mclapply(indx,
                                        function(x)CV_STkr(x,
                                                           air10_lr,
                                                           locations2,
                                                           ordinary=F),
                                        mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_lr_full=end-start

start = Sys.time()
air20_linreg_full <- parallel::mclapply(indx,
                                        function(x)CV_STkr(x,
                                                           air20_lr,
                                                           locations2,
                                                           ordinary=F),
                                        mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_lr_full=end-start

# 3.1) RMSE 
# 
# # 5
# time=air_short$time
# air5_sarima_full_recover=df_recover(x=air5_sarima_full,
#                                        locations2=locations2,time=time,residuals=F)
# air5_tkr_full_recover=df_recover(x=air5_tkr_full,
#                                     locations2=locations2,time=time,residuals=F)
# air5_SDEM_full_recover=df_recover(x=air5_SDEM_full,
#                                      locations2=locations2,time=time,residuals=F)
# air5_naive_full_recover=df_recover(x=air5_naive_full,
#                                       locations2=locations2,time=time,residuals=F)
# 
# miss5=c(na_start[1],(na_start[1]+na_len[1]))
# plot_air5_sarima_full_recover=rmse_detrdeseas(air5_sarima_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="SARIMA - Full",
#                                                 miss=miss5)
# plot_air5_tkr_full_recover=rmse_detrdeseas(air5_tkr_full_recover$S100,
#                                               air_short$S100,
#                                               air_short$time,type="TKR - Full",
#                                               miss=miss5)
# plot_air5_SDEM_full_recover=rmse_detrdeseas(air5_SDEM_full_recover$S100,
#                                                air_short$S100,
#                                                air_short$time,type="SDEM - Full",
#                                                miss=miss5)
# plot_air5_naive_full_recover=rmse_detrdeseas(air5_naive_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="Naive - Full",
#                                                 miss=miss5)
# 
# 
# 
# PG_5full<- ggarrange(plot_air5_sarima_full_recover$plot,
#                      plot_air5_tkr_full_recover$plot,
#                      plot_air5_SDEM_full_recover$plot,
#                      plot_air5_naive_full_recover$plot,
#                       ncol=2,nrow=2,
#           common.legend = T,
#           legend="bottom")
# 
# windows()
# annotate_figure(PG_5full, top = text_grob("S100", 
#                                         color = "Black", face = "bold", size = 14))
# 
# # 10
# air10_sarima_full_recover=df_recover(x=air10_sarima_full,
#                                        locations2=locations2,time=time,residuals=F)
# air10_tkr_full_recover=df_recover(x=air10_tkr_full,
#                                     locations2=locations2,time=time,residuals=F)
# air10_SDEM_full_recover=df_recover(x=air10_SDEM_full,
#                                      locations2=locations2,time=time,residuals=F)
# air10_naive_full_recover=df_recover(x=air10_naive_full,
#                                       locations2=locations2,time=time,residuals=F)
# 
# miss10=c(na_start[2],(na_start[2]+na_len[2]))
# plot_air10_sarima_full_recover=rmse_detrdeseas(air10_sarima_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="SARIMA - Full",
#                                                 miss=miss10)
# plot_air10_tkr_full_recover=rmse_detrdeseas(air10_tkr_full_recover$S100,
#                                               air_short$S100,
#                                               air_short$time,type="TKR - Full",
#                                               miss=miss10)
# plot_air10_SDEM_full_recover=rmse_detrdeseas(air10_SDEM_full_recover$S100,
#                                                air_short$S100,
#                                                air_short$time,type="SDEM - Full",
#                                                miss=miss10)
# plot_air10_naive_full_recover=rmse_detrdeseas(air10_naive_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="Naive - Full",
#                                                 miss=miss10)
# 
# PG_10full<- ggarrange(plot_air10_sarima_full_recover$plot,
#                      plot_air10_tkr_full_recover$plot,
#                      plot_air10_SDEM_full_recover$plot,
#                      plot_air10_naive_full_recover$plot,
#                       ncol=2,nrow=2,
#           common.legend = T,
#           legend="bottom")
# 
# windows()
# annotate_figure(PG_10full, top = text_grob("S100", 
#                                         color = "Black", face = "bold", size = 14))
# 
# # 20
# air20_sarima_full_recover=df_recover(x=air20_sarima_full,
#                                        locations2=locations2,time=time,residuals=F)
# air20_tkr_full_recover=df_recover(x=air20_tkr_full,
#                                     locations2=locations2,time=time,residuals=F)
# air20_SDEM_full_recover=df_recover(x=air20_SDEM_full,
#                                      locations2=locations2,time=time,residuals=F)
# air20_naive_full_recover=df_recover(x=air20_naive_full,
#                                       locations2=locations2,time=time,residuals=F)
# 
# miss20=c(na_start[3],(na_start[3]+na_len[3]))
# plot_air20_sarima_full_recover=rmse_detrdeseas(air20_sarima_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="SARIMA - Full",
#                                                 miss=miss20)
# plot_air20_tkr_full_recover=rmse_detrdeseas(air20_tkr_full_recover$S100,
#                                               air_short$S100,
#                                               air_short$time,type="TKR - Full",
#                                               miss=miss20)
# plot_air20_SDEM_full_recover=rmse_detrdeseas(air20_SDEM_full_recover$S100,
#                                                air_short$S100,
#                                                air_short$time,type="SDEM - Full",
#                                                miss=miss20)
# plot_air20_naive_full_recover=rmse_detrdeseas(air20_naive_full_recover$S100,
#                                                 air_short$S100,
#                                                 air_short$time,type="Naive - Full",
#                                                 miss=miss20)
# 
# PG_20full<- ggarrange(plot_air20_sarima_full_recover$plot,
#                      plot_air20_tkr_full_recover$plot,
#                      plot_air20_SDEM_full_recover$plot,
#                      plot_air20_naive_full_recover$plot,
#                       ncol=2,nrow=2,
#           common.legend = T,
#           legend="bottom")
# 
# windows()
# annotate_figure(PG_20full, top = text_grob("S100", 
#                                         color = "Black", face = "bold", size = 14))
# 
# 
# stat_names=unlist(lapply(air5_sarima_full,function(x)x$stat_id)); stat_names
# 
# RMSE_air5_sarima_full=unlist(lapply(air5_sarima_full,function(x)x$RMSE))
# RMSE_air5_tkr_full=unlist(lapply(air5_tkr_full,function(x)x$RMSE))
# RMSE_air5_SDEM_full=unlist(lapply(air5_SDEM_full,function(x)x$RMSE))
# RMSE_air5_naive_full=unlist(lapply(air5_naive_full,function(x)x$RMSE))
# 
# RMSE_air10_sarima_full=unlist(lapply(air10_sarima_full,function(x)x$RMSE))
# RMSE_air10_tkr_full=unlist(lapply(air10_tkr_full,function(x)x$RMSE))
# RMSE_air10_SDEM_full=unlist(lapply(air10_SDEM_full,function(x)x$RMSE))
# RMSE_air10_naive_full=unlist(lapply(air10_naive_full,function(x)x$RMSE))
# 
# RMSE_air20_sarima_full=unlist(lapply(air20_sarima_full,function(x)x$RMSE))
# RMSE_air20_tkr_full=unlist(lapply(air20_tkr_full,function(x)x$RMSE))
# RMSE_air20_SDEM_full=unlist(lapply(air20_SDEM_full,function(x)x$RMSE))
# RMSE_air20_naive_full=unlist(lapply(air20_naive_full,function(x)x$RMSE))
# 
# # Collect all results in one data frame
# RMSE_full=data.frame(stat_names,
#                      RMSE_air5_sarima_full,
#                      RMSE_air5_tkr_full,
#                      RMSE_air5_SDEM_full,
#                      RMSE_air5_naive_full,
#                      RMSE_air10_sarima_full,
#                      RMSE_air10_tkr_full,
#                      RMSE_air10_SDEM_full,
#                      RMSE_air10_naive_full,
#                      RMSE_air20_sarima_full,
#                      RMSE_air20_tkr_full,
#                      RMSE_air20_SDEM_full,
#                      RMSE_air20_naive_full)
# 
# colMeans(RMSE_full[,-1],na.rm = T)
# 
# # Plot
# 


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

# NEW linreg
air5_loess_lr=LOESS.df(air5_lr)
air10_loess_lr=LOESS.df(air10_lr)
air20_loess_lr=LOESS.df(air20_lr)

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

#NEW linreg
air5_hw_lr=HoltWint.df(air5_lr,24)
air10_hw_lr=HoltWint.df(air10_lr,24)
air20_hw_lr=HoltWint.df(air20_lr,24)

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

# save.image("~/Documents/git/ML4comfort/R/singapore/run_parallel.RData")

save.image("run_parallel2.RData")


# 5) NEW Ordinary kriging lin reg -----------------------------------------

indx=2:ncol(air_short)

# HW
start = Sys.time()
air5_lr_hw_res <- parallel::mclapply(indx,
                                         function(x)CV_STkr(x,
                                                            air5_hw_lr$residuals,
                                                            locations),
                                         mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_lr_hw_res=end-start

start = Sys.time()
air10_lr_hw_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air10_hw_lr$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_lr_hw_res=end-start

start = Sys.time()
air20_lr_hw_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air20_hw_lr$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_lr_hw_res=end-start

#LOESS
start = Sys.time()
air5_lr_loess_res <- parallel::mclapply(indx,
                                          function(x)CV_STkr(x,
                                                             air5_loess_lr$residuals,
                                                             locations),
                                          mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air5_lr_loess_res=end-start

start = Sys.time()
air10_lr_loess_res <- parallel::mclapply(indx,
                                           function(x)CV_STkr(x,
                                                              air10_loess_lr$residuals,
                                                              locations),
                                           mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air10_lr_loess_res=end-start

start = Sys.time()
air20_lr_loess_res <- parallel::mclapply(indx,
                                           function(x)CV_STkr(x,
                                                              air20_loess_lr$residuals,
                                                              locations),
                                           mc.cores = parallel::detectCores())
end = Sys.time()
elapsed_air20_lr_loess_res=end-start


# 6) Plots (station S100) ----------------------------------------------


# 5
time=air_short$time
air5_sarima_full_recover=df_recover(x=air5_sarima_full,
                                    locations2=locations2,time=time,residuals=F)
air5_tkr_full_recover=df_recover(x=air5_tkr_full,
                                 locations2=locations2,time=time,residuals=F)
# air5_SDEM_full_recover=df_recover(x=air5_SDEM_full,
#                                   locations2=locations2,time=time,residuals=F)
air5_naive_full_recover=df_recover(x=air5_naive_full,
                                   locations2=locations2,time=time,residuals=F)
air5_lr_full_recover=df_recover(x=air5_linreg_full,
                             locations2=locations2,time=time,residuals=F)

miss5=c(na_start[1],(na_start[1]+na_len[1]))
plot_air5_sarima_full_recover=rmse_detrdeseas(air5_sarima_full_recover$S100,
                                              air_short$S100,
                                              air_short$time,type="SARIMA - Full",
                                              miss=miss5)
plot_air5_tkr_full_recover=rmse_detrdeseas(air5_tkr_full_recover$S100,
                                           air_short$S100,
                                           air_short$time,type="TKR - Full",
                                           miss=miss5)
# plot_air5_SDEM_full_recover=rmse_detrdeseas(air5_SDEM_full_recover$S100,
#                                             air_short$S100,
#                                             air_short$time,type="SDEM - Full",
#                                             miss=miss5)
plot_air5_naive_full_recover=rmse_detrdeseas(air5_naive_full_recover$S100,
                                             air_short$S100,
                                             air_short$time,type="Naive - Full",
                                             miss=miss5)
plot_air5_lr_full_recover=rmse_detrdeseas(air5_lr_full_recover$S100,
                                          air_short$S100,
                                          air_short$time,type="LinReg - Full",
                                          miss=miss5)

rmse_detrdeseas(air5_sarima_full_recover$S100,
                air_short$S100,
                air_short$time,
                miss=miss5,
                plot=F)



PG_5full<- ggarrange(plot_air5_sarima_full_recover$plot,
                     plot_air5_tkr_full_recover$plot,
                     #plot_air5_SDEM_full_recover$plot,
                     plot_air5_lr_full_recover$plot,
                     plot_air5_naive_full_recover$plot,
                     ncol=2,nrow=2,
                     common.legend = T,
                     legend="bottom")

pdf("S100_5NA_full.pdf",paper="a4r",
    width = 11, height = 8)
windows()
annotate_figure(PG_5full, top = text_grob("S100", 
                                          color = "Black", face = "bold", size = 14))
dev.off()

# 10
air10_sarima_full_recover=df_recover(x=air10_sarima_full,
                                     locations2=locations2,time=time,residuals=F)
air10_tkr_full_recover=df_recover(x=air10_tkr_full,
                                  locations2=locations2,time=time,residuals=F)
# air10_SDEM_full_recover=df_recover(x=air10_SDEM_full,
#                                    locations2=locations2,time=time,residuals=F)
air10_lr_full_recover=df_recover(x=air10_linreg_full,
                                 locations2=locations2,time=time,residuals=F)
air10_naive_full_recover=df_recover(x=air10_naive_full,
                                    locations2=locations2,time=time,residuals=F)


miss10=c(na_start[2],(na_start[2]+na_len[2]))
plot_air10_sarima_full_recover=rmse_detrdeseas(air10_sarima_full_recover$S100,
                                               air_short$S100,
                                               air_short$time,type="SARIMA - Full",
                                               miss=miss10)
plot_air10_tkr_full_recover=rmse_detrdeseas(air10_tkr_full_recover$S100,
                                            air_short$S100,
                                            air_short$time,type="TKR - Full",
                                            miss=miss10)
# plot_air10_SDEM_full_recover=rmse_detrdeseas(air10_SDEM_full_recover$S100,
#                                              air_short$S100,
#                                              air_short$time,type="SDEM - Full",
#                                              miss=miss10)
plot_air10_naive_full_recover=rmse_detrdeseas(air10_naive_full_recover$S100,
                                              air_short$S100,
                                              air_short$time,type="Naive - Full",
                                              miss=miss10)
plot_air10_lr_full_recover=rmse_detrdeseas(air10_lr_full_recover$S100,
                                          air_short$S100,
                                          air_short$time,type="LinReg - Full",
                                          miss=miss10)

PG_10full<- ggarrange(plot_air10_sarima_full_recover$plot,
                      plot_air10_tkr_full_recover$plot,
                      #plot_air10_SDEM_full_recover$plot,
                      plot_air10_lr_full_recover$plot,
                      plot_air10_naive_full_recover$plot,
                      ncol=2,nrow=2,
                      common.legend = T,
                      legend="bottom")

pdf("S100_10NA_full.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_10full, top = text_grob("S100", 
                                           color = "Black", face = "bold", size = 14))
dev.off()

# 20
air20_sarima_full_recover=df_recover(x=air20_sarima_full,
                                     locations2=locations2,time=time,residuals=F)
air20_tkr_full_recover=df_recover(x=air20_tkr_full,
                                  locations2=locations2,time=time,residuals=F)
# air20_SDEM_full_recover=df_recover(x=air20_SDEM_full,
#                                    locations2=locations2,time=time,residuals=F)
air20_lr_full_recover=df_recover(x=air20_linreg_full,
                                 locations2=locations2,time=time,residuals=F)
air20_naive_full_recover=df_recover(x=air20_naive_full,
                                    locations2=locations2,time=time,residuals=F)

miss20=c(na_start[3],(na_start[3]+na_len[3]))
plot_air20_sarima_full_recover=rmse_detrdeseas(air20_sarima_full_recover$S100,
                                               air_short$S100,
                                               air_short$time,type="SARIMA - Full",
                                               miss=miss20)
plot_air20_tkr_full_recover=rmse_detrdeseas(air20_tkr_full_recover$S100,
                                            air_short$S100,
                                            air_short$time,type="TKR - Full",
                                            miss=miss20)
# plot_air20_SDEM_full_recover=rmse_detrdeseas(air20_SDEM_full_recover$S100,
#                                              air_short$S100,
#                                              air_short$time,type="SDEM - Full",
#                                              miss=miss20)
plot_air20_lr_full_recover=rmse_detrdeseas(air20_lr_full_recover$S100,
                                          air_short$S100,
                                          air_short$time,type="LinReg - Full",
                                          miss=miss20)
plot_air20_naive_full_recover=rmse_detrdeseas(air20_naive_full_recover$S100,
                                              air_short$S100,
                                              air_short$time,type="Naive - Full",
                                              miss=miss20)

PG_20full<- ggarrange(plot_air20_sarima_full_recover$plot,
                      plot_air20_tkr_full_recover$plot,
                      #plot_air20_SDEM_full_recover$plot,
                      plot_air20_lr_full_recover$plot,
                      plot_air20_naive_full_recover$plot,
                      ncol=2,nrow=2,
                      common.legend = T,
                      legend="bottom")


pdf("S100_20NA_full.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_20full, top = text_grob("S100", 
                                           color = "Black", face = "bold", size = 14))
dev.off()

# 5 HW
time=air_short$time[-(1:24)]
air5_sarima_hw_res_recover=df_recover(air5_sarima_hw_res,air5_hw_sarima,loess=F,locations2,time)
air5_tkr_hw_res_recover=df_recover(air5_tkr_hw_res,air5_hw_tkr,loess=F,locations2,time)
#air5_SDEM_hw_res_recover=df_recover(air5_SDEM_hw_res,air5_hw_SDEM,loess=F,locations2,time)
air5_lr_hw_res_recover=df_recover(air5_lr_hw_res,air5_hw_lr,loess=F,locations2,time)
air5_naive_hw_res_recover=df_recover(air5_naive_hw_res,air5_hw_naive,loess=F,locations2,time)

miss5=c(na_start[1],(na_start[1]+na_len[1]))
plot_air5_sarima_hw_res_recover=rmse_detrdeseas(air5_sarima_hw_res_recover$S100,
                                                air_short[-(1:24),]$S100,
                                                air_short$time[-(1:24)],type="ARIMA - HW",
                                                miss=miss5)
plot_air5_tkr_hw_res_recover=rmse_detrdeseas(air5_tkr_hw_res_recover$S100,
                                             air_short[-(1:24),]$S100,
                                             air_short$time[-(1:24)],type="TKR - HW",
                                             miss=miss5)
# plot_air5_SDEM_hw_res_recover=rmse_detrdeseas(air5_SDEM_hw_res_recover$S100,
#                                               air_short[-(1:24),]$S100,
#                                               air_short$time[-(1:24)],type="SDEM - HW",
#                                               miss=miss5)
plot_air5_lr_hw_res_recover=rmse_detrdeseas(air5_lr_hw_res_recover$S100,
                                           air_short[-(1:24),]$S100,
                                           air_short$time[-(1:24)],type="LinReg - HW",
                                           miss=miss5)
plot_air5_naive_hw_res_recover=rmse_detrdeseas(air5_naive_hw_res_recover$S100,
                                               air_short[-(1:24),]$S100,
                                               air_short$time[-(1:24)],type="Naive - HW",
                                               miss=miss5)


PG_5HW<- ggarrange(plot_air5_sarima_hw_res_recover$plot,
                   plot_air5_tkr_hw_res_recover$plot,
                   #plot_air5_SDEM_hw_res_recover$plot,
                   plot_air5_lr_hw_res_recover$plot,
                   plot_air5_naive_hw_res_recover$plot,
                   ncol=2,nrow=2,
                   common.legend = T,
                   legend="bottom")

pdf("S100_5NA_HW.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_5HW, top = text_grob("S100", 
                                        color = "Black", face = "bold", size = 14))
dev.off()


# 5 loess
time=air_short$time
air5_sarima_loess_res_recover=df_recover(air5_sarima_loess_res,air5_loess_sarima,loess=T,locations2,time)
air5_tkr_loess_res_recover=df_recover(air5_tkr_loess_res,air5_loess_tkr,loess=T,locations2,time)
#air5_SDEM_loess_res_recover=df_recover(air5_SDEM_loess_res,air5_loess_SDEM,loess=T,locations2,time)
air5_lr_loess_res_recover=df_recover(air5_lr_loess_res,air5_loess_lr,loess=T,locations2,time)
air5_naive_loess_res_recover=df_recover(air5_naive_loess_res,air5_loess_naive,loess=T,locations2,time)

miss5=c(na_start[1],(na_start[1]+na_len[1]))
plot_air5_sarima_loess_res_recover=rmse_detrdeseas(air5_sarima_loess_res_recover$S100,
                                                   air_short$S100,
                                                   air_short$time,type="ARIMA - LOESS",
                                                   miss=miss5)
plot_air5_tkr_loess_res_recover=rmse_detrdeseas(air5_tkr_loess_res_recover$S100,
                                                air_short$S100,
                                                air_short$time,type="TKR - LOESS",
                                                miss=miss5)
# plot_air5_SDEM_loess_res_recover=rmse_detrdeseas(air5_SDEM_loess_res_recover$S100,
#                                                  air_short$S100,
#                                                  air_short$time,type="SDEM - LOESS",
#                                                  miss=miss5)
plot_air5_lr_loess_res_recover=rmse_detrdeseas(air5_lr_loess_res_recover$S100,
                                              air_short$S100,
                                              air_short$time,type="LinReg - LOESS",
                                              miss=miss5)
plot_air5_naive_loess_res_recover=rmse_detrdeseas(air5_naive_loess_res_recover$S100,
                                                  air_short$S100,
                                                  air_short$time,type="Naive - LOESS",
                                                  miss=miss5)


PG_5LOESS<- ggarrange(plot_air5_sarima_loess_res_recover$plot,
                      plot_air5_tkr_loess_res_recover$plot,
                      #plot_air5_SDEM_loess_res_recover$plot,
                      plot_air5_lr_loess_res_recover$plot,
                      plot_air5_naive_loess_res_recover$plot,
                      ncol=2,nrow=2,
                      common.legend = T,
                      legend="bottom")

pdf("S100_5NA_LOESS.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_5LOESS, top = text_grob("S100", 
                                           color = "Black", face = "bold", size = 14))
dev.off()


# 10 HW
time=air_short$time[-(1:24)]
air10_sarima_hw_res_recover=df_recover(air10_sarima_hw_res,air10_hw_sarima,loess=F,locations2,time)
air10_tkr_hw_res_recover=df_recover(air10_tkr_hw_res,air10_hw_tkr,loess=F,locations2,time)
#air10_SDEM_hw_res_recover=df_recover(air10_SDEM_hw_res,air10_hw_SDEM,loess=F,locations2,time)
air10_lr_hw_res_recover=df_recover(air10_lr_hw_res,air10_hw_lr,loess=F,locations2,time)
air10_naive_hw_res_recover=df_recover(air10_naive_hw_res,air10_hw_naive,loess=F,locations2,time)

miss10=c(na_start[2],(na_start[2]+na_len[2]))

plot_air10_sarima_hw_res_recover=rmse_detrdeseas(air10_sarima_hw_res_recover$S100,
                                                 air_short[-(1:24),]$S100,
                                                 air_short$time[-(1:24)],type="ARIMA - HW",
                                                 miss=miss10)
plot_air10_tkr_hw_res_recover=rmse_detrdeseas(air10_tkr_hw_res_recover$S100,
                                              air_short[-(1:24),]$S100,
                                              air_short$time[-(1:24)],type="TKR - HW",
                                              miss=miss10)
# plot_air10_SDEM_hw_res_recover=rmse_detrdeseas(air10_SDEM_hw_res_recover$S100,
#                                                air_short[-(1:24),]$S100,
#                                                air_short$time[-(1:24)],type="SDEM - HW",
#                                                miss=miss10)
plot_air10_lr_hw_res_recover=rmse_detrdeseas(air10_lr_hw_res_recover$S100,
                                             air_short[-(1:24),]$S100,
                                             air_short$time[-(1:24)],type="LinReg - HW",
                                             miss=miss10)
plot_air10_naive_hw_res_recover=rmse_detrdeseas(air10_naive_hw_res_recover$S100,
                                                air_short[-(1:24),]$S100,
                                                air_short$time[-(1:24)],type="Naive - HW",
                                                miss=miss10)

PG_10HW<- ggarrange(plot_air10_sarima_hw_res_recover$plot,
                    plot_air10_tkr_hw_res_recover$plot,
                    #plot_air10_SDEM_hw_res_recover$plot,
                    plot_air10_lr_hw_res_recover$plot,
                    plot_air10_naive_hw_res_recover$plot,
                    ncol=2,nrow=2,
                    common.legend = T,
                    legend="bottom")

pdf("S100_10NA_HW.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_10HW, top = text_grob("S100", 
                                         color = "Black", face = "bold", size = 14))
dev.off()

# 10 loess
time=air_short$time
air10_sarima_loess_res_recover=df_recover(air10_sarima_loess_res,air10_loess_sarima,loess=T,locations2,time)
air10_tkr_loess_res_recover=df_recover(air10_tkr_loess_res,air10_loess_tkr,loess=T,locations2,time)
#air10_SDEM_loess_res_recover=df_recover(air10_SDEM_loess_res,air10_loess_SDEM,loess=T,locations2,time)
air10_lr_loess_res_recover=df_recover(air10_lr_loess_res,air10_loess_lr,loess=T,locations2,time)
air10_naive_loess_res_recover=df_recover(air10_naive_loess_res,air10_loess_naive,loess=T,locations2,time)

miss10=c(na_start[2],(na_start[2]+na_len[2]))
plot_air10_sarima_loess_res_recover=rmse_detrdeseas(air10_sarima_loess_res_recover$S100,
                                                    air_short$S100,
                                                    air_short$time,type="ARIMA - LOESS",
                                                    miss=miss10)
plot_air10_tkr_loess_res_recover=rmse_detrdeseas(air10_tkr_loess_res_recover$S100,
                                                 air_short$S100,
                                                 air_short$time,type="TKR - LOESS",
                                                 miss=miss10)
# plot_air10_SDEM_loess_res_recover=rmse_detrdeseas(air10_SDEM_loess_res_recover$S100,
#                                                   air_short$S100,
#                                                   air_short$time,type="SDEM - LOESS",
#                                                   miss=miss10)
plot_air10_lr_loess_res_recover=rmse_detrdeseas(air10_lr_loess_res_recover$S100,
                                                air_short$S100,
                                                air_short$time,type="LinReg - LOESS",
                                                miss=miss10)
plot_air10_naive_loess_res_recover=rmse_detrdeseas(air10_naive_loess_res_recover$S100,
                                                   air_short$S100,
                                                   air_short$time,type="Naive - LOESS",
                                                   miss=miss10)

PG_10LOESS<- ggarrange(plot_air10_sarima_loess_res_recover$plot,
                       plot_air10_tkr_loess_res_recover$plot,
                       #plot_air10_SDEM_loess_res_recover$plot,
                       plot_air10_lr_loess_res_recover$plot,
                       plot_air10_naive_loess_res_recover$plot,
                       ncol=2,nrow=2,
                       common.legend = T,
                       legend="bottom")

pdf("S100_10NA_LOESS.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_10LOESS, top = text_grob("S100", 
                                            color = "Black", face = "bold", size = 14))
dev.off()

# 20 HW
time=air_short$time[-(1:24)]
air20_sarima_hw_res_recover=df_recover(air20_sarima_hw_res,air20_hw_sarima,loess=F,locations2,time)
air20_tkr_hw_res_recover=df_recover(air20_tkr_hw_res,air20_hw_tkr,loess=F,locations2,time)
#air20_SDEM_hw_res_recover=df_recover(air20_SDEM_hw_res,air20_hw_SDEM,loess=F,locations2,time)
air20_lr_hw_res_recover=df_recover(air20_lr_hw_res,air20_hw_lr,loess=F,locations2,time)
air20_naive_hw_res_recover=df_recover(air20_naive_hw_res,air20_hw_naive,loess=F,locations2,time)

miss20=c(na_start[3],(na_start[3]+na_len[3]))

plot_air20_sarima_hw_res_recover=rmse_detrdeseas(air20_sarima_hw_res_recover$S100,
                                                 air_short[-(1:24),]$S100,
                                                 air_short$time[-(1:24)],type="ARIMA - HW",
                                                 miss=miss20)
plot_air20_tkr_hw_res_recover=rmse_detrdeseas(air20_tkr_hw_res_recover$S100,
                                              air_short[-(1:24),]$S100,
                                              air_short$time[-(1:24)],type="TKR - HW",
                                              miss=miss20)
# plot_air20_SDEM_hw_res_recover=rmse_detrdeseas(air20_SDEM_hw_res_recover$S100,
#                                                air_short[-(1:24),]$S100,
#                                                air_short$time[-(1:24)],type="SDEM - HW",
#                                                miss=miss20)
plot_air20_lr_hw_res_recover=rmse_detrdeseas(air20_lr_hw_res_recover$S100,
                                             air_short[-(1:24),]$S100,
                                             air_short$time[-(1:24)],type="LinReg - HW",
                                             miss=miss20)
plot_air20_naive_hw_res_recover=rmse_detrdeseas(air20_naive_hw_res_recover$S100,
                                                air_short[-(1:24),]$S100,
                                                air_short$time[-(1:24)],type="Naive - HW",
                                                miss=miss20)

PG_20HW<- ggarrange(plot_air20_sarima_hw_res_recover$plot,
                    plot_air20_tkr_hw_res_recover$plot,
                    #plot_air20_SDEM_hw_res_recover$plot,
                    plot_air20_lr_hw_res_recover$plot,
                    plot_air20_naive_hw_res_recover$plot,
                    ncol=2,nrow=2,
                    common.legend = T,
                    legend="bottom")

pdf("S100_20NA_HW.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_20HW, top = text_grob("S100", 
                                         color = "Black", face = "bold", size = 14))
dev.off()

# 20 loess
time=air_short$time
air20_sarima_loess_res_recover=df_recover(air20_sarima_loess_res,air20_loess_sarima,loess=T,locations2,time)
air20_tkr_loess_res_recover=df_recover(air20_tkr_loess_res,air20_loess_tkr,loess=T,locations2,time)
#air20_SDEM_loess_res_recover=df_recover(air20_SDEM_loess_res,air20_loess_SDEM,loess=T,locations2,time)
air20_lr_loess_res_recover=df_recover(air20_lr_loess_res,air20_loess_lr,loess=T,locations2,time)
air20_naive_loess_res_recover=df_recover(air20_naive_loess_res,air20_loess_naive,loess=T,locations2,time)

miss20=c(na_start[3],(na_start[3]+na_len[3]))

plot_air20_sarima_loess_res_recover=rmse_detrdeseas(air20_sarima_loess_res_recover$S100,
                                                    air_short$S100,
                                                    air_short$time,type="ARIMA - LOESS",
                                                    miss=miss20)
plot_air20_tkr_loess_res_recover=rmse_detrdeseas(air20_tkr_loess_res_recover$S100,
                                                 air_short$S100,
                                                 air_short$time,type="TKR - LOESS",
                                                 miss=miss20)
# plot_air20_SDEM_loess_res_recover=rmse_detrdeseas(air20_SDEM_loess_res_recover$S100,
#                                                   air_short$S100,
#                                                   air_short$time,type="SDEM - LOESS",
#                                                   miss=miss20)
plot_air20_lr_loess_res_recover=rmse_detrdeseas(air20_lr_loess_res_recover$S100,
                                                air_short$S100,
                                                air_short$time,type="LinReg - LOESS",
                                                miss=miss20)
plot_air20_naive_loess_res_recover=rmse_detrdeseas(air20_naive_loess_res_recover$S100,
                                                   air_short$S100,
                                                   air_short$time,type="Naive - LOESS",
                                                   miss=miss20)

PG_20LOESS<- ggarrange(plot_air20_sarima_loess_res_recover$plot,
                       plot_air20_tkr_loess_res_recover$plot,
                       #plot_air20_SDEM_loess_res_recover$plot,
                       plot_air20_lr_loess_res_recover$plot,
                       plot_air20_naive_loess_res_recover$plot,
                       ncol=2,nrow=2,
                       common.legend = T,
                       legend="bottom")

pdf("S100_20NA_LOESS.pdf",paper="a4r",
    width = 11, height = 8)
annotate_figure(PG_20LOESS, top = text_grob("S100", 
                                            color = "Black", face = "bold", size = 14))

dev.off()


# 6.1) RMSE --------------------------------------------------------------------

# FULL
RMSE_air5_sarima_full=rmse_st(air5_sarima_full_recover,air_short,miss5)
RMSE_air5_tkr_full=rmse_st(air5_tkr_full_recover,air_short,miss5)
#RMSE_air5_SDEM_full=rmse_st(air5_SDEM_full_recover,air_short,miss5)
RMSE_air5_lr_full=rmse_st(air5_lr_full_recover,air_short,miss5)
RMSE_air5_naive_full=rmse_st(air5_naive_full_recover,air_short,miss5)

RMSE_air10_sarima_full=rmse_st(air10_sarima_full_recover,air_short,miss10)
RMSE_air10_tkr_full=rmse_st(air10_tkr_full_recover,air_short,miss10)
#RMSE_air10_SDEM_full=rmse_st(air10_SDEM_full_recover,air_short,miss10)
RMSE_air10_lr_full=rmse_st(air10_lr_full_recover,air_short,miss10)
RMSE_air10_naive_full=rmse_st(air10_naive_full_recover,air_short,miss10)

RMSE_air20_sarima_full=rmse_st(air20_sarima_full_recover,air_short,miss20)
RMSE_air20_tkr_full=rmse_st(air20_tkr_full_recover,air_short,miss20)
#RMSE_air20_SDEM_full=rmse_st(air20_SDEM_full_recover,air_short,miss20)
RMSE_air20_lr_full=rmse_st(air20_lr_full_recover,air_short,miss20)
RMSE_air20_naive_full=rmse_st(air20_naive_full_recover,air_short,miss20)

# stat_names=unlist(lapply(air5_sarima_full,function(x)x$stat_id)); stat_names
# 
# RMSE_air5_sarima_full=unlist(lapply(air5_sarima_full,function(x)x$RMSE))
# RMSE_air5_tkr_full=unlist(lapply(air5_tkr_full,function(x)x$RMSE))
# #RMSE_air5_SDEM_full=unlist(lapply(air5_SDEM_full,function(x)x$RMSE))
# RMSE_air5_lr_full=unlist(lapply(air5_linreg_full,function(x)x$RMSE))
# RMSE_air5_naive_full=unlist(lapply(air5_naive_full,function(x)x$RMSE))
# 
# RMSE_air10_sarima_full=unlist(lapply(air10_sarima_full,function(x)x$RMSE))
# RMSE_air10_tkr_full=unlist(lapply(air10_tkr_full,function(x)x$RMSE))
# #RMSE_air10_SDEM_full=unlist(lapply(air10_SDEM_full,function(x)x$RMSE))
# RMSE_air10_lr_full=unlist(lapply(air10_linreg_full,function(x)x$RMSE))
# RMSE_air10_naive_full=unlist(lapply(air10_naive_full,function(x)x$RMSE))
# 
# RMSE_air20_sarima_full=unlist(lapply(air20_sarima_full,function(x)x$RMSE))
# RMSE_air20_tkr_full=unlist(lapply(air20_tkr_full,function(x)x$RMSE))
# #RMSE_air20_SDEM_full=unlist(lapply(air20_SDEM_full,function(x)x$RMSE))
# RMSE_air20_lr_full=unlist(lapply(air20_linreg_full,function(x)x$RMSE))
# RMSE_air20_naive_full=unlist(lapply(air20_naive_full,function(x)x$RMSE))

# Collect all results in one data frame
RMSE_full=data.frame(stat_names,
                     RMSE_air5_sarima_full,
                     RMSE_air5_tkr_full,
                     #RMSE_air5_SDEM_full,
                     RMSE_air5_lr_full,
                     RMSE_air5_naive_full,
                     RMSE_air10_sarima_full,
                     RMSE_air10_tkr_full,
                     #RMSE_air10_SDEM_full,
                     RMSE_air10_lr_full,
                     RMSE_air10_naive_full,
                     RMSE_air20_sarima_full,
                     RMSE_air20_tkr_full,
                     #RMSE_air20_SDEM_full,
                     RMSE_air20_lr_full,
                     RMSE_air20_naive_full)

# Long format with tidyverse
RMSE_full_long=RMSE_full %>% gather(key="model",value="RMSE",2:13)
RMSE_full_long$Method=rep(rep(c("SARIMA","TKR","LinReg","Naive"),each=11),3)
RMSE_full_long$Type=rep("FULL",dim(RMSE_full_long)[1])
RMSE_full_long$NNA=rep(c(5,10,20),each=44)

#colMeans(RMSE_full[,-1],na.rm = T)

#HW
RMSE_air5_sarima_hw_res=rmse_st(air5_sarima_hw_res_recover,air_short,miss5)
RMSE_air5_tkr_hw_res=rmse_st(air5_tkr_hw_res_recover,air_short,miss5)
#RMSE_air5_SDEM_hw_res=rmse_st(air5_SDEM_hw_res_recover,air_short,miss5)
RMSE_air5_lr_hw_res=rmse_st(air5_lr_hw_res_recover,air_short,miss5)
RMSE_air5_naive_hw_res=rmse_st(air5_naive_hw_res_recover,air_short,miss5)

RMSE_air10_sarima_hw_res=rmse_st(air10_sarima_hw_res_recover,air_short,miss10)
RMSE_air10_tkr_hw_res=rmse_st(air10_tkr_hw_res_recover,air_short,miss10)
#RMSE_air10_SDEM_hw_res=rmse_st(air10_SDEM_hw_res_recover,air_short,miss10)
RMSE_air10_lr_hw_res=rmse_st(air10_lr_hw_res_recover,air_short,miss10)
RMSE_air10_naive_hw_res=rmse_st(air10_naive_hw_res_recover,air_short,miss10)

RMSE_air20_sarima_hw_res=rmse_st(air20_sarima_hw_res_recover,air_short,miss20)
RMSE_air20_tkr_hw_res=rmse_st(air20_tkr_hw_res_recover,air_short,miss20)
#RMSE_air20_SDEM_hw_res=rmse_st(air20_SDEM_hw_res_recover,air_short,miss20)
RMSE_air20_lr_hw_res=rmse_st(air20_lr_hw_res_recover,air_short,miss20)
RMSE_air20_naive_hw_res=rmse_st(air20_naive_hw_res_recover,air_short,miss20)

#LOESS
RMSE_air5_sarima_loess_res=rmse_st(air5_sarima_loess_res_recover,air_short,miss5)
RMSE_air5_tkr_loess_res=rmse_st(air5_tkr_loess_res_recover,air_short,miss5)
#RMSE_air5_SDEM_loess_res=rmse_st(air5_SDEM_loess_res_recover,air_short,miss5)
RMSE_air5_lr_loess_res=rmse_st(air5_lr_loess_res_recover,air_short,miss5)
RMSE_air5_naive_loess_res=rmse_st(air5_naive_loess_res_recover,air_short,miss5)

RMSE_air10_sarima_loess_res=rmse_st(air10_sarima_loess_res_recover,air_short,miss10)
RMSE_air10_tkr_loess_res=rmse_st(air10_tkr_loess_res_recover,air_short,miss10)
#RMSE_air10_SDEM_loess_res=rmse_st(air10_SDEM_loess_res_recover,air_short,miss10)
RMSE_air10_lr_loess_res=rmse_st(air10_lr_loess_res_recover,air_short,miss10)
RMSE_air10_naive_loess_res=rmse_st(air10_naive_loess_res_recover,air_short,miss10)

RMSE_air20_sarima_loess_res=rmse_st(air20_sarima_loess_res_recover,air_short,miss20)
RMSE_air20_tkr_loess_res=rmse_st(air20_tkr_loess_res_recover,air_short,miss20)
#RMSE_air20_SDEM_loess_res=rmse_st(air20_SDEM_loess_res_recover,air_short,miss20)
RMSE_air20_lr_loess_res=rmse_st(air20_lr_loess_res_recover,air_short,miss20)
RMSE_air20_naive_loess_res=rmse_st(air20_naive_loess_res_recover,air_short,miss20)

# RMSE_air5_sarima_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air5_sarima_hw_res_recover[,-1])^2)
# RMSE_air5_tkr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air5_tkr_hw_res_recover[,-1])^2)
# #RMSE_air5_SDEM_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air5_SDEM_hw_res_recover[,-1])^2)
# RMSE_air5_lr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air5_lr_hw_res_recover[,-1])^2)
# RMSE_air5_naive_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air5_naive_hw_res_recover[,-1])^2)
# 
# # RMSE 5 LOESS
# RMSE_air5_sarima_loess_res=sqrt(colMeans(air_short[,-1]-air5_sarima_loess_res_recover[,-1])^2)
# RMSE_air5_tkr_loess_res=sqrt(colMeans(air_short[,-1]-air5_tkr_loess_res_recover[,-1])^2)
# #RMSE_air5_SDEM_loess_res=sqrt(colMeans(air_short[,-1]-air5_SDEM_loess_res_recover[,-1])^2)
# RMSE_air5_lr_loess_res=sqrt(colMeans(air_short[,-1]-air5_lr_loess_res_recover[,-1])^2)
# RMSE_air5_naive_loess_res=sqrt(colMeans(air_short[,-1]-air5_naive_loess_res_recover[,-1])^2)
# 
# # arrange in dataframe
# # RMSE_air5_res=data.frame(stat_names,
# #   RMSE_air5_sarima_hw_res,
# #                          RMSE_air5_tkr_hw_res,
# #                          #RMSE_air5_SDEM_hw_res,
# #                          RMSE_air5_lr_hw_res,
# #                          RMSE_air5_naive_hw_res,
# #                          RMSE_air5_sarima_loess_res,
# #                          RMSE_air5_tkr_loess_res,
# #                          #RMSE_air5_SDEM_loess_res,
# #                          RMSE_air5_lr_loess_res,
# #                          RMSE_air5_naive_loess_res)
# 
# 
# # RMSE 10 HW
# RMSE_air10_sarima_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air10_sarima_hw_res_recover[,-1])^2)
# RMSE_air10_tkr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air10_tkr_hw_res_recover[,-1])^2)
# #RMSE_air10_SDEM_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air10_SDEM_hw_res_recover[,-1])^2)
# RMSE_air10_lr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air10_lr_hw_res_recover[,-1])^2)
# RMSE_air10_naive_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air10_naive_hw_res_recover[,-1])^2)
# 
# # RMSE 10 LOESS
# RMSE_air10_sarima_loess_res=sqrt(colMeans(air_short[,-1]-air10_sarima_loess_res_recover[,-1])^2)
# RMSE_air10_tkr_loess_res=sqrt(colMeans(air_short[,-1]-air10_tkr_loess_res_recover[,-1])^2)
# #RMSE_air10_SDEM_loess_res=sqrt(colMeans(air_short[,-1]-air10_SDEM_loess_res_recover[,-1])^2)
# RMSE_air10_lr_loess_res=sqrt(colMeans(air_short[,-1]-air10_lr_loess_res_recover[,-1])^2)
# RMSE_air10_naive_loess_res=sqrt(colMeans(air_short[,-1]-air10_naive_loess_res_recover[,-1])^2)
# 
# # arrange in dataframe
# # RMSE_air10_res=data.frame(RMSE_air10_sarima_hw_res,
# #                           RMSE_air10_tkr_hw_res,
# #                           #RMSE_air10_SDEM_hw_res,
# #                           RMSE_air10_lr_hw_res,
# #                           RMSE_air10_naive_hw_res,
# #                           RMSE_air10_sarima_loess_res,
# #                           RMSE_air10_tkr_loess_res,
# #                           #RMSE_air10_SDEM_loess_res,
# #                           RMSE_air10_lr_loess_res,
# #                           RMSE_air10_naive_loess_res)
# 
# 
# 
# # RMSE 20 hw
# RMSE_air20_sarima_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air20_sarima_hw_res_recover[,-1])^2)
# RMSE_air20_tkr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air20_tkr_hw_res_recover[,-1])^2)
# #RMSE_air20_SDEM_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air20_SDEM_hw_res_recover[,-1])^2)
# RMSE_air20_lr_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air20_lr_hw_res_recover[,-1])^2)
# RMSE_air20_naive_hw_res=sqrt(colMeans(air_short[-(1:24),-1]-air20_naive_hw_res_recover[,-1])^2)
# 
# # RMSE 20 LOESS
# RMSE_air20_sarima_loess_res=sqrt(colMeans(air_short[,-1]-air20_sarima_loess_res_recover[,-1])^2)
# RMSE_air20_tkr_loess_res=sqrt(colMeans(air_short[,-1]-air20_tkr_loess_res_recover[,-1])^2)
# #RMSE_air20_SDEM_loess_res=sqrt(colMeans(air_short[,-1]-air20_SDEM_loess_res_recover[,-1])^2)
# RMSE_air20_lr_loess_res=sqrt(colMeans(air_short[,-1]-air20_lr_loess_res_recover[,-1])^2)
# RMSE_air20_naive_loess_res=sqrt(colMeans(air_short[,-1]-air20_naive_loess_res_recover[,-1])^2)
# 
# # arrange in dataframe
# # RMSE_air20_res=data.frame(RMSE_air20_sarima_hw_res,
# #                           RMSE_air20_tkr_hw_res,
# #                           #RMSE_air20_SDEM_hw_res,
# #                           RMSE_air20_lr_hw_res,
# #                           RMSE_air20_naive_hw_res,
# #                           RMSE_air20_sarima_loess_res,
# #                           RMSE_air20_tkr_loess_res,
# #                           #RMSE_air20_SDEM_loess_res,
# #                           RMSE_air20_lr_loess_res,
# #                           RMSE_air20_naive_loess_res)

#HW
RMSE_HW=data.frame(stat_names,
                   RMSE_air5_sarima_hw_res,
                   RMSE_air5_tkr_hw_res,
                   #RMSE_air5_SDEM_hw_res,
                   RMSE_air5_lr_hw_res,
                   RMSE_air5_naive_hw_res,
                   RMSE_air10_sarima_hw_res,
                   RMSE_air10_tkr_hw_res,
                   #RMSE_air10_SDEM_hw_res,
                   RMSE_air10_lr_hw_res,
                   RMSE_air10_naive_hw_res,
                   RMSE_air20_sarima_hw_res,
                   RMSE_air20_tkr_hw_res,
                   #RMSE_air20_SDEM_hw_res,
                   RMSE_air20_lr_hw_res,
                   RMSE_air20_naive_hw_res)

# Long format with tidyverse
RMSE_HW_long=RMSE_HW %>% gather(key="model",value="RMSE",2:13)
RMSE_HW_long$Method=rep(rep(c("SARIMA","TKR","LinReg","Naive"),each=11),3)
RMSE_HW_long$Type=rep("HW",dim(RMSE_HW_long)[1])
RMSE_HW_long$NNA=rep(c(5,10,20),each=44)


RMSE_LOESS=data.frame(stat_names,
                      RMSE_air5_sarima_loess_res,
                      RMSE_air5_tkr_loess_res,
                      #RMSE_air5_SDEM_loess_res,
                      RMSE_air5_lr_loess_res,
                      RMSE_air5_naive_loess_res,
                      RMSE_air10_sarima_loess_res,
                      RMSE_air10_tkr_loess_res,
                      #RMSE_air10_SDEM_loess_res,
                      RMSE_air10_lr_loess_res,
                      RMSE_air10_naive_loess_res,
                      RMSE_air20_sarima_loess_res,
                      RMSE_air20_tkr_loess_res,
                      #RMSE_air20_SDEM_loess_res,
                      RMSE_air20_lr_loess_res,
                      RMSE_air20_naive_loess_res)

#Long format with tidyverse
RMSE_LOESS_long=RMSE_LOESS %>% gather(key="model",value="RMSE",2:13)
RMSE_LOESS_long$Method=rep(rep(c("SARIMA","TKR","LinReg","Naive"),each=11),3)
RMSE_LOESS_long$Type=rep("LOESS",dim(RMSE_LOESS_long)[1])
RMSE_LOESS_long$NNA=rep(c(5,10,20),each=44)

# # CV averages
# colMeans(RMSE_full[,-1],na.rm = T)
# colMeans(RMSE_air5_res,na.rm = T)
# colMeans(RMSE_air10_res,na.rm = T)
# colMeans(RMSE_air20_res,na.rm = T)

# rbind RMSE_FULL_long and RMSE_HW_long and RMSE_LOESS_long
RMSE_long=rbind(RMSE_full_long,RMSE_HW_long,RMSE_LOESS_long)

# Transform character to factor
RMSE_long$Method=factor(RMSE_long$Method,levels = c("SARIMA","TKR","LinReg","Naive"),ordered = F)
RMSE_long$Type=factor(RMSE_long$Type,levels = c("FULL","HW","LOESS"),ordered = F)
RMSE_long$stat_names=factor(RMSE_long$stat_names)

# 6.1.1) Summary stats of RMSEs --------------------------------------------------

# Extract RMSEs for type=5
RMSE_5=RMSE_long[RMSE_long$NNA==5,]

# Compute median values for each Type and method 
RMSE_5 %>% group_by(Type,Method) %>% summarise(Median=median(RMSE,na.rm = T))

# Remove FULL
# RMSE_5_nofull=RMSE_5[RMSE_5$Type!="FULL",]

# Boxplot by Type and Method
bxplt5=ggplot(RMSE_5, aes(x=factor(Method),y=RMSE,fill=Type)) + 
  geom_boxplot()+
  theme_bw()+
  facet_grid(. ~ Method, scales = "free", space = "free")+
  labs(y=" ",x=" ",title="5% NAs")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    title =element_text(face="bold",hjust = .5),
    axis.text.x=element_blank())+ 
  scale_fill_discrete(name = " ")+
  scale_fill_brewer(palette="PuRd")

# Extract RMSEs for type=10
RMSE_10=RMSE_long[RMSE_long$NNA==10,]

# Compute median values for each Type and method
RMSE_10 %>% group_by(Type,Method) %>% summarise(Median=median(RMSE,na.rm = T))

# Remove FULL
# RMSE_10_nofull=RMSE_10[RMSE_10$Type!="FULL",]

# Boxplot by Type and Method
bxplt10=ggplot(RMSE_10, aes(x=factor(Method),y=RMSE,fill=Type)) + 
  geom_boxplot()+
  theme_bw()+
  facet_grid(. ~ Method, scales = "free", space = "free")+
  labs(y=" ",x=" ",title="10% NAs")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
    title =element_text(face="bold",hjust = .5),
    axis.text.x=element_blank())+ 
  scale_fill_discrete(name = " ")+
  scale_fill_brewer(palette="PuRd")

# Extract RMSEs for type=20
RMSE_20=RMSE_long[RMSE_long$NNA==20,]

# Compute median values for each Type and method
RMSE_20 %>% group_by(Type,Method) %>% summarise(Median=median(RMSE,na.rm = T))

# Remove FULL
# RMSE_20_nofull=RMSE_20[RMSE_20$Type!="FULL",]

# Boxplot by Type and Method
bxplt20=ggplot(RMSE_20, aes(x=factor(Method),y=RMSE,fill=Type)) + 
  geom_boxplot()+
  theme_bw()+
  facet_grid(. ~ Method, scales = "free", space = "free")+
  labs(y=" ",x=" ",title="20% NAs")+
  theme(
    #axis.text.x = element_text(angle = 45, hjust = 1),
        title =element_text(face="bold",hjust = .5),
        axis.text.x=element_blank())+ 
  scale_fill_discrete(name = " ")+
  scale_fill_brewer(palette="PuRd")


#  6.1.2) Boxplots --------------------------------------------------------

pdf("RMSE_boxplots.pdf",paper="a4r",
    width = 11, height = 8)
ggarrange(bxplt5,bxplt10,bxplt20,ncol=3,common.legend = T,legend="bottom")
dev.off()

# # Old ---------------------------------------------------------------------
# 
# 
# 
# # Put the above colmeans in a dataframe
# RMSE_means=data.frame(Method=c("SARIMA-FULL-5","TKR-FULL-5","LinReg-FULL-5","Naive-FULL-5",
#                                "SARIMA-FULL-10","TKR-FULL-10","LinReg-FULL-10","Naive-FULL-10",
#                                "SARIMA-FULL-20","TKR-FULL-20","LinReg-FULL-20","Naive-FULL-20",
#                                "SARIMA-HW-5","TKR-HW-5","LinReg-HW-5","Naive-HW-5",
#                                "SARIMA-LOESS-5","TKR-LOESS-5","LinReg-LOESS-5","Naive-LOESS-5",
#                                "SARIMA-HW-10","TKR-HW-10","LinReg-HW-10","Naive-HW-10",
#                                "SARIMA-LOESS-10","TKR-LOESS-10","LinReg-LOESS-10","Naive-LOESS-10",
#                                "SARIMA-HW-20","TKR-HW-20","LinReg-HW-20","Naive-HW-20",
#                                "SARIMA-LOESS-20","TKR-LOESS-20","LinReg-LOESS-20","Naive-LOESS-20"),
#                       RMSE=c(colMeans(RMSE_full[,-1],na.rm = T),
#                              colMeans(RMSE_air5_res,na.rm = T),
#                              colMeans(RMSE_air10_res,na.rm = T),
#                              colMeans(RMSE_air20_res,na.rm = T)))
# 
# # Add column 5 10 or 20 according to the % of missings
# RMSE_means$Type=rep(c("5","10","20"),each=4)
# 
# # Add column with factors "FULL", "HW" or "LOESS"
# RMSE_means$Method=factor(c(rep("FULL",12),rep(rep(c("HW","LOESS"),each=4),3)))
# 
# RMSE_means
# 
# # 6.2) Boxplots -----------------------------------------------------------------
# 
# ### 5% NAs###
# # dataframe with results for 5% NAs
# RMSE_5=data.frame(stat_names,
#                   SARIMA_FULL=RMSE_air5_sarima_full,
#                   TKR_FULL=RMSE_air5_tkr_full,
#                   #SDEM_FULL=RMSE_air5_SDEM_full,
#                   LinReg_FULL=RMSE_air5_lr_full,
#                   Naive_FULL=RMSE_air5_naive_full,
#                   ARIMA_HW=RMSE_air5_sarima_hw_res,
#                   TKR_HW=RMSE_air5_tkr_hw_res,
#                   #SDEM_HW=RMSE_air5_SDEM_hw_res,
#                   LinReg_HW=RMSE_air5_lr_hw_res,
#                   Naive_HW=RMSE_air5_naive_hw_res,
#                   ARIMA_LOESS=RMSE_air5_sarima_loess_res,
#                   TKR_LOESS=RMSE_air5_tkr_loess_res,
#                   #SDEM_LOESS=RMSE_air5_SDEM_loess_res,
#                   LinReg_LOESS=RMSE_air5_lr_loess_res,
#                   Naive_LOESS=RMSE_air5_naive_loess_res)
# 
# # Long format with columns RMSE with the RMSE value, Method with the method name and Stat with the station name usign dplyr
# RMSE_5_long=RMSE_5 %>% gather(Method, RMSE, -stat_names)
# 
# # Recode method column substituting "_" with "-"
# RMSE_5_long$Method=gsub("_","-",RMSE_5_long$Method)
# 
# # Add column "Type" with factor levels "Full" or "Residuals" (the latter for HW and LOESS)
# RMSE_5_long$Type=ifelse(grepl("FULL",RMSE_5_long$Method),"Full","Residuals")
# RMSE_5_long$Type=factor(RMSE_5_long$Type,
#                         levels = c('Full','Residuals'),ordered = TRUE)
# 
# 
# bxplt5=ggplot(RMSE_5_long, aes(x=factor(Method,level=c("SARIMA-FULL","ARIMA-HW","ARIMA-LOESS",
#                                                        "TKR-FULL","TKR-HW","TKR-LOESS",
#                                                        "LinReg-FULL","LinReg-HW","LinReg-LOESS",
#                                                        "Naive-FULL",
#                                                        "Naive-HW",
#                                                        "Naive-LOESS")),
#                                y=RMSE,fill=Type)) + 
#   geom_boxplot()+
#   theme_bw()+
#   labs(y="RMSE",x=" ")+
#   theme(text = element_text(size = 12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size=12),
#         axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=.5),legend.position = "none")+
#   scale_fill_manual(values=c("grey70", "grey40"))
# 
# pdf("boxplot_5.pdf",paper="a4r",
#     width = 11, height = 8)
# annotate_figure(bxplt5, top = text_grob("5% missing", 
#                                         color = "Black", face = "bold", size = 14))
# dev.off()
# 
# 
# ### 10% NAs
# # dataframe with results for 10% NAs
# RMSE_10=data.frame(stat_names,
#                    SARIMA_FULL=RMSE_air10_sarima_full,
#                    TKR_FULL=RMSE_air10_tkr_full,
#                    #SDEM_FULL=RMSE_air10_SDEM_full,
#                    LinReg_FULL=RMSE_air10_lr_full,
#                    Naive_FULL=RMSE_air10_naive_full,
#                    ARIMA_HW=RMSE_air10_sarima_hw_res,
#                    TKR_HW=RMSE_air10_tkr_hw_res,
#                    #SDEM_HW=RMSE_air10_SDEM_hw_res,
#                    LinReg_HW=RMSE_air10_lr_hw_res,
#                    Naive_HW=RMSE_air10_naive_hw_res,
#                    ARIMA_LOESS=RMSE_air10_sarima_loess_res,
#                    TKR_LOESS=RMSE_air10_tkr_loess_res,
#                    #SDEM_LOESS=RMSE_air10_SDEM_loess_res,
#                    LinReg_LOESS=RMSE_air10_lr_loess_res,
#                    Naive_LOESS=RMSE_air10_naive_loess_res)
# 
# # Long format with columns RMSE with the RMSE value, Method with the method name and Stat with the station name usign dplyr
# RMSE_10_long=RMSE_10 %>% gather(Method, RMSE, -stat_names)
# 
# # Recode method column substituting "_" with "-"
# RMSE_10_long$Method=gsub("_","-",RMSE_10_long$Method)
# 
# # Add column "Type" with factor levels "Full" or "Residuals" (the latter for HW and LOESS)
# RMSE_10_long$Type=ifelse(grepl("FULL",RMSE_10_long$Method),"Full","Residuals")
# RMSE_10_long$Type=factor(RMSE_10_long$Type,
#                          levels = c('Full','Residuals'),ordered = TRUE)
# 
# bxplt10=ggplot(RMSE_10_long, aes(x=factor(Method,level=c("SARIMA-FULL","ARIMA-HW","ARIMA-LOESS",
#                                                          "TKR-FULL","TKR-HW","TKR-LOESS",
#                                                          "LinReg-FULL","LinReg-HW","LinReg-LOESS",
#                                                          "Naive-FULL",
#                                                          "Naive-HW",
#                                                          "Naive-LOESS")),
#                                  y=RMSE,fill=Type)) +
#   geom_boxplot()+
#   theme_bw()+
#   labs(y="RMSE",x=" ")+
#   theme(text = element_text(size = 12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size=12),
#         axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=.5),legend.position = "none")+
#   scale_fill_manual(values=c("grey70", "grey40"))
# 
# pdf("boxplot_10.pdf",paper="a4r",
#     width = 11, height = 8)
# annotate_figure(bxplt10, top = text_grob("10% missing", 
#                                          color = "Black", face = "bold", size = 14))
# dev.off()
# 
# ### 20% NAs
# # dataframe with results for 20% NAs
# RMSE_20=data.frame(stat_names,
#                    SARIMA_FULL=RMSE_air20_sarima_full,
#                    TKR_FULL=RMSE_air20_tkr_full,
#                    #SDEM_FULL=RMSE_air20_SDEM_full,
#                    LinReg_FULL=RMSE_air20_lr_full,
#                    Naive_FULL=RMSE_air20_naive_full,
#                    ARIMA_HW=RMSE_air20_sarima_hw_res,
#                    TKR_HW=RMSE_air20_tkr_hw_res,
#                    #SDEM_HW=RMSE_air20_SDEM_hw_res,
#                    LinReg_HW=RMSE_air20_lr_hw_res,
#                    Naive_HW=RMSE_air20_naive_hw_res,
#                    ARIMA_LOESS=RMSE_air20_sarima_loess_res,
#                    TKR_LOESS=RMSE_air20_tkr_loess_res,
#                    #SDEM_LOESS=RMSE_air20_SDEM_loess_res,
#                    LinReg_LOESS=RMSE_air20_lr_loess_res,
#                    Naive_LOESS=RMSE_air20_naive_loess_res)
# 
# # Long format with columns RMSE with the RMSE value, Method with the method name and Stat with the station name usign dplyr
# RMSE_20_long=RMSE_20 %>% gather(Method, RMSE, -stat_names)
# 
# # Recode method column substituting "_" with "-"
# RMSE_20_long$Method=gsub("_","-",RMSE_20_long$Method)
# 
# # Add column "Type" with factor levels "Full" or "Residuals" (the latter for HW and LOESS)
# RMSE_20_long$Type=ifelse(grepl("FULL",RMSE_20_long$Method),"Full","Residuals")
# RMSE_20_long$Type=factor(RMSE_20_long$Type,
#                          levels = c('Full','Residuals'),ordered = TRUE)
# 
# bxplt20=ggplot(RMSE_20_long, aes(x=factor(Method,level=c("SARIMA-FULL","ARIMA-HW","ARIMA-LOESS",
#                                                          "TKR-FULL","TKR-HW","TKR-LOESS",
#                                                          "LinReg-FULL","LinReg-HW","LinReg-LOESS",
#                                                          "Naive-FULL",
#                                                          "Naive-HW",
#                                                          "Naive-LOESS")),
#                                  y=RMSE,fill=Type)) +
#   geom_boxplot()+
#   theme_bw()+
#   labs(y="RMSE",x=" ")+
#   theme(text = element_text(size = 12),
#         legend.text=element_text(size=12),
#         axis.text = element_text(size=12),
#         axis.text.x = element_text(angle = 45,vjust = 0.5, hjust=.5)
#         ,legend.position = "none")+
#   scale_fill_manual(values=c("grey70", "grey40"))
# 
# pdf("boxplot_20.pdf",paper="a4r",
#     width = 11, height = 8)
# annotate_figure(bxplt20, top = text_grob("20% missing", 
#                                          color = "Black", face = "bold", size = 14))
# dev.off()
# 
# 

