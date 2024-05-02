
load("locations.Rdata")
#load("air_short.Rdata")
#Desktop
#load("20240430.Rdata")

#dat_temp=air_short[1:200,]


# Imputation --------------------------------------------------------------

fill_sarima=function(x,period){
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

temp_krige=function(x){
  x$knot=1
  x$indx=1:nrow(x)
  x_sp=x
  
  temp_grid=1:nrow(x)
  temp_grid=expand.grid(temp_grid,1)
  colnames(temp_grid)=c("indx","knot")
  gridded(temp_grid)=~indx+knot
  
  for(i in 2:ncol(x)){
    #i=17
    x_sp=x[,c(1,i)]
    ind_fill=which(is.na(x_sp[,2]))
    NAcount=sum(is.na(x_sp[,2]))
    
    if(NAcount>0){
      colnames(x_sp)=c("time","z")
      x_sp$indx=1:nrow(x_sp)
      x_sp$knot=1
      x_sp=drop_na(x_sp)
      coordinates(x_sp) <- ~ indx + knot
      
      onedim_krig = autoKrige(z~1, x_sp,temp_grid)
      x[ind_fill,i]=onedim_krig$krige_output$var1.pred[ind_fill]
    }
  }
  return(x)
}

MA_imp=function(x_data){
  
  # x_data is a data frame with time as first column. Each column is a station
  
  names(x_data)[1]="time"
  overall_mean=mean(as.matrix(x_data[,-1]),na.rm=T)
  
  #as.factor(weekdays(x_data$time))
  x_data$hour=as.factor(hour(x_data$time))
  #week(x_data$time)
  #month(x_data$time)
  
  xsw_bar=x_data%>%
    group_by(hour)%>%
    summarise_if(is.numeric,mean,na.rm=T)
  colnames(xsw_bar)[-1]=paste0(colnames(xsw_bar)[-1],"mean")
  
  xsw_bar2=merge(xsw_bar,x_data[,c("time","hour")],by="hour")
  imput.naive=xsw_bar2[order(xsw_bar2$time),]
  
  xsw=(xsw_bar[,-1]-rowMeans(xsw_bar[,-1]))/2
  xsw$hour=xsw_bar$hour
  
  xsw=merge(xsw,x_data[,c("time","hour")],by="hour")
  
  # sort by time
  xsw=xsw[order(xsw$time),]
  
  imput.SDEM=overall_mean+select(xsw,-c(time,hour))
  imput.SDEM=data.frame(time=xsw$time,imput.SDEM)
  
  x_data=subset(x_data,select=-hour)
  x_data.SDEM=x_data
  x_data.naive=x_data
  for(i in 2:ncol(x_data)){
    indx=which(is.na(x_data[,i]))
    x_data.SDEM[indx,i]=imput.SDEM[indx,i]
    x_data.naive[indx,i]=imput.naive[indx,i]
  }
  
  return(list(SDEM=x_data.SDEM,naive=x_data.naive))
  
}

rmse=function(x,y){
  # x is the true data, y is the imputed data
  # For both, the first column is time
  
  D=dim(x)[2]-1
  RMSEs=data.frame(matrix(0,ncol=D,nrow=1))
  names(RMSEs)=colnames(x)[-1]
  
  for(i in 1:D){
    RMSEs[i]=sqrt(mean((x[,(i+1)]-y[,(i+1)])^2))
  }
  
  return(RMSEs)
}


# Detrend-deseas ----------------------------------------------------------

LOESS.Decomp=function(tsx,sw=24,tw=6){
  # tsx is vector of observations
  
  # Transform tsx in a ts object, first turing it into a matrix
  tsx=ts(tsx,frequency = sw)
  
  ts.stl <- stl(tsx, 
                s.window=sw,
                t.window=tw,
                na.action = na.approx,
                robust=T)
  res=ts.stl$time.series[,"remainder"]
  trend=ts.stl$time.series[,"trend"]
  seasonal=ts.stl$time.series[,"seasonal"]
  return(list(ts.stl=ts.stl,res=res,trend=trend,seasonal=seasonal))
}

LOESS.df=function(data,sw=24,tw=6){
  
  # This function takes a data frame "data" with time as first column and returns a list with three data frames:
  # trend, level, and seasonal components
  # sw is the window for seasonal decomposition (default 24 hours)
  # tw is the window for trend decomposition (default 6 hours)
  
  colnames(data)[1]="time"
  trend=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  #level=data.frame()
  season=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  residuals=matrix(0,ncol=ncol(data)-1,nrow=nrow(data))
  for(i in 2:ncol(data)){
    loess=LOESS.Decomp(data[,i],sw,tw)
    trend[,(i-1)]<-loess$trend
    #level[,(i-1)]<-loess$seasonal
    season[,(i-1)]<-loess$seasonal
    residuals[,(i-1)]<-loess$res
  }
  trend=data.frame(time=data$time,trend)
  #level=data.frame(time=data$time,level)
  season=data.frame(time=data$time,season)
  residuals=data.frame(time=data$time,residuals)
  colnames(trend)=colnames(data)
  #colnames(level)=colnames(data)
  colnames(season)=colnames(data)
  colnames(residuals)=colnames(data)
  
  return(list(trend=trend,season=season,residuals=residuals))
}

# Holt-Winters (better)
HoltWintersDecomposition <- function(x
                                     ,period=24
) {
  x=ts(x,frequency = period)
  xhw=HoltWinters(x)
  res=x-xhw$fitted[,"xhat"]
  return(list(HW=xhw,
              level=xhw$fitted[,"level"],
              trend=xhw$fitted[,"trend"],
              season=xhw$fitted[,"season"],
              residuals=res))
}


HoltWint.df=function(data,period=24){
  
  # This function takes a data frame "data" with time as first column and returns a list with four data frames:
  # trend, level, seasonal, and residuals components
  # period is the frequency of the time series
  
  # Remember that using Holt-Winter method we loose the first "period" observations
  
  colnames(data)[1]="time"
  time=data$time[-(1:period)]
  trend=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  level=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  season=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  residuals=matrix(0,ncol=ncol(data)-1,nrow=nrow(data)-period)
  for(i in 2:ncol(data)){
    hw=HoltWintersDecomposition(data[,i],period)
    trend[,(i-1)]<-hw$trend
    level[,(i-1)]<-hw$level
    season[,(i-1)]<-hw$season
    residuals[,(i-1)]<-hw$residuals
  }
  trend=data.frame(time=time,trend)
  level=data.frame(time=time,level)
  season=data.frame(time=time,season)
  residuals=data.frame(time=time,residuals)
  colnames(trend)=colnames(data)
  colnames(level)=colnames(data)
  colnames(season)=colnames(data)
  colnames(residuals)=colnames(data)
  
  return(list(trend=trend,level=level,season=season,residuals=residuals))
}

# Kriging -----------------------------------------------------------------

cvobj_STFDF=function(dat,locations,stat_col){
  
  # This function creates a STFDF object from a data frame "dat" and excludes the station with index "stat_col"
  
  locations<- locations[,c("id","longitude","latitude")]
  loc_to_pred=locations[locations$id %in% colnames(dat)[stat_col],]
  stat_id=as.character(loc_to_pred$id)
  loc_to_pred=loc_to_pred[,-1]
  dat_orig=dat
  dat=dat[,-stat_col]
  
  stats<- locations[locations$id %in% colnames(dat)[-1],]
  rownames(stats)=stats[,1]
  
  # dat format long using dplyr
  dat_long<- dat %>%  gather(key = "station", value = "air_temperature", -time)
  
  # merge with stats
  dat_long<-merge(dat_long,stats,by.x="station",by.y="id")
  dat_long=dat_long[,-1]
  
  # Spatial coordinates
  stats<- stats[,-1]
  stats=SpatialPoints(stats)
  
  #tdat=t(dat[,-1])
  #dat_stfdf<- STFDF(stats, dat$time, data.frame(dat = as.vector(tdat)))
  dat_stfdf<- STFDF(stats, dat$time, dat_long[,-1])
  
  return(list(loc_to_pred=loc_to_pred,
              stat_id=stat_id,
              dat_stfdf=dat_stfdf,
              dat_orig=dat_orig))
  
}

# step1=cvobj_STFDF(dat_temp,locations,6)
# dat=step1$dat_stfdf
# loc_to_pred=step1$loc_to_pred

# Wrap up in a function
STkriging<-function(dat,
                    #vgm.mod,
                    loc_to_pred,ordinary=T){
  
  # This function performs spatio-temporal kriging
  # dat is a STFDF object
  # loc_to_pred is a data frame with columns "longitude" and "latitude", representing the location to predict
  # ordinary is a boolean indicating whether to use ordinary kriging (default is TRUE)
  
  names(dat@data)=c("z","x","y")
  
  if(ordinary){
    vsta <- variogramST(z ~ 1, dat,
                        #cutoff = Mdstats,
                        #width=.01,
                        #cores=3,
                        tlags = 0:11,
                        na.omit = T,
                        progress = F)
  }
  else{
    vsta <- variogramST(z ~ x+y, dat,
                        #cutoff = Mdstats,
                        #width=.01,
                        #cores=3,
                        tlags = 0:11,
                        na.omit = T,
                        progress = F)
  }
  
  
  v.sp <- vsta[vsta$timelag==0,c("spacelag","gamma")]
  v.t <- vsta[vsta$spacelag==0,c("timelag","gamma")]
  s.sill=.9*max(v.sp$gamma,na.rm = T)
  s.range=max(v.sp$spacelag)
  t.sill=.9*max(v.t$gamma,na.rm = T)
  t.range=
    as.numeric(
      v.t$timelag[which(v.t$gamma>=.9*max(v.t$gamma,na.rm = T))[1]]
    )/3
  st.an=estiStAni(vsta,c(0,100))
  
  vgm.mod <- vgmST(stModel="sumMetric",
                          space=vgm(s.sill,
                                    "Exp", s.range,0),
                          time=vgm(t.sill,
                                   "Exp", t.range, 0),
                          joint=vgm(s.sill,
                                    "Exp",s.range, 0),
                          stAni=st.an+0.01)
  
  sp.sill.lb <- 0.7 * max(v.sp$gamma, na.rm=TRUE)
  sp.range.lb=min(v.sp$spacelag)
  t.sill.lb <- 0.7 * max(v.t$gamma, na.rm=TRUE)
  t.range.lb=
    as.numeric(
      v.t$timelag[which(v.t$gamma>=.7*max(v.t$gamma,na.rm = T))[1]]
    )/3
  vgm.mod.est <-
    fit.StVariogram(vsta, vgm.mod,
                    method="L-BFGS-B",
                    control=list(maxit=500),
                    lower=c(sp.sill.lb, sp.range.lb,0,
                            t.sill.lb, t.range.lb,0,
                            0,1,0,
                            1))
  
  x_gridded <- SpatialPoints(loc_to_pred)
  
  dat_rep=c(loc_to_pred$longitude,loc_to_pred$latitude)
  dat_rep=rep(dat_rep,length(dat@time))
  dat_rep=matrix(dat_rep,ncol=2,byrow=T)
  dat_rep=data.frame(dat_rep)
  names(dat_rep)=c("x","y")
  
  grid=STFDF(sp=x_gridded, 
           time=dat@time, 
           data=dat_rep
           )
  
  st=Sys.time()
  if(ordinary){
    stkgr <- krigeST(z~1, data=dat, newdata=grid,
                     modelList=vgm.mod.est,
                     computeVar = T)
  }
  else{
    stkgr <- krigeST(z~x+y, data=dat, newdata=grid,
                     modelList=vgm.mod.est,
                     computeVar = T)
  }
  
  en=Sys.time()
  elapsed=en-st
  
  return(list(stkgr=stkgr,
              elapsed=elapsed))
}


# step1=cvobj_STFDF(air5_naive,locations,6)
# step2=STkriging(step1$dat_stfdf,step1$loc_to_pred,ordinary = F)
# 
# plot(air_short[,6],type='l',col='red',xlab='Time',ylab='Temperature')
# lines(step2$stkgr@data$var1.pred,col='blue')

# Function computing RMSE and MAE for predicted station
compute_errors=function(stkgr,cvobj){
  
  pred=stkgr$stkgr@data$var1.pred
  obs=cvobj$dat_orig[,cvobj$stat_id]
  
  rmse=sqrt(mean((pred-obs)^2))
  mae=mean(abs(pred-obs))
  return(list(rmse=rmse,mae=mae))
}

# CV (to be performed in parallel)
CV_STkr=function(stat_ind,dat,locations,ordinary=T){
  step1=cvobj_STFDF(dat,locations,stat_ind) # Exclude stat_ind
  step2=STkriging(step1$dat_stfdf,
                  #vgm.model,
                 step1$loc_to_pred,ordinary = ordinary)
  step3=compute_errors(step2,step1)
  return(list(
    step1=step1,
    step2=step2,
    RMSE=step3$rmse,
    MAE=step3$mae))
}

# # a parita di metodo di imputazione, valutare krigin con e senza detrend-deseas
# sh.wi=1:200
# dat_temp=air5_naive[sh.wi,]
# res_temp=air5_loess_naive$residuals[sh.wi,]
# air5_naive_full=CV_STkr(5,dat_temp,locations)
# air5_naive_res=CV_STkr(5,res_temp,locations)
# kgr.ST.res=air5_naive_res$step2$stkgr@data$var1.pred
# #back to original series
# 
# x=air5_loess_naive
# locations2<- locations[,c("id","longitude","latitude")]
# locations2=locations2[locations2$id %in% colnames(x$trend),]
# target="S115"
# 
# library(geosphere)
# dstats=distm(x=locations2[,2:3], fun = distHaversine)
# colnames(dstats)=locations2$id
# rownames(dstats)=locations2$id
# 

# TBF
get_orig_series=function(x,kgrST.res,locations2,target,loess=T){
  # x is an object obtained with function LOESS.df or HoltWint.df
  # kgrST.res is the result of the spatio-temporal kriging, using function CV_STkr above

  # Compute weights
  dstats=distm(x=locations2[which(locations2$id==target),2:3],
               y=locations2[,2:3], fun = distHaversine)/1000
  colnames(dstats)=locations2$id
  rownames(dstats)=target
  dstats[which(dstats==0)]=1
  wstats=1/(dstats)
  wstats[which(wstats==1)]=0
  wstats=wstats/sum(wstats)
  if(loess){
    trend.w=apply(x$trend[,-1],1,function(y) sum(y*wstats))
    seas.w=apply(x$season[,-1],1,function(y) sum(y*wstats))
   # result=trend.w+seas.w+kgr.ST.res
    result=trend.w[1:200]+seas.w[1:200]+kgr.ST.res

    sqrt(mean((result-air_short[1:200,target])^2))

  }
}



