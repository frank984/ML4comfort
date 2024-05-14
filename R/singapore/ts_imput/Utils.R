
#load("locations.Rdata")
#load("air_short.Rdata")
#Desktop
#load("20240430.Rdata")

#dat_temp=air_short[1:200,]
Sys.setenv(LANG = "en")
Sys.setlocale("LC_ALL", "English")
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
#library(geojsonio)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
library(ggmap)
library(gstat)
library(automap)
library(xts)
library(geosphere)


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

# y_data=air_5
# x_data=rh_short

lin_reg_imp=function(y_data,x_data){
  
  # y_data is a data frame with variable to predict with time as first column. Each column is a station
  # x_data is a data frame with the covariates to be used in the linear regression
  
  names(x_data)[1]="time"
  names(y_data)[1]="time"
  y_data_pred=y_data
  x_data_reg=x_data
  for(i in 2:ncol(x_data)){
    indx=which(is.na(y_data[,i]))
    df=data.frame(y=y_data[,i],x=x_data_reg[,i])
    df_pred=df[indx,]
    df_pred=df$x
    df[indx,]=NA
    y_pred=lm(y~x,data=df)
    y_pred=predict(y_pred,newdata=data.frame(x=df_pred[indx]))
    y_data_pred[indx,i]=y_pred
    # plot(y_data_pred[,i],type='l',col='red')
    # lines(y_data[,i],col='blue')
  }
  
  return(y_data_pred)
  
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

weightdist_imp=function(x_data,locations2){
  
  # This function interpolates missing values in x_data using a weighted mean of station records.
  # The weights are inversely proportional to the distance between the target station and the other stations.
  
  # Arguments:
  # x_data: a data frame with time in the first column and station records in the other columns
  # locations2: a data frame with station id, longitude and latitude
  
  # Value:
  # xi_data: a data frame with interpolated values
  
  rel_stat=intersect(locations2$id, colnames(x_data))
  locations2=locations2[which(locations2$id%in%rel_stat),]
  # Sort by locations$id
  locations2=locations2[order(locations2$id),]
  
  # Order columns in x_data
  time=x_data$time
  x_data=x_data[,locations2$id]
  x_data=cbind(time,x_data)
  xi_data=x_data
  
  for(i in 2:ncol(x_data)){
    target=colnames(x_data)[i]
    dstats=distm(x=locations2[which(locations2$id==target),2:3],
                 y=locations2[,2:3], fun = distHaversine)/1000
    colnames(dstats)=locations2$id
    rownames(dstats)=target
    dstats[which(dstats==0)]=NA
    wstats=1/(dstats)
    wstats[which(is.na(wstats))]=0
    wstats=wstats/sum(wstats,na.rm = T)
    
    for(t in 1:dim(x_data)[1]){
      if(is.na(x_data[t,i])){
        xi_data[t,i]=weighted.mean(x_data[t,-1],wstats,na.rm = T)
      }
    }
    
  }
  
  return(xi_data)
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
  # trend, seasonal components and residuals
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
CV_STkr=function(stat_ind,dat,locations,ordinary=T,plot=F){
  step1=cvobj_STFDF(dat,locations,stat_ind) # It automatically excludes stat_ind
  stat_id=step1$stat_id
  step2=STkriging(step1$dat_stfdf,
                  #vgm.model,
                 step1$loc_to_pred,ordinary = ordinary)
  step3=compute_errors(step2,step1)
  
  if(plot){
    df=data.frame(time=dat$time,
                  result=step2$stkgr@data$var1.pred,
                  true=step1$dat_orig[,step1$stat_id])
    P=ggplot(df)+
      geom_line(aes(x=time,y=result,col="blue"))+
      geom_line(aes(x=time,y=true,col="red"))+
      theme_bw()+labs(x='Time',y='Temperature')+
      scale_colour_manual(name = ' ', 
                          values =c('blue'='blue','red'='red'), labels = c('Fitted','True'))
    return(list(
      stat_id=stat_id,
      step1=step1,
      step2=step2,
      RMSE=step3$rmse,
      MAE=step3$mae,
      plot=P))
  }
  
  else{
    return(list(
      stat_id=stat_id,
      step1=step1,
      step2=step2,
      RMSE=step3$rmse,
      MAE=step3$mae))
  }

}

# 
get_orig_series=function(x,kgrST.res,locations2,target,loess=T){
  
  # This function recovers the original series from the trend, seasonal, and residuals components
  
  # Arguments:
  # x is an object obtained with function LOESS.df or HoltWint.df
  # kgrST.res is the result of the spatio-temporal kriging, using function CV_STkr above
  # locations2 is a data frame with columns "id", "longitude", and "latitude"
  # target is a character specifying the station to predict
  # loess is a boolean indicating whether to use LOESS decomposition (default is TRUE)
  
  # Value:
  # A vector with the original series (trens, seasonal, level components plus residuals from kriging)
  
  # Compute weights
  dstats=distm(x=locations2[which(locations2$id==target),2:3],
               y=locations2[,2:3], fun = distHaversine)/1000
  colnames(dstats)=locations2$id
  rownames(dstats)=target
  dstats[which(dstats==0)]=NA
  wstats=1/(dstats)
  wstats[which(is.na(wstats))]=0
  wstats=wstats/sum(wstats,na.rm = T)
  
  trend.w=apply(x$trend[,-1],1,function(y) sum(y*wstats))
  seas.w=apply(x$season[,-1],1,function(y) sum(y*wstats))
  
  if(loess){
   # result=trend.w+seas.w+kgr.ST.res
    result=trend.w+seas.w+kgrST.res
  }
  else{
    level.w=apply(x$level[,-1],1,function(y) sum(y*wstats))
    result=level.w+trend.w+seas.w+kgrST.res
  }
  return(list(result=result,
              target=target))
}


df_recover=function(x,x_trend_seas=NULL,loess=T,locations2,time,residuals=T){
  
  df=NULL
  if(residuals){
    if(!is.null(x_trend_seas)){
      for(i in 1:length(x)){
        temp=get_orig_series(
          x_trend_seas,
          x[[i]]$step2$stkgr@data$var1.pred,
          locations2,
          target=x[[i]]$stat_id,
          loess=loess)
        df=cbind(df,temp$result)
        colnames(df)[i]=temp$target
      }
      df=data.frame(time,df)
    }
    else{
      stop("x_trend_seas must be provided")
    }
  }
  else{
    for(i in 1:length(x)){
      temp=x[[i]]$step2$stkgr@data$var1.pred
      df=cbind(df,temp)
      colnames(df)[i]=x[[i]]$stat_id
    }
    df=data.frame(time,df)
  }
  return(df)
}


rmse_detrdeseas=function(reconstr_series,true_series,time,plot=T,type="SARIMA - HW",legend=T,miss,z=60){
  
  # This function computes the RMSE between the reconstructed series and the true series
  
  # Arguments:
  # reconstr_series is a vector with the reconstructed series, output of the function get_orig_series
  # true_series is a vector with the true series
  # miss is a vector specifying first and last indexes of missing data
  # time is a vector with the time
  # plot is a boolean indicating whether to plot the series
  # type is a character specifying the type of reconstruction
  # legend is a boolean indicating whether to include a legend in the plot
  # z is a numeric value specifying the zoom in the plot
  
  # Value:
  # A numeric value with the RMSE
  # An optional plot with the fitted and true series
  
  RMSE=sqrt(mean((reconstr_series[miss]-true_series[miss])^2))
  zoom=miss
  zoom[1]=zoom[1]-z
  zoom[2]=zoom[2]+z
  if(plot){
    df=data.frame(time=time[zoom[1]:zoom[2]],
                  result=reconstr_series[zoom[1]:zoom[2]],
                  true=true_series[zoom[1]:zoom[2]])
    P=ggplot(df)+
      geom_rect(aes(
        #xmin = time[miss[1]], xmax = time[miss[2]], 
        xmin = time[z], xmax = time[z+miss[2]-miss[1]], 
                    ymin = min(c(min(result),min(true))), 
                    ymax = max(c(max(result),max(true)))), alpha = 0.1,fill="grey")+
      geom_line(aes(x=time,y=true,col="blue"),size=1)+
      geom_line(aes(x=time,y=result,col="red"),size=.8)+
      theme_bw()+labs(x=' ',y=type)+
      theme(text = element_text(size = 12),
            legend.text=element_text(size=12),
            axis.text = element_text(size=12))
    
      if(legend){
        P=P+scale_colour_manual(name = ' ', 
                                values =c('blue'='blue','red'='red'), 
                                labels = c('True','Fitted'))
      }
      
    return(list(RMSE=RMSE,
                plot=P))
  }
  else{
    return(RMSE) 
  }
  
}

rmse_st=function(reconstr,true,miss){
  
  # This function computes the RMSE between the reconstructed series and the true series
  
  # Arguments:
  # reconstr is a data frame with the reconstructed series. First column is time, the others are stations
  # true is a data frame with the true series. First column is time, the others are stations
  # miss is a vector specifying first and last indexes of missing data
  
  # Value:
  # A numeric vector with the RMSE for each station
  
  reconstr=reconstr[miss[1]:miss[2],]
  true=true[miss[1]:miss[2],]
  RMSE=rep(0,ncol(true)-1)
  for (i in 2:ncol(true)){
    RMSE[i-1]=sqrt(mean((reconstr[,i]-true[,i])^2,na.rm=T))
  }
  return(RMSE)
}