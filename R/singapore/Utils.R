
load("locations.Rdata")
load("air_short.Rdata")

dat_temp=air_short[1:200,]

# Function creating STFDF object and excluding station with index "stat_colt"
cvobj_STFDF=function(dat,locations,stat_col){
  
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

step1=cvobj_STFDF(dat_temp,locations,6)
dat=step1$dat_stfdf
loc_to_pred=step1$loc_to_pred

# Wrap up in a function
STkriging<-function(dat,
                    #vgm.mod,
                    loc_to_pred,ordinary=T){
  
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

step2=STkriging(step1$dat_stfdf,step1$loc_to_pred)

# Function computing RMSE and MAE for predicted station
compute_errors=function(stkgr,cvobj){
  
  pred=stkgr$stkgr@data$var1.pred
  obs=cvobj$dat_orig[,cvobj$stat_id]
  
  rmse=sqrt(mean((pred-obs)^2))
  mae=mean(abs(pred-obs))
  return(list(rmse=rmse,mae=mae))
}

# CV (to be performed in parallel)
CV_STkr=function(stat_ind,dat,locations){
  step1=cvobj_STFDF(dat,locations,stat_ind) # Exclude station 2
  step2=STkriging(step1$dat_stfdf,
                  #vgm.model,
                 step1$loc_to_pred)
  step3=compute_errors(step2,step1)
  return(list(
    RMSE=step3$rmse,
    MAE=step3$mae))
}

prv=CV_STkr(5,dat_temp,locations)



