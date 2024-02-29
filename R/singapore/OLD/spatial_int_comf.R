constr_obspoints=function(locations,time=1,
                          air_temp,RH,wind_dir,wind_speed,rainfall){
  
  tmp=locations[c("id","latitude","longitude")]
  tmp$air_t=NA
  tmp$rh=NA
  tmp$wdir=NA
  tmp$wspeed=NA
  tmp$rainf=NA
  #time=1
  for(i in 1:nrow(locations)){
    #DA RIVEDERE 
    if(sum(tmp$id[i]==colnames(air_temp))!=0){
      tmp$air_t[i]=air_temp[time,colnames(air_temp)[which(tmp$id[i]==colnames(air_temp))]]
    }
    if(sum(tmp$id[i]==colnames(RH))!=0){
      tmp$rh[i]=air_temp[time,colnames(RH)[which(tmp$id[i]==colnames(RH))]]
    }
    if(sum(tmp$id[i]==colnames(wind_dir))!=0){
      tmp$wdir[i]=wind_dir[time,colnames(wind_dir)[which(tmp$id[i]==colnames(wind_dir))]]
    }
    if(sum(tmp$id[i]==colnames(wind_speed))!=0){
      tmp$wspeed[i]=wind_speed[time,colnames(wind_speed)[which(tmp$id[i]==colnames(wind_speed))]]
    }
    if(sum(tmp$id[i]==colnames(rainfall))!=0){
      tmp$rainf[i]=rainfall[time,colnames(rainfall)[which(tmp$id[i]==colnames(rainfall))]]
    }
  }
  
}



coordinates(meuse) <- ~ x + y
class(meuse)
str(meuse)

# 1. Calculate the sample variogram. This is done with the variogram function
lzn.vgm <- variogram(log(zinc)~1, meuse)  

# 2. Fit a model to the sample variogram
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1))

# For the fit.variogram function, a sample variogram is the first argument. 
# The second is the model, with parameters, to be fit to the sample variogram. 
# For a list of all possible variograms that can be used, call vgm, and to see graphical properties/characteristics of these models, call show.vgms.
plot(lzn.vgm, lzn.fit)

coordinates(meuse.grid) <- ~ x + y # step 3 above
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)


lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()