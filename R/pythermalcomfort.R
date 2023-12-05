# library(comf)
# ta <- c(20,22,24)
# tr <- ta
# vel <- rep(.15,3)
# rh <- rep(50,3)
# 
# maxLength <- max(sapply(list(ta, tr, vel, rh), length))
# SET <- sapply(seq(maxLength), function(x) { calcSET(ta[x], tr[x], vel[x], rh[x]) } ) 
# SET

library(reticulate)
# py_install("pythermalcomfort") only the first time
import("pythermalcomfort")
source_python('pytherm_fun.py')
source_python('pythermalcomfort.py')

# create a new environment 
virtualenv_create("r-reticulate")
virtualenv_install("r-reticulate", "pythermalcomfort")
pythermalcomfort <- import("pythermalcomfort")


source_python('pythermalcomfort.py')


pet_steady(tdb=20, tr=20, rh=50, v=0.15, met=1.37, clo=0.5)
utci(tdb=25, tr=25, v=1.0, rh=50)

pet_steady_R=function(x){
  return(
    pet_steady(x[1],x[2],x[3],x[4],x[5],x[6])
  )
}

df=data.frame(tdb=c(20,21,22),
              tr=c(20,21,22),
              rh=c(50,50,50),
              v=c(0.15,0.16,0.14),
              met=c(1.37,1.38,1.37),
              clo=c(.5,.6,.4))

apply(df,1,pet_steady_R)
