library(comf)

calcUTCI(ta=25, tr=25, vel=1.0, rh=50)
#ta	
#a numeric value presenting air temperature in [degree C]

#tr	
#a numeric value presenting mean radiant temperature in [degree C]

#vel	
#a numeric value presenting air velocity in [m/s]

#rh	
#a numeric value presenting relative humidity [%]

# For vectors:
TA <- c(20,22,28)
TR <- TA
VEL <- c(3,1,0)
RH <- c(1,50,100)

maxLength <- max(sapply(list(TA, TR, VEL, RH), length))
UTCI <- sapply(seq(maxLength), function(x) { calcUTCI(TA[x], TR[x], VEL[x], RH[x]) } )
UTCI

# For data.frames
df <- data.frame(TA, TR, VEL, RH)
UTCI <- apply(df, 1, function(x) { calcUTCI(x[1], x[2], x[3], x[4]) } )
UTCI
