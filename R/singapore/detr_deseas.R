# creazione intervalli di delta=15 minuti sui quali fare la media delle temperature
# il primo e l'ultimo tempo devono essere le 00:00:00 perché l'intervallo 
# sia un multiplo esatto di delta scelto tra 5, 10, 15, 20 minuti
# infatti air_temp$time[1] è "2022-10-10 00:01:00 +08" e
# tail(air_temp$time,1) è "2023-07-03 23:59:00 +08", non suddivisibile esattamente

delta=15 # intervallo di tempo in minuti
# con i tempi in questo formato la funzione seq interpreta l'argomento by in secondi
tgrid = seq(air_temp$time[1]-60,tail(air_temp$time,1)+60, by = delta*60)

# assegno una etichetta per indicizzare i diversi intervalli su cui fare la media
# calcolo lungo 
idtime=numeric(nrow(air_temp))
for(i in 1:(length(tgrid)-1)){
  ind = air_temp$time >= tgrid[i] & air_temp$time < tgrid[i+1]
  idtime[ind] = i
}
# medie di temperature su intervalli consecutivi disgiunti
air_temp = data.frame(air_temp, idtime=idtime)
air_temp.m=aggregate(air_temp[,2],by=list(idtime=idtime),FUN="mean",na.rm=T)
for(j in 3:18){
  tmp = aggregate(air_temp[,j],by=list(idtime=idtime),FUN="mean",na.rm=T)
  air_temp.m = data.frame(air_temp.m,tmp[,-1])
}
names(air_temp.m)[2:18] = names(air_temp)[2:18] # nuovo data frame con medie
# conteggio numero di casi completi in ciascun intervallo
for(j in 2:18){
  tmp = aggregate(complete.cases(air_temp[,j]),by=list(idtime=idtime),FUN="sum")
  air_temp.m = data.frame(air_temp.m,tmp[,-1])
}
names(air_temp.m)[20:36] = paste("nc.",names(air_temp)[2:18],sep="")

# assegno i valori centrali degli intervalli di tempo e li attacco al dataframe
midt = tgrid[air_temp.m$idtime]+delta*30
air_temp.m = data.frame(midt,air_temp.m) 
