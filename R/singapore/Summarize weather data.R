# Load packages -----------------------------------------------------------
# Load libraries
library(scales)
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(lubridate)
library(sp)
library(spacetime)
# library(ozmaps)
library(sf)
#library(ggmap)
library(gstat)
library(automap)
library(geoR)


# Summarize data ----------------------------------------------------------

wdn="15 mins"

air_temp2=air_temp %>% 
  group_by(time=floor_date(time,wdn))%>%
  summarise_if(is.numeric, mean, na.rm = TRUE) 
# here na.rm=TRUE is used to ignore NA values. 
# With this value, when we compute mean for all NA values, it returns NaN.

# Inpute NaN values with NA

air_temp2[,-1]=air_temp2[,-1]%>% mutate_all(~ifelse(is.nan(.), NA, .))

# Equally distanced times -------------------------------------------------

eqdist_times=seq(from=air_temp2$time[1],
                 to=tail(air_temp2$time,1),
                 by=wdn)

air_temp3=air_temp2%>% 
  right_join(data.frame(time=eqdist_times),by="time")%>%
  arrange(time)

# Plot  -------------------------------------------------------------------

# Long format for easier plotting
air_temp_long=air_temp3 %>%
  pivot_longer(!time, names_to = "station", values_to = "air_temperature")


par(mfrow=c(4,5),mar=c(2,2,2,2))
for(i in 2:ncol(air_temp3)){
  plot(x=air_temp3$time,y=as.vector(unlist(air_temp3[,i])),type="l",col="black",
       xlab=" ",ylab=" ",
       main=colnames(air_temp3[,i]))
  title(main=colnames(air_temp3)[i])
}
