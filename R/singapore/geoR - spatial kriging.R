
# Spatial kriging with geoR -----------------------------------------------

# Load the geoR package
library(geoR)

# source: https://github.com/GiTAP-feagri/Kriging-with-geoR

# Data to be interpolated (should be repeated for each time step)

i=1
data_timefixed=air_res_complete[which(air_res_complete$time_ind==i),]

# Drop NA
data_timefixed = data_timefixed[complete.cases(data_timefixed),]

# GeoR works wih geodata format, and it has a problem with high values of the spatial coordiantes. 
# To solve this, we reduce the values of long and lat by its minimum value

data = data.frame(z = data_timefixed$air_res,
                  x = data_timefixed$longitude #- min(data_timefixed$longitude)
                  ,
                  y = data_timefixed$latitude #- min(data_timefixed$latitude)
)

# Separate attribute variable
solo_atr<- "z"

# Transform data into geodata format
data.geo = as.geodata(data, coords.col = c(2,3),
                      data.col = solo_atr)

# plot data
plot(data.geo, lowess = TRUE)
points(data.geo)


# Variogram analysis ------------------------------------------------------

var_exp = variog(data.geo)
plot(var_exp)

# Eye-fit initial values for the semivariogram parameters # Click "Save" and then click "Quit"
eye_fit_exp = eyefit(var_exp, silent = FALSE)
eye_fit_exp
# Problem: there's need to manually save results

# Experimental semivariogram by MoM and fitting of a parametric model to the semivariogram
fit_exp = geoR::variofit(var_exp, ini.cov.pars = eye_fit_exp, fix.nug = F,
                         cov.model = "exp")

#LOOCV: leave-one-out cross validation to choose the best model
loocv_exp = xvalid(data.geo, model = fit_exp)

# Getting metrics (for model selection, assuming to have tried different specifications for the variagram)
metrics_exp = c(mae = hydroGOF::mae(loocv_exp$predicted, loocv_exp$data),
                me = hydroGOF::me.data.frame(loocv_exp$predicted, loocv_exp$data),
                rmse = hydroGOF::rmse(loocv_exp$predicted, loocv_exp$data),
                r2 = hydroGOF::br2(loocv_exp$predicted, loocv_exp$data),
                ave = hydroGOF::NSE(loocv_exp$predicted, loocv_exp$data),
                willmott = hydroGOF::md(loocv_exp
                                        $predicted, loocv_exp$data))


# Kriging -----------------------------------------------------------------

# Create grid
# See weather_vars.Rmd
load("grid_coords.Rdata")
class(grid_coords)
names(grid_coords)=c("y","x")

# Ordinary kriging
ord.kriging <- krige.conv(geodata = data.geo, locations = grid_coords,
                          krige = krige.control(type.krige = "OK",
                                                obj.model = fit_exp)) 

OK_results=data.frame(pred=ord.kriging$predict, 
                      var=ord.kriging$krige.var,
                      grid_coords)

# Universal kriging
univ.kriging = geodata = krige.conv(data.geo, locations = grid_coords,
                                    krige = krige.control(type.krige = "OK",
                                                          obj.model = fit_exp,
                                                          trend.d = ~ x + y,
                                                          trend.l = ~ x + y)) 

UK_results=data.frame(pred=univ.kriging$predict, 
                      var=univ.kriging$krige.var,
                      grid_coords)


# Recover "original" data: how to add tren and seasonal components? Based on weighted average proximity?

