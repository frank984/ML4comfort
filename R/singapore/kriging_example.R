# Source: https://rpubs.com/nabilabd/118172

library(sp)
library(gstat)

suppressPackageStartupMessages({
  library(dplyr) # for "glimpse"
  library(ggplot2)
  library(scales) # for "comma"
  library(magrittr)
})

data(meuse)
glimpse(meuse)

meuse %>% as.data.frame %>% 
  ggplot(aes(x, y)) + geom_point(aes(size=zinc), color="blue", alpha=3/4) + 
  ggtitle("Zinc Concentration (ppm)") + coord_equal() + theme_bw()


# Convert the dataframe to a spatial points dataframe (SPDF)

class(meuse)

# To convert it to a spatial dataframe, we must first specify which of the columns contain the coordinates of the data. 
# This is done by using R’s formula notation as follows:

coordinates(meuse) <- ~ x + y
class(meuse)
str(meuse)

coordinates(meuse) %>% glimpse


# Fitting a variogram -----------------------------------------------------

# 1. Calculate the sample variogram. This is done with the variogram function
lzn.vgm <- variogram(log(zinc)~1, meuse)  

# 2. Fit a model to the sample variogram
lzn.fit <- fit.variogram(lzn.vgm, model=vgm(1, "Sph", 900, 1))

# For the fit.variogram function, a sample variogram is the first argument. 
# The second is the model, with parameters, to be fit to the sample variogram. 
# For a list of all possible variograms that can be used, call vgm, and to see graphical properties/characteristics of these models, call show.vgms.

plot(lzn.vgm, lzn.fit)


# Performing Kriging ------------------------------------------------------

# we need two spatial domains: one having values associated with the points, and one for which we want estimates.

# load spatial domain to interpolate over
data("meuse.grid")

str(meuse.grid)

# to compare, recall the bubble plot above; those points were what there were values for. this is much more sparse
plot1 <- meuse %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points with measurements")

# this is clearly gridded over the region of interest
plot2 <- meuse.grid %>% as.data.frame %>%
  ggplot(aes(x, y)) + geom_point(size=1) + coord_equal() + 
  ggtitle("Points at which to estimate")

library(gridExtra)
grid.arrange(plot1, plot2, ncol = 2)

# Computation -------------------------------------------------------------
# We are now ready to krige.
# Note that the second and third arguments have to be SPDF’s and cannot just be dataframes.

coordinates(meuse.grid) <- ~ x + y # step 3 above
lzn.kriged <- krige(log(zinc) ~ 1, meuse, meuse.grid, model=lzn.fit)

str(lzn.kriged)

# Results
lzn.kriged %>% as.data.frame %>%
  ggplot(aes(x=x, y=y)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()


df=lzn.kriged %>% as.data.frame
df$var1.pred=scale(df$var1.pred)*3
df$time=1
df2=df[,-5]
df2$time=2
df2$var1.pred=-df$var1.pred+.5
df3=df[,-5]
df3$time=3
df3$var1.pred=df$var1.pred+2
df=rbind(df,df2,df3)

#save(df,file="example_spatiotemp.Rdata")


# Dynamic heatmap ---------------------------------------------------------
library(ggplot2)
library(shiny)
ui <- fluidPage(
  
  # App title ----
  titlePanel("Dynamic Heatmap"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "time",
                  label = "Time",
                  min = 1,
                  max = 3,
                  step=1,
                  value = 1)
      
    ),
    
    mainPanel(
      
      plotOutput(outputId = "HeatMap")
      
    )
  )
)

server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$HeatMap <- renderPlot({
    
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$time)
    # 
    # hist(x, breaks = bins, col = "#007bc2", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    load("example_spatiotemp.Rdata")
    time=seq(min(input$time), max(input$time), length.out = length(input$time))
    #dftemp=df[df$time==time,]
    ggplot(data=df[df$time==time,],aes(x=x, y=y)) + 
      geom_tile(aes(fill=var1.pred)) + coord_equal() +
      scale_fill_gradient(low = "red", high="green") +
      #scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
      theme_bw()
  })
  
}

shinyApp(ui = ui, server = server)


# Spatio-temporal kriging with NAs ----------------------------------------

# Suggerimento di Antonio
#1) Fai kriging univariato per le stazioni con dati mancanti
#2) Imputa NA con dati da kriging
#3) Fai kriging completo con tutte le stazioni
