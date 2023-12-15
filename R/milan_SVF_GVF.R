library(geojsonR)
library(geojsonio)
library(leaflet)
library(ggplot2)
# file_js = FROM_GeoJson(url_file_string = 
#                          "Milan_edges_100m.geojson")
# 
# file_js2 = FROM_GeoJson(url_file_string = 
#                           "Milan_nodes_100m.geojson")

milan_nodes=geojson_read("Milan_nodes_100m.geojson",what="sp")
class(milan_nodes)


df=milan_nodes %>% as.data.frame

palsky=colorBin(palette="Blues",domain=range(df$Sky.View.Mean)
                , bins = c(0,.2,.4,.6,.8, 1)
                )

df%>%
  leaflet()%>%
  addTiles()%>%
  addHeatmap(lng=~x,lat=~y,blur = 10,max=1,radius=10,
             intensity = ~Sky.View.Mean, gradient="Blues")%>%
  addLegend(pal = palsky, values = ~Sky.View.Mean
            #, group = "circles"
            , position = "bottomleft")
  
palgreen=colorBin(palette="Greens",domain=range(df$Green.View.Mean)
                  , bins = c(0,.2,.4,.6,.8, 1)
)
df%>%
  leaflet()%>%
  addTiles()%>%
  addHeatmap(lng=~x,lat=~y,blur = 5,max=5,radius=10,
             intensity = ~Green.View.Mean, gradient="Greens")%>%
  addLegend(pal = palgreen, values = ~Green.View.Mean
            #, group = "circles"
            , position = "bottomleft")

library(leaflet.extras)

summary(df[,c("Sky.View.Mean","Green.View.Mean")])

ggplot(df,aes(x, y)) +
  geom_point(aes(color=Sky.View.Mean))+
  scale_colour_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

ggplot(df,aes(x, y)) +
  geom_point(aes(color=Green.View.Mean),size=.5)+
  scale_colour_gradient2(
    low = "red",
    mid = "white",
    high = "green",
    midpoint = 0.5,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  )

FirstScen=df[which(df$Sky.View.Mean>.6),c("x","y","Sky.View.Mean")]
FirstScen$Scen=as.factor(1)

SecondScen=df[which(df$Sky.View.Mean<.4&df$Green.View.Mean<0.2),
              c("x","y","Sky.View.Mean")]
SecondScen$Scen=as.factor(2)

ThirdScen=df[which(df$Sky.View.Mean<.4&df$Green.View.Mean>0.6),
             c("x","y","Sky.View.Mean")]
ThirdScen$Scen=as.factor(3)

df2=full_join(FirstScen,SecondScen)
df2=full_join(df2,ThirdScen)


pal <- colorFactor(c("blue","red", "darkgreen"), domain = c("1", "2","3"))
colnames(df2)[1:2]=c("long","lat")
leaflet(df2) %>% addTiles() %>%
  addCircleMarkers(
    radius = 2.5,
    color = ~pal(Scen),
    stroke = FALSE, fillOpacity = 0.5
  )
