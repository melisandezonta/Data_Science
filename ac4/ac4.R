#username : mzr3
# code --------------------------------------------------------------------
library(ggmap)
library(Rcpp)
library(raster)
library(ggplot2)
library(sp)
data(crime)

# Mapping of the type of crimes-------------------------------

houston <- get_map(location = "houston", zoom = 13) ##Get the houston map
houstonMap <-ggmap(houston, extent = "device")
houstonMap+geom_point(aes(x = lon, y = lat, colour = offense),
                      data = crime) + ggtitle("Map of Crime in Houston")



# heatwaves ---------------------------------------------------------------

houston <- get_map(location = "houston", zoom = 13) ##Get the houston map
houstonMap<-ggmap(houston, extent = "device")       ##Prepare Map

houstonMap +
  stat_density2d(aes(x = lon, y = lat, fill = ..level..,alpha=..level..), bins = 10, geom = "polygon", data = crime) +
  scale_fill_gradient(low = "black", high = "red")+
  ggtitle("Map of Crime Density in Houston")




