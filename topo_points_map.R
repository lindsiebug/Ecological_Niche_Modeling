library(dismo)
library(raster)

setwd("C:/Users/lma243.NAU/Dropbox/bee_lab")
point = read.csv ("Bee lab data_SFP_March 28_2018.csv")
head(point)
coordinates(point) <- c(6,5) #set coordinates
projection(point) <- "+proj=longlat +ellps=WGS84"

x = extent(-111.9902, -111.2697,35.648, 35.0903)
base.map <- gmap(x, type = "terrain")
point <- spTransform(point, base.map@crs)
plot(base.map)
points(point, col = "darkblue", cex = 1)

plot(point, col = "darkblue", cex = 1, pch = 16, add= T)



