library(rgdal)
library(foreign)
library(RgoogleMaps)
library(ggmap)

setwd("C:/Users/lma24/Dropbox/Biodiversity/Arthropod Projects/Dugway/Maps")



CenterOfMap <- geocode("40.1997,-112.8605")
Dugway <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 12, maptype = "terrain", source = "google")
Dugwaymap <- ggmap(Dugway)
Dugwaymap



#dugway1=readOGR('C:/Users/lma24/Dropbox/Biodiversity/Arthropod Projects/Dugway/Shape_file','Airspace')
#plot(dugway1)


Dugway2=readOGR('C:/Users/lma24/Dropbox/Biodiversity/Arthropod Projects/Dugway/Shape_file','MOA')
plot(Dugway2)

Dugway2 <- spTransform(Dugway2, CRS("+proj=longlat +datum=WGS84"))
Dugway2 <- fortify(Dugway2)

Dugwaymap <- Dugwaymap + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='green', data=Dugway2, alpha=0)
Dugwaymap




