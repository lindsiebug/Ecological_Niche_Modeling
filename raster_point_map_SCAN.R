library(ggplot2)
library(ggthemes)
library(scales)
library(ggmap)
library (rgeos)
library (rgdal)
library (maptools)
library (raster)
library(MASS)
library(sp)
library(viridis)


setwd("C:/Users/lma24/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Acrididae_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=750,nrow=375)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 1.5e3] <- 1.5e3
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c("lightBlue","Orange", "Red"))
plot(pr2,col=colfunc(5))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)

pop <- read.table("allGeo.tsv", header=TRUE, stringsAsFactors=FALSE)
head(pop)
r2 = raster (ncol=1500,nrow=3000)
pr<-raster::rasterize(pop, r2, fun='count', cex=0.6) 
cropbox1 <-c(-125,-60,25, 49)
#crop the raster
pr2 <- crop(pr, cropbox1)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 1e3] <- 1e3
pr2[pr2 <= 10]<- 0
pr2
plot(pr2)
plot(usaWGS, bg="transparent", add=TRUE)




setwd("C:/Users/lma24/Downloads/f244c94d-c902-41f3-b91e-004dd69d23a3")
small <- read.csv("occurrence_raw.csv", header=TRUE)
head(small)
Bee<-small[,c("dwc.decimalLongitude","dwc.decimalLatitude")]
r = raster (ncol=1500,nrow=3000)
sr<-raster::rasterize(Bee, r, fun='count', cex=0.6)
cropbox <-c(-178,-49,18, 71)
sr2 <- crop(sr, cropbox2)
sr2[sr2 >= 50] <- 50
sr2
plot(sr2)
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)


Bee<-small[,c("dwc.decimalLongitude","dwc.decimalLatitude")]
r = raster (ncol=750,nrow=1500)
sr<-raster::rasterize(Bee, r, fun='count', cex=0.6)
cropbox <-c(-178,-49,18, 71)
sr2 <- crop(sr, cropbox2)
sr2[sr2 >= 1000] <- 1000
sr2
plot(sr2)
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)



usa=readOGR('S:/projects/ecogis/SDM_Class/All_SDM_Courses/2015_Fall/Closed/Week3_Sep15/usa','US_STATES_wgs84')
#dugway=readOGR('U:/All_SDM_Courses/2015_Fall/Closed/Week3_Sep15/usa','US_STATES_wgs84')

globe=readOGR('S:/projects/ecogis/SDM_Class/All_SDM_Courses/2015_Fall/Closed/Week3_Sep15/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
plot(NAm, bg="transparent", add=TRUE)

NAm=crop(NAm,extent(-165,-60,8,85))
plot(NAm)

usaWGS=spTransform(usa,CRS(proj4string(NAm)))
plot(NAm)
gg<-plot(usaWGS,add=T)

tiff('SCAN_map.tiff')
plot(pr2)
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
NAm=crop(NAm,extent(-165,-60,5,70))
dev.off()

jpeg('map_points.jpg')
plot(NAm)
plot(usaWGS, bg="transparent", add=TRUE)
points(pop$decimalLatitude, pop$decimalLongitude, col = "red", cex = .01)
dev.off()


t.test()
setwd("C:/Users/lma24/OneDrive/Documents/Shape_files")
library(rgdal)
# Read SHAPEFILE.shp from the current working directory (".")
shape <- readOGR('C:/Users/lma24/OneDrive/Documents/Shape_files','Dugway')
