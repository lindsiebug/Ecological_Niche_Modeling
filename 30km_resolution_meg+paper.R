library(scales)
library(ggmap)
library (rgeos)
library (rgdal)
library (maptools)
library (raster)
library(ridigbio)
library(dismo)
library(XML)
library(biomod2)
library(fields)



setwd("C:/Users/lma24/Dropbox/SDModeling")
occurrences=read.csv("megdata_adj2.csv")
head(occurrences)
occurrences= occurrences[,c("x","y")]

usa=readOGR('C:/Users/lma24/Dropbox/SDModeling/Environmental_Shape_files/usa','US_STATES_wgs84')
globe=readOGR('C:/Users/lma24/Dropbox/SDModeling/Environmental_Shape_files/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
NAm=crop(NAm,extent(-165,-60,8,85))
usaWGS=spTransform(usa,CRS(proj4string(NAm)))
AZt = spTransform(AZ, CRS(proj4string(NAm)))

AZ = readOGR ('C:/Users/lma24/Dropbox/SDModeling/Environmental_Shape_files/Arizona','tl_2015_04_cousub')
plot (AZt)



### RASTERIZING OCCURENCE RECORDS#####
popA = occurrences
head(popA)
r2 = raster (ncol=200,nrow=100)
pr<-raster::rasterize(popA, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
pr2 = mask (pr, NAm)
### LOOK AT RASTER ####
pr2

df <- data.frame(x=coordinates(pr)[,1], y=coordinates(pr)[,2])
values <- extract(pr,df)
test <- cbind.data.frame(coordinates(df),values)
test_cl=na.omit(test)
writeRaster(pr2, file=paste(paste("C:/Users/lma24/Dropbox/SDModeling/Bees",paste(spNames[i,1],spNames[i,2],sep="_"),sep="/"),"raster.grd",sep="_"))

library(RColorBrewer)
darkcols <- brewer.pal(8, "OrRd")


#### Abudance plots
plot(pr2, legend  = FALSE, axes=FALSE, frame.plot=FALSE, add = FALSE)
plot(NAm, bg="white", col = "floralwhite" )
plot(pr2, col = darkcols, legend  = TRUE, axes=FALSE, frame.plot=FALSE, add = TRUE,interpolate= FALSE)
#plot(pr2,col='deepskyblue1', interpolate= TRUE, add = TRUE, legend  = FALSE)
plot(usaWGS, bg="transparent", add=TRUE)
#points (popA$lon ,popA$lat, col = 'Black', cex = .1, pch= 16)
plot(NAm, bg="gray", col = "transparent", add = TRUE)
#dev.off()
