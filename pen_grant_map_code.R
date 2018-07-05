####PEN Grant Graphs####

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


usa=readOGR('C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/usa','US_STATES_wgs84')

globe=readOGR('C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
plot(NAm, bg="transparent", add=TRUE)

NAm=crop(NAm,extent(-165,-60,8,85))
plot(NAm)

usaWGS=spTransform(usa,CRS(proj4string(NAm)))
plot(NAm, col= 'gray')
points(respXY, pch = 15, col= 'green', cex=3.2)
points(respXY, pch = 16, col = "black", cex=.5)
plot(usaWGS,add=T)



###NUMBER OF COL or ROWS === KM RESOLUTION####
#1000x500 = 36x36 KM
#750X375 = 50X50 KM
#468.75x234.5= 75x75KM
#375x187.5 = 100x100 KM



###ACRIDIDAE####

setwd("C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Acrididae_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=468.75,nrow=234.5)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 200] <- 200
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c( "lightgoldenrod1","goldenrod1" ,"Orange", "Red"))
#save as png##
png(filename = "C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Acrididae_50x50km.png", height = 823, width = 1046)
plot(pr2,col=colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
dev.off()


###BLATTODEA####

setwd("C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Blattodea_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=468.75,nrow=234.5)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 50] <- 50
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c( "lightgoldenrod1","goldenrod1" ,"Orange", "Red"))
#save as png##
png(filename = "C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Blattodea_50x50km.png", height = 823, width = 1046)
plot(pr2,col=colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
dev.off()


###DERMAPTERA####

setwd("C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Dermaptera_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=468.75,nrow=234.5)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 50] <- 50
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c( "lightgoldenrod1","goldenrod1" ,"Orange", "Red"))
#save as png##
png(filename = "C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Dermaptera_50x50km.png", height = 823, width = 1046)
plot(pr2,col=colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
dev.off()


###GRYLLIDAE####

setwd("C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Gryllidae_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=468.75,nrow=234.5)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 50] <- 50
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c( "lightgoldenrod1","goldenrod1" ,"Orange", "Red"))
#save as png##
png(filename = "C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Gryllidae_50x50km.png", height = 823, width = 1046)
plot(pr2,col=colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
dev.off()


###MANTIDS####

setwd("C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd("C:/Users/lma24/Downloads")
popB <- read.csv("Mantids_occurrences.csv", header=TRUE)
head(popB)
popB= popB[,c("decimalLongitude","decimalLatitude")]
head(popB)
r2 = raster (ncol=468.75,nrow=234.5)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
#pr2[pr2 >= 3e4] <- 3e4
pr2[pr2 >= 50] <- 50
#pr2[pr2 <= 10]<- 0
pr2
colfunc <- colorRampPalette(c( "lightgoldenrod1","goldenrod1" ,"Orange", "Red","Purple"))
#save as png##
png(filename = "C:/Users/lma243.NAU/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Mantids_50x50km.png", height = 823, width = 1046)
plot(pr2,col=colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="transparent",  add=TRUE)
dev.off()
