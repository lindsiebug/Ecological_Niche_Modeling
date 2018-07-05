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



setwd("C:/Users/lma24/Dropbox/SDModeling/Bees")
spNames=read.csv("Megachilidae.csv")


i= 1



setwd("C:/Users/lma24/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps")
#setwd(paste("C:/Users/lma24/Dropbox/SDModeling/Bees/",paste(spNames[i,1],spNames[i,2],sep="_"),sep=""))
#setwd("C:/Users/lma24/Downloads")

### USE IF YOU ALREADY HAVE DATA DOWNLOADED###
#popB <- read.csv("Blattodea_occurrences.csv", header=TRUE)

###UES IF YOU NEED TO DOWNLOAD DATA FROM GBIF & IDIGBIO###
dir.create(paste("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""))
gbifDat=gbif(tolower(spNames[i,1]),paste(spNames[i,2],"*",sep=""))
write.csv(gbifDat,file=paste(paste("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"_OR.csv",sep="_"))
gbifDat= gbifDat[,c("lon","lat")]
gbifDat<- na.omit(gbifDat)

idigbioDat <- idig_search_records(rq=list(genus=paste(spNames[i,1],"",sep=""), specificepithet=paste(spNames[i,2],"",sep="")), fields = "geopoint")
idigbioDat<- na.omit(idigbioDat)
names(idigbioDat) <- c("lon","lat")

occurrences<-rbind(gbifDat)


write.csv(occurrences,file=paste(paste("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"_cleaned.csv",sep="_"))

### RASTERIZING OCCURENCE RECORDS#####
popB = occurrences 
head(popB)
r2 = raster (ncol=180,nrow=90)
pr<-raster::rasterize(popB, r2, fun='count', cex=0.6) 
cropbox2 <-c(-165,-60,8, 72)
#crop the raster
pr2 <- crop(pr, cropbox2)
### LOOK AT RASTER ####
pr2

#SAVE AS PNG##
#png(filename = "C:/Users/lma24/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Species_Maps/Blattodea_50x50km.png", height = 823, width = 1046)
plot(pr2, legend  = FALSE, axes=FALSE, frame.plot=FALSE)
plot(NAm, bg="transparent", col = "burlywood1" , add=TRUE)
plot(pr2,col='deepskyblue1', interpolate= TRUE, add = TRUE, legend  = FALSE)
plot(usaWGS, bg="transparent", add=TRUE)
points (popB$lon ,popB$lat, col = 'Black', cex = .1, pch= 16)
plot(NAm, bg="transparent", add = TRUE)
#dev.off()

