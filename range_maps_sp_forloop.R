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

setwd("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/Megachilidae_diversity_paper_modeling")
spData = read.csv("final_meg_occ.csv")
head(spData)
spNames = read.csv("Meg_sp_list.csv")

i=9
for (i in c(11:949)){
 spname = spData[ which (spData$genus == paste(spNames[i,1])
                         & spData$specificEpithet == paste(spNames[i,2])),]
  pop = spname
  pop= pop[,c("decimalLongitude","decimalLatitude")]
  head(pop)
  r = raster (ncol=180,nrow=90)
  pr<-raster::rasterize(pop, r, fun='count', cex=0.6) 
  cropbox2 <-c(-165,-60,8, 72)
  #crop the raster
  pr <- crop(pr, cropbox2)
  #razteca = mask (pr, NAm)
  pr[pr >= 1] <- 1
  pr[is.na(pr)] <- 0

  writeRaster(pr, file=paste(paste("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/Megachilidae_diversity_paper_modeling/species_raster_layers",paste(spNames[i,1],spNames[i,2],sep="_"),sep="/"),".grd",sep="_"))
}



plot(pr)
