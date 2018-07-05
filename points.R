setwd('C:/Users/Lindsie/Dropbox/SFP Pollinators/Modeling')
respXY <- read.csv("spider_points.csv")
respXY <- respXY[,1:3]
head(respXY)
pres=respXY[respXY$IC==1,]
head(pres)
#this can be read as 'just rows of respXY where PresAbs is 1'. Not entering any numbers after the comma indicates that all columns are to be retained. If instead we put in e.g. ',1:2]', only columns 1 and 2 of respXY would be included in the new object 'pres'
abse=respXY[respXY$IC==0,]#only selecting absences
head(abse)

library (rgeos)
library (rgdal)
library (maptools)
library (raster)
usa=readOGR('U:/2015_Fall/Closed/Week3_Sep15/usa','US_STATES_wgs84')

globe=readOGR('U:/2015_Fall/Closed/Week3_Sep15/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
plot(NAm)

NAm=crop(NAm,extent(-130,-60,25,49))
plot(NAm)

usaWGS=spTransform(usa,CRS(proj4string(NAm)))
plot(NAm)
plot(usaWGS,add=T)
points(abse[,2:1],col='red', pch=20, cex=.75)
#creating a new plot, here where absences are red
points(pres[,2:1],col='blue', pch=20, cex=0.75)
#adding blue presence points to the plot