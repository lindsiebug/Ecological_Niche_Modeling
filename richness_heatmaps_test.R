require(raster)

#n <- c(1:2000); for (i in 1:2000) {n[i] <- sample(as.factor(1:100),1)} #create a random set of 100 'species' (numnbers 1 to 100 as factor levels, each representing a species), i.e. pick one species 2000 times

#spp_records <- data.frame(latitude=runif(2000, min=-33, max=-30), longitude=runif(2000, min=135, max=140), value=n) #create a data frame simulating 2000 spatial records of the 100 simulated species within a predetermined spatial area - numbers under 'value' are in this case factors representing unique species from the above pool of 100 

setwd("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees")
meg_rich <- read.csv("Megachilidae_rich.csv", header=TRUE)
spp_records= meg_rich[,c("decimalLongitude","decimalLatitude","genus","specificEpithet")]
spp_records= within(spp_records, values <- paste(genus, specificEpithet, sep='_'))
spp_records = na.omit(spp_records)
spp_records = spp_records[,c("decimalLongitude","decimalLatitude","values")]
head(spp_records)

coordinates(spp_records) <- c(1,2) #set to spatial points data frame object by highlighting the latitude and longitude columns

plot(spp_records, pch=20, cex=0.5) # plot as points to visualise where the records were made


spp_records <- data.frame(latitude=round(spp_records$decimalLatitude, digits=1), longitude=round(spp_records$decimalLongitude, digits=1), value=spp_records) #now recreate, but truncate the coordinates to a regular grid at 0.1 degrees resolution
coordinates(spp_records) <- c(1,2)

rich_count <- function(x) length(unique(x)) # simple function to count the number of unique 'species' (factor levels)


richness <- aggregate(spp_records$value.values, list(spp_records$value.decimalLatitude, spp_records$value.decimalLongitude), rich_count) #count the number of unique species that have the same truncated coordinates

coordinates(richness) <- c(2,1) #set coordinates

##line 31 is not working correctly##
#richness2 <- rasterFromXYZ(richness) #create a gridded map (at 0.1 resolution) with values as the number of unique species recorded in that cell

projection(richness) <- "+proj=longlat +ellps=WGS84" #set the projection for plotting

plot(richness, pch= 15, cex=.3,  col=rev(heat.colors(max(richness$x))), add=TRUE) # cell-based 'species richness' map


usa=readOGR('C:/Users/lma243.NAU/Dropbox/SDModeling/Environmental_Shape_files/usa','US_STATES_wgs84')
globe=readOGR('C:/Users/lma243.NAU/Dropbox/SDModeling/Environmental_Shape_files/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
NAm=crop(NAm,extent(-165,-60,8,85))
usaWGS=spTransform(usa,CRS(proj4string(NAm)))

plot(richness, legend  = FALSE, axes=FALSE, frame.plot=FALSE)
plot(NAm, bg="white", col = "floralwhite" , main = "Megachilidae Richness")
plot(richness, pch= 15, cex=.3,  col=rev(topo.colors(max(richness$x))), add=TRUE,interpolate= TRUE) # cell-based 'species richness' map
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)


head(richenss@data)
df <- data.frame(x=coordinates(richness)[,1], y=coordinates(richness)[,2], richness@data)
write.csv(df, "C:/Users/lma243.NAU/Dropbox/SDModeling/megdata.csv", row.names=FALSE)


library(raster)
library(sp)

r <- getData("worldclim",var="bio",res=10)
r <- r[[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]]
names(r) <- c("AMT","MDR","ISO","TSea","MTWM","MTCM","TAR","MTWetQ","MTDQ","MTWarmQ",
              "MTColdQ","AMP","PWM","PDM","PSean","PWetQ","PDryQ","PWarmQ","PColdQ")
values <- extract(r,richness)
df2 <- cbind.data.frame(coordinates(richness),values)
head(df2)
write.csv(df2, "C:/Users/lma243.NAU/Dropbox/SDModeling/meg_clim_data.csv", row.names=FALSE)


###richness and climate regressions####
RvC= read.csv ("C:/Users/lma243.NAU/Dropbox/SDModeling/megdata_adj.csv")
RvC=na.omit(RvC)
head(RvC)

rall <- lm(Richness ~ AMT + MDR + ISO + TSea + MTWM + MTCM + TAR + MTWetQ + MTDQ + 
           MTWarmQ + MTColdQ + AMP + PWM + PDM + PSean + PWetQ + PDryQ + PWarmQ +
           PColdQ, data=RvC)
summary(rall)

rall2 <- lm(Richness ~  MDR + ISO + MTWetQ + MTDQ + AMP + PWM + PDM + PWetQ 
            + PDryQ + PWarmQ, data=RvC)
summary(rall2)


r1 <- lm(Richness ~ AMT , data=RvC)
summary(r1)

r2 <- lm(Richness ~ MDR , data=RvC)
summary(r2)

r3 <- lm(Richness ~ ISO , data=RvC)
summary(r3)

r4 <- lm(Richness ~  TSea , data=RvC)
summary(r4)

r5 <- lm(Richness ~  MTWM , data=RvC)
summary(r5)

r6 <- lm(Richness ~  MTCM , data=RvC)
summary(r6)

r7 <- lm(Richness ~  TAR , data=RvC)
summary(r7)

r8 <- lm(Richness ~  MTWetQ , data=RvC)
summary(r8)

r9 <- lm(Richness ~  MTDQ , data=RvC)
summary(r9)

r10 <- lm(Richness ~ MTWarmQ , data=RvC)
summary(r10)

r11 <- lm(Richness ~  MTColdQ , data=RvC)
summary(r11)

r12 <- lm(Richness ~  AMP , data=RvC)
summary(r12)

r13 <- lm(Richness ~  PWM , data=RvC)
summary(r13)

r14 <- lm(Richness ~  PDM , data=RvC)
summary(r14)

r15 <- lm(Richness ~ PSean , data=RvC)
summary(r15)

r16 <- lm(Richness ~  PWetQ , data=RvC)
summary(r16)

r17 <- lm(Richness ~ PDryQ , data=RvC)
summary(r17)

r18 <- lm(Richness ~ PWarmQ, data=RvC)
summary(r18)

r19 <- lm(Richness ~ PColdQ, data=RvC)
summary(r19)

r20 = lm (Richness ~ elevation, data =df_elev3 )
summary(r20)

par(mfrow = c(3,4))

plot(y = RvC$Richness, x = RvC$MDR, main= "MDR")
abline(r2, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$ISO, main= "ISO")
abline(r3, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$TSea, main= "TSea")
abline(r4, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$MTWetQ, main= "MTWetQ")
abline(r8, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$MTDQ, main= "MTDQ")
abline(r9, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$AMP, main= "AMP")
abline(r12, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$PWM, main= "PWM")
abline(r13, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$PDM, main= "PDM")
abline(r14, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$PWetQ, main= "PWetQ")
abline(r16, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$PDryQ, main= "PDryQ")
abline(r17, lty=1, col = "red")
plot(y = RvC$Richness, x = RvC$PWarmQ, main= "PWarmQ")
abline(r18, lty=1, col = "red")
plot(y=df_elev3$Richness, x= df_elev3$elevation, main= "Elevation")
abline(r20, lty=1, col = "red")


par(mfrow = c(1,1))

###extract elevation points###
library(elevatr)
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
df_elev2 <- get_elev_point(RvC, prj = prj_dd, src = "mapzen")

write.csv(df_elev, "C:/Users/lma243.NAU/Dropbox/SDModeling/meg_elevation_data.csv", row.names=FALSE)

df_elev3 = read.csv("C:/Users/lma243.NAU/Dropbox/SDModeling/meg_elevation_data2.csv")
head(df_elev3)
par(mfrow = c(1,1))
plot(y=df_elev3$Richness, x= df_elev3$elevation, main= "Elevation")
abline(r20, lty=1, col = "red")


r21 <- lm(Richness ~ x, data=RvC)
summary(r21)

par(mfrow = c(1,2))
plot(y=df_elev3$Richness, x= df_elev3$elevation, main= "Elevation")
abline(r20, lty=1, col = "red")
plot(y=RvC$Richness, x= RvC$y, main= "Latitude")
abline(r21, lty=1, col = "red")



###transforming varriables####
#log transformation
RvC$trans_MDR <- log(RvC$MDR) 
RvC$trans_ISO <- log(RvC$ISO) 



rt2 <- lm(Richness ~ trans_MDR , data=RvC)
summary(rt2)
rt3 <- lm(Richness ~ trans_ISO , data=RvC)
summary(rt3)



#subsetting
subelev = subset(df_elev3, Richness < 40, select = c(Richness, elevation))
head (subelev)


rsubel = lm (Richness ~ elevation, data =subelev )
summary(rsubel)
par(mfrow = c(1,1))
plot(y=subelev$Richness, x= subelev$elevation, main= "Elevation")
abline(rsubel, lty=1, col = "red")



##non-linera regressions###

RvC= read.csv ("C:/Users/lma243.NAU/Dropbox/SDModeling/megdata_adj_cleaned.csv")
RvC=na.omit(RvC)
head(RvC)

##bell-shaped test##
model<-nls(Richness~a-b*exp(-c*AMT),start=list(a=120,b=110,c=0.064), data = RvC)

rAMT <- nls(Richness ~ a * exp(- b* AMT*2),start=list(a=120,b=110,c=0.064), data=RvC)
summary(r1)
