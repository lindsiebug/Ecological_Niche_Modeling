setwd ("C:/Users/lma243.NAU/Dropbox/SDModeling/Bees/Megachilidae_diversity_paper_modeling/species_raster_layers")
library(raster)
library(RColorBrewer)
Anthidiellum = raster ("Anthidiellum.grd")
Anthidium= raster ("Anthidium.grd")
Anthodioctes= raster ("Anthodioctes.grd")
Ashmeadiella= raster ("Ashmeadiella.grd")
Atoposmia= raster ("Atoposmia.grd")
Chelostoma= raster ("Chelostoma.grd")
Coelioxys= raster ("Coelioxys.grd")
Dianthidium= raster ("Dianthidium.grd")
Dioxys= raster ("Dioxys.grd")
Heriades= raster ("Heriades.grd")
Hoplitis= raster ("Hoplitis.grd")
Hoplostelis= raster ("Hoplostelis.grd")
Lithurgus= raster ("Lithurgus.grd")
Megachile= raster ("Megachile.grd")
Osmia= raster ("Osmia.grd")
Paranthidium= raster ("Paranthidium.grd")
Stelis= raster ("Stelis.grd")
Trachusa= raster ("Trachusa.grd")
Xeroheriades= raster ("Xeroheriades.grd")

usa=readOGR('C:/Users/lma243.NAU/Dropbox/SDModeling/Environmental_Shape_files/usa','US_STATES_wgs84')
globe=readOGR('C:/Users/lma243.NAU/Dropbox/SDModeling/Environmental_Shape_files/continents','continent')
NAm=globe[globe$CONTINENT=='North America',]
NAm=crop(NAm,extent(-165,-60,8,85))
usaWGS=spTransform(usa,CRS(proj4string(NAm)))

colfunc <- colorRampPalette(c("lightskyblue","orange", "red", "darkred"))
colfunc(25)

meg = Anthidiellum +Anthidium+ Anthodioctes+ Ashmeadiella + Atoposmia+ 
  Chelostoma+ Coelioxys+ Dianthidium+ Dioxys+ Heriades+ Hoplitis+ Hoplostelis+ 
  Lithurgus+ Megachile+ Osmia+ Paranthidium+ Stelis+ Trachusa +Xeroheriades
meg[meg == 0] <- NA
par(mfrow=c(1,1))
plot(meg, interpolate= T, main = "Megachilidae Richness", col=colfunc(30))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)



Ashmeadiella[Ashmeadiella == 0] <- NA
Atoposmia[Atoposmia == 0] <- NA
Chelostoma[Chelostoma == 0] <- NA
Coelioxys[Coelioxys == 0] <- NA
Dianthidium[Dianthidium == 0] <- NA
Dioxys[Dioxys == 0] <- NA
Heriades[Heriades == 0] <- NA
Xeroheriades[Xeroheriades == 0] <- NA
Trachusa[Trachusa == 0] <- NA
Stelis[Stelis == 0] <- NA
Paranthidium[Paranthidium == 0] <- NA
Osmia[Osmia == 0] <- NA
Megachile[Megachile == 0] <- NA
Lithurgus[Lithurgus == 0] <- NA
Hoplostelis[Hoplostelis == 0] <- NA
Hoplitis[Hoplitis == 0] <- NA
Anthodioctes[Anthodioctes == 0] <- NA
Anthidium[Anthidium == 0] <- NA
Anthidiellum[Anthidiellum == 0] <- NA



par(mfrow=c(1,1))
plot(Anthidiellum, interpolate= T, main = "Anthidiellum", col="lightblue")
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Anthidium, interpolate= T, main = "Anthidium", col = "green")
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Anthodioctes, interpolate= T, main = "Anthodioctes", col = "lightgreen")
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Ashmeadiella, interpolate= T, main = "Ashmeadiella", col = "lightgreen")
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Atoposmia, interpolate= T, main = "Atoposmia", col = colfunc(17))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Chelostoma, interpolate= T, main = "Chelostoma", col = colfunc(2))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Coelioxys, interpolate= T, main = "Coelioxys", col = colfunc(20))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Dianthidium, interpolate= T, main = "Dianthidium", col = colfunc(10))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Dioxys, interpolate= T, main = "Dioxys", col = colfunc(7))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Heriades, interpolate= T, main = "Heriades", col = colfunc(13))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Hoplitis, interpolate= T, main = "Hoplitis", col = colfunc(30))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Hoplostelis, interpolate= T, main = "Hoplostelis", col = colfunc(3))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Lithurgus, interpolate= T, main = "Lithurgus", col = colfunc(2))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Megachile, interpolate= T, main = "Megachile", col = colfunc(50))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Osmia, interpolate= T, main = "Osmia", col = colfunc(80))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Paranthidium, interpolate= T, main = "Paranthidium", col = colfunc(2))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Stelis, interpolate= T, main = "Stelis", col = colfunc(18))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Trachusa, interpolate= T, main = "Trachusa", col = colfunc(7))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)

plot(Xeroheriades, interpolate= T, main = "Xeroheriades", col = colfunc(2))
plot(usaWGS, bg="transparent", add=TRUE)
plot(NAm, bg="gray", col = "transparent", add = TRUE)


