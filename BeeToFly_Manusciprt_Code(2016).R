setwd("C:/Users/lma24/Dropbox/SFP Pollinators/All_year_data")
all <- read.csv("All_year_peaks_data.csv")
allbee <- all[c(1:54),]
allfly <- all [c(55:108),]
head(all)

head(allbee)
richbee <- aov(Richness ~ Habitat*Site, data = allbee)
richfly <- aov(Richness ~ Habitat*Site, data = allfly)
summary(richbee)
TukeyHSD(richbee)


abaov <- aov(Abundance ~ Group*Habitat*Year, data = all)
richaov <- aov (Richness ~ Group*Habitat*Year, data = all)
pabaov <- aov (P.Abundacne ~ Group*Habitat*Year, data = all)
prichaov <- aov (P.Richness ~ Group*Habitat*Year, data = all)
#summary(abaov)
summary(richaov)
#summary(pabaov)
#summary(prichaov)


allcom <- read.csv("All_year_peaks_data_combind.csv")
head(allcom)

abaov <- aov(Abundance ~ Group*Habitat*Site, data = allcom)
richaov <- aov (Richness ~ Group*Habitat*Site, data = allcom)
pabaov <- aov (P.Abundacne ~ Group*Habitat*Site, data = allcom)
prichaov <- aov (P.Richness ~ Group*Habitat*Site, data = allcom)
summary(abaov)
summary(richaov)
summary(pabaov)
summary(prichaov)

SFbees <- allcom [c(1:18),]
SFflies <- allcom [c(19:36),]
Kenbees <- allcom [c(37:45),]
Kenflies <- allcom[c(46:54),]
ken <- allcom [c(37:54),]

kenab <- aov(ptAbundance ~ Site*Group, data= ken)
summary(kenab)
TukeyHSD(kenab)

kenrich <- aov (ptRichness ~ Site*Group, data=ken)
summary(kenrich)
TukeyHSD(kenrich)

par(mfrow=c(1,2))
bar2(dv = ptAbundance, 
     factors = c(Group,Site), 
     dataframe = ken, 
     errbar = TRUE, 
     ylim=c(0,12), col=gray.colors (2), main='Kendrick Abundance', ylab="insects per trap", xlab="Elevation")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(7,4.3,10,8.6,5.3,4.3),labels=c("a","a","a","a","a","a"))
bar2(dv = ptRichness, 
     factors = c(Group,Site), 
     dataframe = ken, 
     errbar = TRUE, 
     ylim=c(0,6), col=gray.colors (2), main='Kendrick Richness', ylab="insect species per trap", xlab="Elevation")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(2.6,1.5,3.2,5.2,2.1,1.1),labels=c("a","a","a","a","a","a"))




abaov <- aov(Abundance ~ Habitat*Site, data = SFbees)
summary(abaov)
TukeyHSD(abaov)


richaov <- aov (Richness ~ Habitat*Site, data = SFbees)
summary(richaov)
TukeyHSD(richaov)



abaov <- aov(ptAbundance ~ Habitat*Site, data = SFflies)
summary(abaov)
TukeyHSD(abaov)


richaov <- aov (Richness ~ Habitat*Site, data = SFflies)
summary(richaov)
TukeyHSD(richaov)


png("FIG2:SFPeaks", width = 480, height= 480)
par(mfrow=c(2,2))
bar2(dv = ptAbundance, 
     factors = c(Habitat,Life.Zone), 
     dataframe = SFbees, 
     errbar = TRUE, 
     ylim=c(0,15), col=gray.colors (2), main='Bee Abundance', ylab="insects per trap", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(5.5,7.5,4,5.3,4,2.5),labels=c("a","b","a","c","a","a"))
bar2(dv = ptAbundance, 
     factors = c(Habitat,Life.Zone), 
     dataframe = SFflies, 
     errbar = TRUE, 
     ylim=c(0,15), col=gray.colors (2), main='Fly Abundance', ylab="insects per trap", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(3.7,4,4.1,5.9,8.2,14.5),labels=c("a","a","b","c","d","e"))
bar2(dv = ptRichness, 
     factors = c(Habitat,Life.Zone), 
     dataframe = SFbees, 
     errbar = TRUE, 
     ylim=c(0,6), col=gray.colors (2), main='Bee Richness', ylab="insects species per trap", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(5,4.4,2.8,4.2,3.4,2.5),labels=c("a","a","b","a","b","b"))
bar2(dv = ptRichness, 
     factors = c(Habitat,Life.Zone), 
     dataframe = SFflies,  
     errbar = TRUE, 
     ylim=c(0,6), col=gray.colors (2), main='Fly Richness', ylab="insects species per trap", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(4.8,4,3.2,3.4,4.3,5.9),labels=c("a","a","a","a","a","b"))



