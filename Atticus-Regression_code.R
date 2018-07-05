setwd("C:/Users/Lindsie/Dropbox/SFP Pollinators/Undergraduate_Research_Posters/Atticus")

CC<-read.csv("Bee Measurements.csv")
head(CC)

mod1 <- lm(Elevation~X100.minus.darkness, data=CC)
summary(mod1)

mod2 <- lm(Elevation~Volume, data=CC)
summary(mod2)

mod2 <- lm(Elevation~Wingspan, data=CC)
summary(mod2)


mod1 <- aov(Elevation~Darkness*Genus, data=CC)
summary(mod1)

mod2 <- aov(Elevation~Volume*Genus, data=CC)
summary(mod2)

mod2 <- aov(Elevation~Wingspan*Genus, data=CC)
summary(mod2)



Kendrick <- subset(CC, Mountian=="Kendrick")
SFPeaks <- CC[CC$Mountian=='SFPeaks',]


reg1 <- lm(PC~Elevation, data=Kendrick); summary(reg1)
reg2 <- lm(PC~Elevation, data=SFPeaks); summary(reg2)
plot(PC~Elevation, data=CC, type='n', xlab="Elevation (meters)", ylab="Plant Cover (%)")
points(Kendrick$Elevation,Kendrick$PC, pch=20)
points(SFPeaks$Elevation,SFPeaks$PC, pch=1)
abline(reg1, lty=1)
abline(reg2, lty=2)
legend("topright", c("Kendrick","SFPeaks"), lty=c(1,2), pch=c(20,1) )





