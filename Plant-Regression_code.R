setwd("C:/Users/Lindsie/OneDrive/Documents/Manuscripts/Kendrick_Paper_data/raw data")
CC<-read.csv("plant_regression_data.csv")
head(CC)

mod1 <- aov(Elevation~PC*Mountian, data=CC)
summary(mod1)

mod2 <- aov(Elevation~PC+Mountian, data=CC)
summary(mod2)

anova(mod1,mod2)

Kendrick <- subset(CC, Mountian=="Kendrick")
SFPeaks <- CC[CC$Mountian=='SFPeaks',]
Ponderosa <- subset(SFPeaks, Section=="Low")
MixCon <- subset(SFPeaks, Section=="Middle")
SF <- subset(SFPeaks, Section=="High")


reg1 <- lm(PC~Elevation, data=Kendrick); summary(reg1)
reg2 <- lm(PC~Elevation, data=SFPeaks); summary(reg2)
reg3 <- lm (PC ~ CC, data=CC); summary (reg3)
plot(PC~CC, data=CC, type='n', xlab="Elevation (meters)", ylab="Plant Cover (%)")
points(Kendrick$Elevation,Kendrick$PC, pch=20)
points(SFPeaks$Elevation,SFPeaks$PC, pch=1)
abline(reg1, lty=1)
abline(reg2, lty=2)
legend("topright", c("Kendrick","SFPeaks"), lty=c(1,2), pch=c(20,1) )



setwd("C:/Users/Lindsie/OneDrive/Documents")
CC<-read.csv("plant_regression_data.csv")
head(CC)

mod1 <- aov(CC~PC*Mountian, data=CC)
summary(mod1)

mod2 <- aov(CC~PC+Mountian, data=CC)
summary(mod2)

anova(mod1,mod2)

Kendrick <- subset(CC, Mountian=="Kendrick")
SFPeaks <- CC[CC$Mountian=='SFPeaks',]


reg1 <- lm(CC~PC, data=Kendrick); summary(reg1)
reg2 <- lm(CC~PC, data=SFPeaks); summary(reg2)
reg3 <- lm (PC ~ CC, data=CC); summary (reg3)
plot(CC~PC, data=SFPeaks, type='n', ylab="Canopy Cover (%)", xlab="Plant Cover (%)")
points(Ponderosa$PC, Ponderosa$CC, pch=1)
points(SF$PC, SF$CC, pch=5)
points(MixCon$PC, MixCon$CC, pch=20)

points(Kendrick$CC,Kendrick$PC, pch=1)
points(SFPeaks$PC,SFPeaks$CC, pch=20)
abline(reg2, lty=1)
#abline(reg1, lty=1)
#abline(reg2, lty=2)
legend("bottomright", c("Ponderosa","Mixed Conifer", "Spruce-fir"), pch=c(1,20.5) )


#####bee-fly manuscript figure 4 2013-2014 data####
par(mfrow=c(1,2))

#reg2 <- lm(PC~CC, data=SFPeaks); summary(reg2)
##points(Ponderosa$CC, Ponderosa$PC, pch=1)
#points(SF$CC, SF$PC, pch=5)
##points(MixCon$CC, MixCon$PC, pch=20)
#abline(reg2, lty=1)
#legend("bottomleft", c("Ponderosa","Mixed Conifer", "Spruce-fir"), pch=c(1,20,5) )

setwd("C:/Users/Lindsie/OneDrive/Documents/Manuscripts/Kendrick_Paper_data/raw data")
CCC<-read.csv("plant_regression_data.csv")
head(CCC)
SFPeaks <- CCC[CCC$Mountian=='SFPeaks',]
Ponderosa <- subset(SFPeaks, Section=="Low")
MixCon <- subset(SFPeaks, Section=="Middle")
SF <- subset(SFPeaks, Section=="High")

reg <- lm(bee13~ CC, data=SFPeaks); summary(reg)
plot(bee13~CC, data=SFPeaks, type='n', xlab="Canopy Cover (%)", ylab="Bee Abundance (%)", ylim = c(0,100))
points(Ponderosa$CC, Ponderosa$bee13, pch=15)
points(MixCon$CC, MixCon$bee13, pch=16)
points(SF$CC, SF$bee13, pch=17)
abline(reg, lty=1)
legend("bottomleft", c("Ponderosa","Mixed Conifer", "Spruce-fir"), pch=c(15,16,17) )

reg2 <- lm(fly13~ CC, data=SFPeaks); summary(reg)
plot(fly13~CC, data=SFPeaks, type='n', xlab="Canopy Cover (%)", ylab="Fly Abundance (%)", ylim = c(0,100))
points(Ponderosa$CC, Ponderosa$fly13, pch=15)
points(MixCon$CC, MixCon$fly13, pch=16)
points(SF$CC, SF$fly13, pch=17)
abline(reg2, lty=1)
legend("bottomright", c("Ponderosa","Mixed Conifer", "Spruce-fir"), pch=c(15,16,17) )

#####Figure###########

setwd("C:/Users/Lindsie/OneDrive/PhD_Documents")
CC<-read.csv("meadow_forest_plant_cover.csv")
head(CC)

PPM <- subset(CC, Section=="PPM")
MCM <- subset(CC, Section=="MCM")
SFM <- subset(CC, Section=="SFM")
PPF <- subset(CC, Section=="PPF")
MCF <- subset(CC, Section=="MCF")
SFF <- subset(CC, Section=="SFF")

par(mfrow=c(1,1))
CC_PC <- lm(PC~CC, data=CC); summary(CC_PC)
plot(PC~CC, data=CC, type='n', xlab="Tree Canopy Cover (%)", ylab="Flowering Herbaceous Plant Cover (%)")
points(PPM$CC, PPM$PC, pch=21, col= "red")
points(MCM$CC, MCM$PC, pch=22, col="darkblue")
points(SFM$CC, SFM$PC, pch=24, col="green")
points(PPF$CC, PPF$PC, pch=16, col="red")
points(MCF$CC, MCF$PC, pch=15, col="darkblue")
points(SFF$CC, SFF$PC, pch=17, col="green")
abline(CC_PC, lty=1)
text (70, 35, expression(paste("R"^"2"*"=0.894"))) 
legend("bottomleft", c("Ponderosa Meadow","Ponderosa Forest","Mixed Conifer Meadow","Mixed Conifer Forest" ,"Spruce-fir Meadow", "Spruce-fir Forest"), pch=c(21,16,22,15,24,17), col= c("red","red","darkblue","darkblue","green","green") )


######
HC_FC <- lm(FC~HC, data=CC); summary(HC_FC)
plot(FC~HC, data=CC, type='n', xlab="Tree Canopy Cover (%)", ylab="Flowering Herbaceous Plant Cover (%)")
points(PPM$HC, PPM$FC, pch=15)
points(MCM$HC, MCM$FC, pch=16)
points(SFM$HC, SFM$FC, pch=17)
points(PPF$HC, PPF$FC, pch=22)
points(MCF$HC, MCF$FC, pch=21)
points(SFF$HC, SFF$FC, pch=24)
abline(HC_FC, lty=1)
text (70, 35, expression(paste("R"^"2"*"=0.894"))) 
legend("bottomleft", c("Ponderosa Meadow","Ponderosa Forest","Mixed Conifer Meadow","Mixed Conifer Forest" ,"Spruce-fir Meadow", "Spruce-fir Forest"), pch=c(21,16,22,15,24,17), col= c("red","red","blue","blue","green","green") )







