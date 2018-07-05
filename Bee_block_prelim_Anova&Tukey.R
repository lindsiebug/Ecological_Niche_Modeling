setwd ("C:/Users/lma243.NAU/OneDrive/PhD_Documents")
data2= read.csv ("bee_block_meta_info2.csv")
head (data2)


ANOVA <- aov(Colinization~Life_zone, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = Colinization, 
     factors = c(Life_zone), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,65), col=c('blue','red','green'), main='Bee Block Colonization', ylab="# of Holes Colonized", xlab="Life Zone")
text(c(.7,1.9,3.1),c(27,44,60),labels=c("a","a","b"))

ANOVA <- aov(Colinization~Life_zone*Hole_Size, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)


bar2(dv = Colinization, 
     factors = c(Hole_Size,Life_zone), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,1.8), col=c('blue','blue','blue','red','red','red','green','green','green'),density=c(1000,75,15), main='Bee Block Colonization by Hole Size', ylab="Colonization per block", xlab="Life Zone")
text(c(1.5,2.5,3.5,5.5,6.5,7.5,9.5,10.5,11.5),c(.4,.7,.35,.4,1.1,.35,.45,.95,1.65),labels=c("a","a","a","a","b","a","a","b","c"))


bar2(dv = Colinization, 
     factors = c(Life_zone,Hole_Size), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,1.8), col=c('blue','red','green'), density=c(1000,1000,1000,75,75,75,15,15,15), main='Bee Block Colonization by Hole Size', ylab="Colonization per block", xlab="Life Zone")
text(c(1.5,2.5,3.5,5.5,6.5,7.5,9.5,10.5,11.5),c(.4,.4,.45,.7,1.1,.95,.35,.35,1.65),labels=c("a","a","a","a","b","a","a","b","c"))

ANOVA <- aov(Colinization~Hole_Size, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)


bar2(dv = Colinization, 
     factors = c(Hole_Size), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,.8), col=c('blue'), density=c(1000,75,15), main='Hole Size Colonization', ylab="Colonization per block", xlab="Life Zone")
text(c(.7,1.9,3.1),c(0.3,0.72,0.7),labels=c("a","b","b"))


ANOVA <- aov(Colinization~as.factor(Distance)*Life_zone, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = Colinization, 
     factors = c(Life_zone, Distance), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,2.3),col=c('blue','red','green'), main='Colonization Distance from Forest', ylab="Colonization per block", xlab="Distance form Forest (Meters)")
text(c(.7,1.9,3.1),c(0.3,0.72,0.7),labels=c("a","b","b"))


bar2(dv = Colinization, 
     factors = c(Distance,Life_zone), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,2.3),col=rep(c("blue","red","green"),each=10), main='Colonization Distance from Forest', ylab="Colonization per block", xlab="Life Zone")



Reg = lm(Colinization~Distance, data=data2)
summary(Reg)



setwd("C:/Users/lma243.NAU/Dropbox/SFP Pollinators/Undergraduates/Undergraduate_Research_Posters/Jeremy Arcelia UGRAD 2017 Poster")
data=read.csv("final_data_set.csv")
head(data)

reg1 <- lm(Abundance~MT, data=data); summary(reg1)
reg2 <- lm(Abundance ~ MT, data=data) ;summary(reg2)


test=data(iris)
View(iris)

library(e1071); library(ggplot2)

###Abundance Temp by year and lifezone
qplot(MAT, Abundance, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Temperature")+ ylab("Abudnace per Trap")+
theme_bw() + 
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

###Richness Temp by year and lifezone
qplot(MAT, Richness, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Temperature")+ ylab("Abudnace per Trap")+
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )


###Abundance Precip by year and lifezone
qplot(MAP, Abundance, colour = Year, shape = Life.Zone, 
      data = data)+  xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

###Richness Precip by year and lifezone
qplot(MAP, Richness, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )









qplot(MAT, Abundance, colour = Year, data = data) +
theme_bw() +
theme(plot.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank() )


reg1 <- lm(MAP~Year, data=data); summary(reg1)

reg2 <- lm(PC~Elevation, data=SFPeaks); summary(reg2)
plot(Abundance~MAT, data=data, type='n', xlab="MAT", ylab="Abundace")
points(data$MAT,data2$Abudance, pch=20)
points(SFPeaks$Elevation,SFPeaks$PC, pch=1)
abline(reg1, lty=1)
abline(reg2, lty=2)
legend("topright", c("Kendrick","SFPeaks"), lty=c(1,2), pch=c(20,1) )



#text(2.5,c(385,370,355),labels=c("p<0.0001 (lifezone)","p<0.0001 (Habitat)","p<0.0001 (Interaction)"))

