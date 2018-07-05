setwd ("C:/Users/lma243.NAU/OneDrive/PhD_Documents")
data= read.csv ("Bandon_stats.csv")
data2 = read.csv ("Non_native_brandon.csv")
head(data)
head (data2)

ANOVA = aov (Abundance ~ Type *LifeZone* Group, data=data)
summary (ANOVA)
TukeyHSD(ANOVA)

ANOVA <- aov(Abundance~LifeZone*Habitat, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = Abundance, 
     factors = c(Habitat,LifeZone), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,100), col=c('blue','red'), density=c(1000,1000,15,15), main='Fly Abundance', ylab="insects per site", xlab="Life Zone")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))

bar(dv = Richness, 
    factors = c(Cover,Elevation), 
    dataframe = data3, 
    errbar = TRUE, 
    ylim=c(0,100), col=c('red','darkblue'), main='Bee Richness', ylab="Bee Species per Site", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(15,14,12,21,6,7),labels=c("a","a","a","b","a","a"))
text(2.5,c(385,370,355),labels=c("p<0.0001 (lifezone)","p<0.0001 (Habitat)","p<0.0001 (Interaction)"))



data4= read.csv ("Anova_percent_fly.csv")
bar(dv = Abundance, 
    factors = c(Cover,Elevation), 
    dataframe = data4, 
    errbar = TRUE, 
    ylim=c(0,50), col=c('red','darkblue'), main='Fly Abundance', ylab="Flies per Site", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(14,13,31,41,20,18),labels=c("a","a","b","b","c","c"))

bar(dv = Richness, 
    factors = c(Cover,Elevation), 
    dataframe = data2, 
    errbar = TRUE, 
    ylim=c(0,50), col=c('red','darkblue'), main='Fly Richness', ylab="Fly Species per Site", xlab="Life Zone")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(9,10,25,21,10,10),labels=c("a","a","b","b","a","a"))









ANOVA <- aov(Richness~Elevation*Cover*Group, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

data2= read.csv ("Anova_richness.csv")

ANOVA <- aov(Richness~Elevation*Group, data=data2)
summary (ANOVA)

data= read.csv ("Anova_abundance_relative.csv")

ANOVA <- aov(Abundance~Elevation*Group, data=data2)
summary (ANOVA)

data2= read.csv ("Anova_richness_relative.csv")

ANOVA <- aov(Richness~Elevation*Cover, data=data2)
summary (ANOVA)



setwd ("C:/Users/Lindsie/Documents/Master's work")
data= read.csv ("richness_RPA.csv")
glm = glm(Richness~Elevation*Cover,data=data,family=poisson)
anova(glm, test="Chisq")
summary (glm)
TukeyHSD(glm)

chisq.post.hoc(tbl, test = c("fisher.test")

data= read.csv ("ANOVA_percent.csv")
glm = glm(X.Richness~Elevation*Cover,data=data,family=poisson)
anova(glm, test="Chisq")
TukeyHSD(ANOVA)




setwd ("C:/Users/Lindsie/Documents/Master's work")
DV= read.csv ("anova_DV.csv")
ANOVA <- aov (Richness ~ Elevation*Cover, data=data)
summary (ANOVA)
TukeyHSD(ANOVA)

ANOVA <- aov (Volume ~ Elevation, data=DV)
summary (ANOVA)
TukeyHSD(ANOVA)