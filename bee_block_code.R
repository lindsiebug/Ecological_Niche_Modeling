### logistict regression

install.packages('caTools')
library(caTools)


setwd("C:/Users/lma243.NAU/Dropbox/SFP Pollinators/Data/Bee_Block_data/2016-2017/")
beeblk = read.csv ("Bee_blocks_movement3.csv")
head(beeblk)

##All model####
model <- glm (EON ~  Type, data = beeblk, family = binomial(link='logit'))
summary(model)


#Ponderosa
blkPP = beeblk[c(1:24),]

model <- glm (EON ~  Type, data = blkPP, family = binomial(link='logit'))
summary(model)



#Mixed Conifer
blkMC = beeblk[c(25:53),]

model <- glm (EON ~  Type, data = blkMC, family = binomial(link='logit'))
summary(model)



#Spruce-fir
blkSF = beeblk[c(54:109),]

model <- glm (EON ~  Type, data = blkSF, family = binomial(link='logit'))
summary(model)
