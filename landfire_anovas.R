####landfire ANOVAs

setwd("C:/Users/lma243.NAU/Dropbox/SDModeling")
data = read.csv("megdata_cleaned.csv")
head(data)

cc = aov (Richness ~ as.factor(cc), data = data)
summary(cc)
TukeyHSD(cc)

bar2(dv = Richness, 
     factors = as.factor(cc), 
     dataframe = data, 
     errbar = TRUE, 
     ylim=c(0,5), ylab="Meg Richness", xlab="Canopy Cover")

evc = aov (Richness ~ as.factor(evc), data = data)
summary(evc)

bar2(dv = Richness, 
     factors = as.factor(evc), 
     dataframe = data, 
     errbar = TRUE, 
     ylim=c(0,5), ylab="Meg Richness", xlab="EVC")



hist (data$Richness)
df10 = data[which(data$Richness >= 10),]
hist(df10$Richness, breaks=80)
