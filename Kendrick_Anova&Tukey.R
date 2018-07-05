setwd("C:/Users/Lindsie/Dropbox/SFP Pollinators/Pollinator Study_2015/Kendrick_Study")
data=read.csv("All_data.csv",na.strings=c("","NA"))
data2=data[,-c(14:109)]
head (data2)


ANOVA <- aov(Bee.total~Mountian+low.high, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = Bee.total, 
     factors = c(Mountian,low.high), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,100), col=c('gray','gray5'), main='Bee Abundance', ylab="insects per site", xlab="Life Zone")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))


bar2(dv = Fly.Total, 
     factors = c(Mountian,low.high), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,100), col=c('gray','gray5'), main='Fly Abundance', ylab="insects per site", xlab="Life Zone")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))


bar2(dv = Fly.SR, 
     factors = c(Mountian,low.high), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,25), col=c('gray','gray5'), main='Fly Richness', ylab="insects per site", xlab="Life Zone")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))

bar2(dv = Bee.SR, 
     factors = c(Mountian,low.high), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,25), col=c('gray','gray5'), main='Bee Richness', ylab="insects per site", xlab="Life Zone")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))
