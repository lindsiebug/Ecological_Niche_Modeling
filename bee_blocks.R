setwd ("C:/Users/lma243.NAU/OneDrive/PhD_Documents")
data= read.csv ("ANOVA_Bee_block.csv")
head (data)

data2= read.csv ("ANOVA_Bee_block_removed02.csv")
head (data2)

MCdat= data2[c(1:9),]
PPdat= data2[c(10:16),]
SFdat= data2[c(17:24),]

ANOVA <- aov(Total.emerged ~ Type*Starting.Life.Zone, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

#mixed conifer
ANOVA <- aov(X..Emerged ~ Type, data=MCdat)
summary (ANOVA)
TukeyHSD(ANOVA)


bar2(dv = X..Emerged, 
     factors = c(Type), 
     dataframe = MCdat, 
     errbar = TRUE, legend = FALSE,
     ylim=c(0,110), col=c('red','purple','blue'),  ylab="% insects emerged per block ", xlab="Block Movement")
text(c(.7,1.91,3.11,.7,3.11),c(3,103,84,105,105),labels=c("a","b","b","+2.1°C","-2.3°C"))

#ponderosa
ANOVA <- aov(X..Emerged ~ Type, data=PPdat)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = X..Emerged, 
     factors = c(Type), 
     dataframe = PPdat, 
     errbar = TRUE, legend = FALSE,
     ylim=c(0,110), col=c('red','purple','blue'),  ylab="% insects emerged per block ", xlab="Block Movement")
text(c(.7,1.91,3.11,.7,3.11),c(45,105,97,105,105),labels=c("a","b","b","+2.1°C","-2.3°C"))


#spruce-fir
ANOVA <- aov(X..Emerged ~ Type, data=SFdat)
summary (ANOVA)
TukeyHSD(ANOVA)

bar2(dv = X..Emerged, 
     factors = c(Type), 
     dataframe = SFdat, legend = FALSE,
     errbar = TRUE, 
     ylim=c(0,110), col=c('red','purple','blue'),  ylab="% insects emerged per block ", xlab="Block Movement")
text(c(.7,1.91,3.11,.7,3.11),c(45,80,98,105,105),labels=c("a","b","b","+2.1°C","-2.3°C"))

#ALL
bar2(dv = X..Emerged, 
     factors = c(Type), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,110), col=c('red','purple','blue'),  ylab="% insects emerged per block ", xlab="Block Movement")
text(c(.7,1.91,3.11,.7,3.11),c(53,83,84.25,65,94),labels=c("a","b","b","+2.1°C","-2.3°C"))

ANOVA <- aov(Total.emerged ~ Type*Starting.Life.Zone, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)


bar2(dv = X..Emerged, 
     factors = c(Type, Starting.Life.Zone), 
     dataframe = data2, 
     errbar = TRUE, 
     ylim=c(0,120), col=c('red','purple','blue'),  ylab="% insects emerged per block ", xlab="Block Movement")
text(c(1.5,2.5,3.5,4.5,6.5,7.5,8.5,9.5,11.5,12.5,13.5,14.5),c(12,13,12,17,42,31,27,43,17,22,85,94),labels=c("a","a","a","a","b","b","b","b","c","c","d","d"))
