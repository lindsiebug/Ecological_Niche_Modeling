setwd('F:/Final Data sets')
FlyVol<-read.csv ("Volume_all_flies1.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=FlyVol)
summary (ANOVA)
TukeyHSD(ANOVA)
png(file="CWM_Fly_Volume.png",width=400, height=350)
bar(dv = SES, 
    factors = c(LifeZone), 
    dataframe = FlyVol, 
    errbar = TRUE, 
    ylim=c(0,400), col=c('Blue','darkorange'), main='Fly Volume', ylab="Volume(mm3)")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(110,105,200,200,290,265),labels=c("a","a","b","b","c","d"))
text(2.5,c(385,370,355),labels=c("p<0.0001 (lifezone)","p<0.0001 (Habitat)","p<0.0001 (Interaction)"))
dev.off()




BeeVol<-read.csv ("Volume_all_bees1.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=BeeVol)
summary (ANOVA)
TukeyHSD(ANOVA)
png(file="CWM_Bee_Volume.png",width=400, height=350)
bar(dv = SES, 
      factors = c(Habitat, LifeZone), 
      dataframe = BeeVol, 
      errbar = TRUE, 
      ylim=c(0, 1100), col=c('Blue','darkorange'), main='Bee Volume', ylab="Volume (mm3)")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(400,400,400,425,850,810),labels=c("a","a","b","c","d","e"))
text(2.5,c(1000,950,900),labels=c("p<0.0001 (lifezone)","p=<0.0001 (Habitat)","p=0.122 (Interaction)"))
dev.off()







BeeDark<-read.csv ("Darkness_all_bees1.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=BeeDark)
summary (ANOVA)
TukeyHSD(ANOVA)
png(file="F:/CWM images/CWM_Bee_Darkness.png",width=966, height=664)
bar(dv = SES, 
      factors = c(Habitat, LifeZone), 
      dataframe = BeeDark, 
      errbar = TRUE, 
      ylim=c(0,120), col=c('Blue','darkorange'), main='Bee Darkness', ylab="Darkness")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(79,80,75,78,56,40),labels=c("a","a","b","a","c","d"))
text(2.5,c(110,105,100),labels=c("p<0.0001 (lifezone)","p=0.0068 (Habitat)","p<0.0001 (Interaction)"))
dev.off()



FlyDark<-read.csv ("Darkness_all_flies1.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=FlyDark)
summary (ANOVA)
TukeyHSD(ANOVA)
png(file="CWM_Fly_Darkness.png",width=400, height=350)
bar(dv = SES, 
      factors = c(Habitat, LifeZone), 
      dataframe = FlyDark, 
      errbar = TRUE, 
      ylim=c(0,125), col=c('Blue','darkorange'), main='Fly Darkness', ylab="Darkness")
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(97,103,79,85,76,76),labels=c("a","b","d","c","d","d"))
text(2.5,c(120,115,110),labels=c("p<0.0001 (lifezone)","p<0.0001 (Habitat)","p=0.112 (Interaction)"))
dev.off()