library(ggplot2)
setwd('C:/Users/lma243.NAU/OneDrive/Documents/Manuscripts/Trait_data_Paper_data/Trait Data/Final Data sets')
BeeVolCWM<-read.csv ("CWM_Volume_all_bees.csv")
FlyVolCWM<-read.csv ("CWM_Volume_all_flies.csv")
BeeVolRng<-read.csv ("Volume_all_bees.csv")
FlyVolRng<-read.csv ("Volume_all_flies.csv")
head(BeeVolCWM)
head(FlyVolCWM)
head(BeeVolRng)
head(FlyVolRng)
BeeDarkCWM<-read.csv ("CWM_Darkness_all_bees.csv")
FlyDarkCWM<-read.csv ("CWM_Darkness_all_flies.csv")
BeeDarkRng<-read.csv ("Darkness_all_bees1.csv")
FlyDarkRng<-read.csv ("Darkness_all_flies.csv")
head(BeeDarkCWM)
head(FlyDarkCWM)
head(BeeDarkRng)
head(FlyDarkRng)

####Bee Vol CWM#####

ANOVA <- aov(Volume~LifeZone*Habitat, data=BeeVolCWM)
summary (ANOVA)
TukeyHSD(ANOVA)

BeeVolCWM1 <- summarySE(BeeVolCWM, measurevar="Volume", groupvars=c("Habitat","LifeZone"))
BeeVolCWM1

beevolcwm<-(ggplot(data=BeeVolCWM1, aes(x=LifeZone, y=Volume, group=Habitat, color=Habitat)) + 
                geom_errorbar(aes(ymin=Volume-se, ymax=Volume+se), width=0.25, size = 1) +
                geom_line(size=1.5) +
                geom_point(size=6, shape=16,) + # 21 is filled circle
                ylim(0,700) +  
                xlab("Life Zone") +
                ylab("CWM Volume") +
                scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                                 
                                 l=40)
              +                    # Use darker colors, lightness=40
              theme_bw()+
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                      panel.background = element_blank(), axis.line = element_line(colour = "black")) +
                theme(legend.justification=c(1,0),
                      legend.position=c(1,0),legend.text=element_text(size=11)) +
                theme(axis.text=element_text(size=18, color="black"),
                      plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))


###Bee Vol Range######

ANOVA <- aov(SES~LifeZone*Habitat, data=BeeVolRng)
summary (ANOVA)
TukeyHSD(ANOVA)

BeeVolRng1 <- summarySE(BeeVolRng, measurevar="SES", groupvars=c("Habitat","LifeZone"))
BeeVolRng1

beevolrng<-(ggplot(data=BeeVolRng1, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(-3,3) +  
              xlab("Life Zone") +
              ylab("sesRange Volume") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))

####Fly Vol CWM#####

ANOVA <- aov(Volume~LifeZone*Habitat, data=FlyVolCWM)
summary (ANOVA)
TukeyHSD(ANOVA)

FlyVolCWM1 <- summarySE(FlyVolCWM, measurevar="Volume", groupvars=c("Habitat","LifeZone"))
FlyVolCWM1

flyvolcwm<-(ggplot(data=FlyVolCWM1, aes(x=LifeZone, y=Volume, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=Volume-se, ymax=Volume+se), width=0.25, size = 1) +
              geom_line(size=1.) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(0,700) +  
              xlab("Life Zone") +
              ylab("CWM Volume") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))


####Fly Vol Range######

ANOVA <- aov(SES~LifeZone*Habitat, data=FlyVolRng)
summary (ANOVA)
TukeyHSD(ANOVA)

FlyVolRng1 <- summarySE(FlyVolRng, measurevar="SES", groupvars=c("Habitat","LifeZone"))
FlyVolRng1

flyvolrng<-(ggplot(data=FlyVolRng1, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(-3,3) +  
              xlab("Life Zone") +
              ylab("sesRange Volume") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))


par(mfrow=c(2,2))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
setwd("C:/Users/lma243.NAU/OneDrive/Documents/Manuscripts/Trait_data_Paper_data/final_figures_5.12.17")
png("BeeVolCWM.png")
plot(beevolcwm)
dev.off()

png("FlyVolCWM.png")
plot(flyvolcwm)
dev.off()

png("BeeVolRng.png")
plot(beevolrng)
dev.off()

png("FlyVolRng.png")
plot(flyvolrng)
dev.off()







####Bee Dark CWM#####

ANOVA <- aov(Darkness~LifeZone*Habitat, data=BeeDarkCWM)
summary (ANOVA)
TukeyHSD(ANOVA)

BeeDarkCWM1 <- summarySE(BeeDarkCWM, measurevar="Darkness", groupvars=c("Habitat","LifeZone"))
BeeDarkCWM1
write.csv(BeeDarkCWM1, file = "BeeDarkCWM1.csv")
BeeDarkCWM1 = read.csv("BeeDarkCWM1.csv")


beedarkcwm<-(ggplot(data=BeeDarkCWM1, aes(x=LifeZone, y=Darkness, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=Darkness-se, ymax=Darkness+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(0,75) +  
              xlab("Life Zone") +
              ylab("CWM Darkness") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))


###Bee Dark Range######

ANOVA <- aov(SES~LifeZone*Habitat, data=BeeVolRng)
summary (ANOVA)
TukeyHSD(ANOVA)

BeeDarkRng1 <- summarySE(BeeDarkRng, measurevar="SES", groupvars=c("Habitat","LifeZone"))
BeeDarkRng1

beedarkrng<-(ggplot(data=BeeDarkRng1, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(-3,3) +  
              xlab("Life Zone") +
              ylab("sesRange Darkness") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))

####Fly Dark CWM#####

ANOVA <- aov(Volume~LifeZone*Habitat, data=FlyVolCWM)
summary (ANOVA)
TukeyHSD(ANOVA)

FlyDarkCWM1 <- summarySE(FlyDarkCWM, measurevar="Darkness", groupvars=c("Habitat","LifeZone"))
FlyDarkCWM1

flydarkcwm<-(ggplot(data=FlyDarkCWM1, aes(x=LifeZone, y=Darkness, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=Darkness-se, ymax=Darkness+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(0,75) +  
              xlab("Life Zone") +
              ylab("CWM Darkness") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))


####Fly Dark Range######

ANOVA <- aov(SES~LifeZone*Habitat, data=FlyVolRng)
summary (ANOVA)
TukeyHSD(ANOVA)

FlyDarkRng1 <- summarySE(FlyDarkRng, measurevar="SES", groupvars=c("Habitat","LifeZone"))
FlyDarkRng1

flydarkrng<-(ggplot(data=FlyDarkRng1, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
              geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=0.25, size = 1) +
              geom_line(size=1.5) +
              geom_point(size=6, shape=16,) + # 21 is filled circle
              ylim(-3,3) +  
              xlab("Life Zone") +
              ylab("sesRange Darkness") +
              scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                               
                               l=40)
            +                    # Use darker colors, lightness=40
              theme_bw()+
              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                    panel.background = element_blank(), axis.line = element_line(colour = "black")) +
              theme(legend.justification=c(1,0),
                    legend.position=c(1,0),legend.text=element_text(size=11)) +
              theme(axis.text=element_text(size=18, color="black"),
                    plot.title=element_text(size=24,face="bold"),axis.title=element_text(size=14,face="bold")))



setwd("C:/Users/lma243.NAU/OneDrive/Documents/Manuscripts/Trait_data_Paper_data/final_figures_5.12.17")
png("BeeDarkCWM.png")
plot(beedarkcwm)
dev.off()

png("FlyDarkCWM.png")
plot(flydarkcwm)
dev.off()

png("BeeDarkRng.png")
plot(beedarkrng)
dev.off()

png("FlyDarkRng.png")
plot(flydarkrng)
dev.off()

