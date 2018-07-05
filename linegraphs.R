library (ggplot2)
setwd ("C:/Users/Lindsie/OneDrive/Documents")
Dark<-read.csv ("Darkness.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=Dark)
summary (ANOVA)
TukeyHSD(ANOVA)

Dark2 <- summarySE(Dark, measurevar="Darkness", groupvars=c("Group","LifeZone"))
Dark2


ggplot(data=Dark2, aes(x=LifeZone, y=Darkness, group=Group, color=Group)) + 
  geom_errorbar(aes(ymin=Darkness-se, ymax=Darkness+se), width=.25) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Darkness (100-saturation pixels)") +
  scale_colour_hue(name="Insect",    # Legend label, use darker colors
                   
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Darkness") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))






Vol<-read.csv ("Volume_all_NoHab.csv")
ANOVA <- aov(SES~LifeZone*Habitat, data=Vol)
summary (ANOVA)
TukeyHSD(ANOVA)

Vol2 <- summarySE(Vol, measurevar="SES", groupvars=c("Habitat","LifeZone"))
Vol2

ggplot(data=Vol2, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
  geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=.1) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Volume (mm3)") +
  scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                   
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Volume") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))











setwd ("F:/Final Data sets")
BeeVol<-read.csv ("Volume_all_bees1.csv")

BeeVol2 <- summarySE(BeeVol, measurevar="SES", groupvars=c("Habitat","LifeZone"))
BeeVol2

ggplot(data=BeeVol2, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
  geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=.1) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Volume (mm3)") +
  scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                                      labels=c("Meadow", "Forest"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Bee Volume") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12, color="black"),
         plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))





setwd ("F:/Final Data sets")
FlyVol<-read.csv ("Volume_all_flies1.csv")

FlyVol2 <- summarySE(FlyVol, measurevar="SES", groupvars=c("Habitat","LifeZone"))
FlyVol2

ggplot(data=FlyVol2, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
  geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=.1) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Volume (mm3)") +
  scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                   labels=c("Meadow", "Forest"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Fly Volume") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))


setwd ("F:/Final Data sets")
BeeDark<-read.csv ("Darkness_all_bees1.csv")


BeeDark2 <- summarySE(BeeDark, measurevar="SES", groupvars=c("Habitat","LifeZone"))
BeeDark2

ggplot(data=BeeDark2, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
  geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=.1) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Darkness (100-saturation pixels)") +
  scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                   labels=c("Meadow", "Forest"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Bee Darkness") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))



setwd ("F:/Final Data sets")
FlyDark<-read.csv ("Darkness_all_flies1.csv")


FlyDark2 <- summarySE(FlyDark, measurevar="SES", groupvars=c("Habitat","LifeZone"))
FlyDark2

ggplot(data=FlyDark2, aes(x=LifeZone, y=SES, group=Habitat, color=Habitat)) + 
  geom_errorbar(aes(ymin=SES-se, ymax=SES+se), width=.1) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Darkness (100-saturation pixels)") +
  scale_colour_hue(name="Habitat",    # Legend label, use darker colors
                   labels=c("Meadow", "Forest"),
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Fly Darkness") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=12)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))
