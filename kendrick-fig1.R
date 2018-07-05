library(ggplot2)

setwd ("C:/Users/lma24/Dropbox/SFP Pollinators/Data/All_year_data")
Kendrick = read.csv("All_year_kendrick.csv")
Meadow = read.csv ("All_year_SF_meadow.csv")
Forest = read.csv ("All_year_SF_forest.csv")


#ANOVA <- aov(Darkness~LifeZone*Group, data=Dark)
#summary (ANOVA)
#TukeyHSD(ANOVA)

Kendrick_abn <- summarySE(Kendrick, measurevar="P.Abundance", groupvars=c("Group","Life.Zone"))
Kendrick_abn
Meadow_abn<-summarySE(Meadow, measurevar="P.Abundance", groupvars=c("Group","Life.Zone"))
Meadow_abn
Forest_abn<- summarySE(Forest, measurevar="P.Abundance", groupvars=c("Group","Life.Zone"))
Forest_abn

kendrick_ab<-(ggplot(data=Kendrick_abn, aes(x=Life.Zone, y=P.Abundance, group=Group, color=Group)) + 
  geom_errorbar(aes(ymin=P.Abundance-se, ymax=P.Abundance+se), width=0.25) +
  geom_line(size=1) +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Abundance (%)") +
  scale_colour_hue(name="Insect",    # Legend label, use darker colors
                   
                   l=40)
  +                    # Use darker colors, lightness=40
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold")))


Vol_all<- (ggplot(data=Vol2, aes(x=LifeZone, y=Volume, group=Group, color=Group)) + 
  geom_errorbar(aes(ymin=Volume-se, ymax=Volume+se), width=.25) +
  geom_line(size=1) +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Life Zone") +
  ylab("Volume (mm3)") +
  scale_colour_hue(name="Insect",    # Legend label, use darker colors
                   
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Volume: All Species") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold")))


Dark_intra<-(ggplot(data=Dark3, aes(x=LifeZone, y=Darkness, group=Group, color=Group)) + 
             geom_errorbar(aes(ymin=Darkness-se, ymax=Darkness+se), width=.25) +
             geom_line(size=1) +
             geom_point(size=4, shape=16) + # 21 is filled circle
             xlab("Life Zone") +
             ylab("Darkness (100-saturation pixels)") +
             scale_colour_hue(name="Insect",    # Legend label, use darker colors
                              
                              l=40) +                    # Use darker colors, lightness=40
             ggtitle("Darkness: All Life Zone Species") +
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
             theme(legend.justification=c(1,0),
                   legend.position=c(1,0),legend.text=element_text(size=11)) +
             theme(axis.text=element_text(size=12, color="black"),
                   plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold")))


Vol_intra<- (ggplot(data=Vol3, aes(x=LifeZone, y=Volume, group=Group, color=Group)) + 
             geom_errorbar(aes(ymin=Volume-se, ymax=Volume+se), width=.25) +
             geom_line(size=1) +
             geom_point(size=4, shape=16) + # 21 is filled circle
             xlab("Life Zone") +
             ylab("Volume (mm3)") +
             scale_colour_hue(name="Insect",    # Legend label, use darker colors
                              
                              l=40) +                    # Use darker colors, lightness=40
             ggtitle("Volume: All Life Zone Species") +
             theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                   panel.background = element_blank(), axis.line = element_line(colour = "black")) +
             theme(legend.justification=c(1,0),
                   legend.position=c(1,0),legend.text=element_text(size=11)) +
             theme(axis.text=element_text(size=12, color="black"),
                   plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold")))
Dark_all1<-Dark_all+ annotate ("text", x= c(1,2,3,1,2,3),y= c(10,31,37,30,37,65), label= c("a","b","b","b","b","c"))
Vol_all1<- Vol_all+ annotate ("text", x= c(1,2,3,1,2,3),y= c(130,210,300,385,390,820), label= c("a","b","c","d","d","e"))
Dark_intra1 <-Dark_intra+ annotate ("text", x= c(1,2,3,1,2,3),y= c(4,25,30,35,42,38), label= c("a","b","b","b","b","b"))
Vol_intra1 <- Vol_intra + annotate ("text", x= c(1,2,3,1,2,3),y= c(175,200,390,275,350,620), label= c("a","a","a","a","a","b"))

multiplot(Dark_all1, Vol_all1, Dark_intra1, Vol_intra1, cols=2)
text(c(1.5,2.5,4.5,5.5),c(25,31,14,47),labels=c("A","B","C","D"))
Dark_all
Vol_all
Dark_intra
Vol_intra