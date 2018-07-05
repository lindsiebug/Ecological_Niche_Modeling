setwd ("C:/Users/Lindsie/OneDrive/Documents")
dat<- read.csv("perdiction_graphA.csv")
dat2<- read.csv("perdiction_graphSR.csv")
head(dat)
head(dat2)
library(ggplot2)

Abundance<-ggplot(data=dat, aes(x=Elevation, y=Abundance, group=Group,label="TRUE")) + 
          geom_line(size=1, linetype=c("dashed","dashed","dashed","dashed","dashed","dashed","dashed","solid","solid","solid","solid","solid","solid","solid"), color=c("green","blue","green","red","red","blue","red","green","blue","green","red","red","blue","red") )+
          geom_point(size=4, shape=c(15,15,15,15,15,15,15,17,17,17,17,17,17,17), color=c("green","blue","green","red","red","blue","red","green","blue","green","red","red","blue","red")) + # 21 is filled circle
          xlab("Elevation (meters)") +
          ylab("Abundance (%)") +
          scale_colour_hue(name="Insect",    # Legend label, use darker colors                             
          l=40) +                    # Use darker colors, lightness=40
          ggtitle("Abundance") +
          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.background = element_blank(), axis.line = element_line(colour = "black")) +
          theme(legend.justification=c(1,0),
              legend.position=c(1,0),legend.text=element_text(size=11)) 
         
Richness<-ggplot(data=dat2, aes(x=Elevation, y=SpeciesRichness, group=Group)) + 
  geom_line(size=1, linetype=c("dashed","dashed","dashed","dashed","dashed","dashed","dashed","dashed","dashed","solid","solid","solid","solid","solid","solid","solid","solid","solid"), 
            color=c("red2","darkgoldenrod1","darkgoldenrod1","darkmagenta","gray48","darkmagenta","gray48","red2","red2","red2","darkgoldenrod1","darkgoldenrod1","darkmagenta","gray48","darkmagenta","gray48","red2","red2") )+
  geom_point(size=4, shape=c(15,15,15,15,15,15,15,15,15,17,17,17,17,17,17,17,17,17), color=c("red2","darkgoldenrod1","darkgoldenrod1","darkmagenta","gray48","darkmagenta","gray48","red2","red2","red2","darkgoldenrod1","darkgoldenrod1","darkmagenta","gray48","darkmagenta","gray48","red2","red2")) + # 21 is filled circle
  xlab("Elevation (meters)") +
  ylab("Species Richness (%)") +
  scale_colour_hue(name="Insect",    # Legend label, use darker colors                             
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Species Richness") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11))

par(mfrow=c(2,1))
plot(Abundance)
plot(Richness)

