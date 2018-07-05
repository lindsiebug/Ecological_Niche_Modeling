setwd ("C:/Users/lma24/OneDrive/PhD_Documents")
data2 = read.csv ("bee_block_meta_info.csv")
head(data2)


ANOVA <- aov(Distance~Colinization, data=data2)
summary (ANOVA)
TukeyHSD(ANOVA)

Dark_intra<-(ggplot(data=data2, aes(x=Distance, y=Colonization,)) + 
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
