setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Pollinator Study/kendrick")
ken<- read.csv("kendrick_master.csv")
head(ken)
ken[ken > 1] <- 1
head(ken)
write.csv (ken, "kendrick_master_species_richness.csv")

ken2<- read.csv("Kendrick_bee_fly_data3.csv")
head (ken2)


ken_sum <- summarySE(ken2, measurevar="Percent.Richness", groupvars=c("Group","Elevation"))
ken_sum


ggplot(data=ken_sum, aes(x=Elevation, y=Percent.Richness, group=Group, color=Group)) + 
  geom_errorbar(aes(ymin=Percent.Richness-se, ymax=Per+se), width=.25) +
  geom_line() +
  geom_point(size=4, shape=16) + # 21 is filled circle
  xlab("Elevation (m)") +
  ylab("Percent Species Richness") +
  scale_colour_hue(name="Insect",    # Legend label, use darker colors
                   
                   l=40) +                    # Use darker colors, lightness=40
  ggtitle("Kendrick Speice Richness Transition") 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.justification=c(1,0),
        legend.position=c(1,0),legend.text=element_text(size=11)) +
  theme(axis.text=element_text(size=12, color="black"),
        plot.title=element_text(size=18,face="bold"),axis.title=element_text(size=14,face="bold"))
