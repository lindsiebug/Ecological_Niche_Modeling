RvC= read.csv ("C:/Users/lma243.NAU/Dropbox/SDModeling/megdata_adj.csv")
RvC=na.omit(RvC)
head(RvC)

library( MASS)
climate = glm.nb (Richness ~ AMT + MDR + ISO + TSea + MTWM + MTCM + 
                    TAR + MTWetQ + MTDQ + MTWarmQ + MTColdQ + AMP + 
                    PWM + PDM + PSean + PWetQ + PDryQ + PWarmQ + PColdQ, data=RvC)
summary (climate)

drop1 (climate, test = "Chi")
stepAIC (climate)





climate2 = glm.nb (Richness ~ TSea + MTCM +  MTWarmQ + MTColdQ + 
                     AMP + PWM + PDM + PSean +  PDryQ, data=RvC)
summary (climate2)
op = par (mfrow = c(2,2))
plot (climate2)
plot (op)




climate3 = glm (Richness ~ TSea + MTCM +  MTWarmQ + MTColdQ + 
                     AMP + PWM + PDM + PSean +  PDryQ, family = poisson, data=RvC)
summary (climate3)
