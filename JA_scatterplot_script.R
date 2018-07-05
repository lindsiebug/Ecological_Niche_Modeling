library(ggplot2)

###Abundance Temp by year and lifezone
MATA = lm(MAT~ Abundance, data=data)
qplot(MAT, Abundance, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Temperature")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MATA)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

###Richness Temp by year and lifezone
MATR = lm(MAT~ Richness, data=data)
qplot(MAT, Richness, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Temperature")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MATR)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )


###Abundance Precip by year and lifezone
MAPA = lm(MAP~ Abundance, data=data)
qplot(MAP, Abundance, colour = Year, shape = Life.Zone, 
      data = data)+  xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MAPA)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

###Richness Precip by year and lifezone
MAPR = lm(MAP~ Richness, data=data)
qplot(MAP, Richness, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MAPR)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )


###Abundance Monthly Precip by year and lifezone
MTA = lm(MT~ Abundance, data=data)
qplot(MT, Abundance, colour = Year, shape = Life.Zone, 
      data = data)+  xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MTA)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )

###Richness Precip by year and lifezone
MTR = lm(MT~ Richness, data=data)
qplot(MT, Richness, colour = Year, shape = Life.Zone, 
      data = data)+ xlab("Mean Annual Precipitation")+ ylab("Abudnace per Trap")+
  theme_bw() + geom_abline(data = MT)+
  theme(plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank() )
