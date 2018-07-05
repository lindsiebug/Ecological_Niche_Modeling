###Despriptave Stats####
setwd("S:/Dugway_Monitoring/Projects/Herp_Pitfall/Data Entry/Pitfall 2015/R_analysis")
DPG <- read.csv("2011_May2015_final_es.csv")
library ("pastecs")

attach(DPG)
DPG<-DPG[,-c(1:11)]
options(scipen=100)
options(digits=2)
Summary<-stat.desc(DPG)

write.csv(x = Summary, file = "Summary_Stats_2015.csv")

