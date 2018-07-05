library (bipartite)

setwd("C:/Users/Lindsie/Dropbox/SFP Pollinators/Plant_host_associations")
WebDat <- read.csv ("Insect-Plants.csv")
labels <- read.csv ("plant-pollinator_list.csv")
plant <- read.csv("plant_list.csv")
colnames(WebDat)=labels$pollinators
rownames(WebDat)=plant$plant


plotweb(WebDat)
visweb(WebDat)
