library (bipartite)

setwd("C:/Users/lma243.NAU/Dropbox/SFP Pollinators/Data/Plant_host_associations")
WebDat <- read.csv ("July_Insects_Plants.csv")
WebDat2=WebDat[,-c(1:1)]
WebDat2[is.na(WebDat2)] <- 0
rownames(WebDat2)= WebDat[,1]


plotweb(WebDat2)
visweb(WebDat2)
