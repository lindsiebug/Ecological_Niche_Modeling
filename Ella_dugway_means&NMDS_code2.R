setwd("C:/Users/lma24/Downloads")
data <- read.csv("2011_2015_final_pitfall_9.3.16.csv")
library(plyr)
commDat <- ddply(data, .(RID), colwise(mean))
write.csv(x = mean, file = "Dugway_means_all.csv")


#ordenations
head(commDat)
commDat= commDat[,-c(562:590)]
head(commDat)
comm=commDat[,-c(1:16)]#creating a dataset without all the species ID info, for the sake of convenience later
head(comm)
rownames(comm)=commDat[,1]
comm = comm[-c(1:1),]
comm[is.na(comm)]<- 0
head(comm)
library(vegan)


#####2011###
setwd("/Volumes/cpmab/CPMAB_Projects/Dugway_Monitoring/Progress Reports/2016/Annual/STATS/Data_RCodes/Ordinations")
comm=read.csv("Dugway_means_all_es.csv")
CheatGrass = comm[c(2:31),(8:552)] 
comm[is.na(comm)]<- 0
CheatGrass[is.na(CheatGrass)] <- 0
head(CheatGrass)

library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(CheatGrass)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3"),each=,6),pch= rep(c(16,16,16,16,16,16), each=6),xlab=" ",ylab=" ", main="Dugway Cheatgrass Variation")

legend("bottomleft", # places a legend at the appropriate place 
       c("cheatgrass","non-cheatgrass"), # puts text in the legend 
       pch=c(16,16,16,16,16,16), # gives the legend appropriate symbols (lines)
       
       col=c("darkblue","red3")) # gives the legend lines the correct color and width

##20011##
library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(comm)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3"),each=6),pch= rep(c(16,16,16), each=6),xlab=" ",ylab=" ", main="Dugway Year Variation")

legend("bottomleft", # places a legend at the appropriate place 
       c("cheatgrass","non-cheatgrass"), # puts text in the legend 
       pch=c(16,16,16,16,16,16), # gives the legend appropriate symbols (lines)
       
       col=c("darkblue","red3")) # gives the legend lines the correct color and width


##Original##
library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(comm)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3","green","yellow","pink"),each=3),pch= rep(c(16,16,16), each=3),xlab=" ",ylab=" ", main="Dugway Year Variation")

legend("bottomleft", # places a legend at the appropriate place 
       c("2011","2012","2013","2014","2015"), # puts text in the legend 
       pch=c(16,16,16,16,16,16), # gives the legend appropriate symbols (lines)
       
       col=c("darkblue","red3","green","yellow","pink")) # gives the legend lines the correct color and width
