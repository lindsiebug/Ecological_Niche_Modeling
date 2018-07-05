#ordenations
setwd("C:/Users/Lindsie/OneDrive")
commDat=read.csv("Kendrick_bees.csv",na.strings=c("","NA"))
head(commDat)
comm=commDat[,-c(1:3)]#creating a dataset without all the species ID info, for the sake of convenience later
head(comm)
rownames(comm)=commDat[,1]
head(comm)
library(vegan)

comm.dca=decorana(comm)
comm.dca
commDat2<- comm

library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(comm)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3","green"),each=3),pch= rep(c(15,17,16), each=3), cex=1.25, xlab=" ",ylab=" ", main="Kendrick Bees")
