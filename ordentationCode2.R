#ordenations
setwd("C:/Users/lma243.NAU/Dropbox/SFP Pollinators/Data/Plant_host_associations")
commDat=read.csv("June_2016_plants_NMDS.csv")
commDat[is.na(commDat)] <- 0
head(commDat)
comm=commDat[,-c(1:1)]#creating a dataset without all the species ID info, for the sake of convenience later
comm=comm[-c(33:33),]
rownames(comm)=commDat[,1]
head(comm)
library(vegan)

comm.dca=decorana(comm)
comm.dca
commDat2<- comm

plot(comm.dca)
plot(comm.dca,display="sites")
plot(comm.dca,display="species")

#the raw outputs from the DCA can be accessed with the summary() function
dcaSites=scores(comm.dca)
dcaSpp=scores(comm.dca,display="species")
plot(dcaSites[,2],dcaSites[,1],col=rep(c("Blue","Yellow","Black"),each=6),pch= rep(c(15,16,17), each=6),xlab="DCA2",ylab="DCA1")

library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(comm)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3","green","darkgoldenrod2"),each=8),pch= rep(c(15,18,16,17), each=8),xlab=" ",ylab=" ", main="June Flowers by Life Zone")


data(commDat2)
ENV=read.csv("N:/Data/2013&2014/env.csv")
data(ENV)
bees.mrpp <- mrpp(commDat2, ENV, distance = "bray")

par(fig = c(0, 1, 0, 1), oma = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), new = TRUE)
legend("bottomleft", # places a legend at the appropriate place 
       c("Pinyon-Juniper","Ponderosa","Mixed Conifer","Spruce-fir"), # puts text in the legend 
       
       pch=c(16,18,16,17), # gives the legend appropriate symbols (lines)
       
       col=c("darkblue","red3","green","darkgoldenrod2")) # gives the legend lines the correct color and width

plot(hclust(bcDist,method="average"),labels=colnames(comm))

setwd ("N:/Data/2014")
png(file="beeordenation2014.png",width=400,height=350,res=72)
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3","green"),each=8),pch= rep(c(15,22,16,21,17,24), each=4),xlab=" ",ylab=" ", main="Bees 2014")
dev.off()

comm.perm=adonis(comm)~rep(c("low","mid","high"),each=3),method="bray")
comm.perm#this prints out a typical ANOVA table

comm.perm=adonis(t(comm)~rep(c("Ponderosa","Mixed Conifer","Spruce-fir"),each=16),method="bray")
comm.perm#this prints out a typical ANOVA table

comm.perm.NarBC=adonis(t(comm[,1:12])~rep(c("Ponderosa","Mixed Conifer"),each=6),method="bray")
comm.perm.NarBC#no difference
comm.perm.FreF1=adonis(t(comm[,7:18])~rep(c("Mixed Conifer","Spruce-fir"),each=6),method="bray")
comm.perm.FreF1#significant difference

comm.perm.NarBC=adonis(t(comm[,1:6])~rep(c("PMeadow","PForest"),each=3),method="bray")
comm.perm.NarBC#no difference
comm.perm.NarBC=adonis(t(comm[,7:12])~rep(c("MMeadow","MForest"),each=3),method="bray")
comm.perm.NarBC#no difference
comm.perm.NarBC=adonis(t(comm[,13:18])~rep(c("SMeadow","SForest"),each=3),method="bray")
comm.perm.NarBC#no difference

citation()