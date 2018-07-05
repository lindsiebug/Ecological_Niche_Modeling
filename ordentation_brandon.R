#ordenations
setwd("C:/Users/Lindsie/OneDrive/Documents")
commDat=read.csv("Ant Species Raw Data.csv",na.strings=c("","NA"))
head(commDat)
comm=commDat[,-c(2:8)]#creating a dataset without all the species ID info, for the sake of convenience later
head(comm)
rownames(comm)=commDat[,1]
head(comm)
library(vegan)

comm <- comm[rowSums(comm == 0) != ncol(comm),]

comm.dca=decorana(commDat)
comm.dca
commDat2<- comm

#plot(comm.dca)
#plot(comm.dca,display="sites")
#plot(comm.dca,display="species")

#the raw outputs from the DCA can be accessed with the summary() function
#dcaSites=scores(comm.dca)
#dcaSpp=scores(comm.dca,display="species")
#plot(dcaSites[,2],dcaSites[,1],col=rep(c("Blue","Yellow","Black"),each=6),pch= rep(c(15,16,17), each=6),xlab="DCA2",ylab="DCA1")

library(ecodist)#this libary has a function for calculating Bray-Curtis distance
bcDist=bcdist(comm)
comm.nms=nmds(bcDist,mindim=1,maxdim=2)#NMDS utilizes a random seed to initiate calculation of orthogonal axes.  Because of this, the default is to run the analysis many times, then select the run with the lowest "stress" (disagreement between the raw and ordinated data).
comm.nms.min2=nmds.min(comm.nms)#this function finds the run with the minimum stress
par(oma = c(1, 1, 1, 1))
plot(comm.nms.min2[,1],comm.nms.min2[,2],col=rep(c("darkblue","red3","green"),each=3),pch= rep(c(15,22,16), each=3),xlab=" ",ylab=" ", main="Kendrick Bees")


data(commDat2)
ENV=read.csv("N:/Data/2013&2014/env.csv")
data(ENV)
bees.mrpp <- mrpp(commDat2, ENV, distance = "bray")

par(fig = c(0, 1, 0, 1), oma = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), new = TRUE)
legend("bottomleft", # places a legend at the appropriate place 
       c("PiPo Meadow","PiPo Forest","MixCon Meadow","MixCon Forest","Spruce Meadow","Spruce Forest"), # puts text in the legend 
       
       pch=c(16,21,15,22,17,24), # gives the legend appropriate symbols (lines)
       
       col=c("red3","darkblue","darkblue","darkblue",  "green", "green")) # gives the legend lines the correct color and width

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