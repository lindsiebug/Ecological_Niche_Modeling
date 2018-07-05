setwd("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps")
spNames=read.csv("Swallowtail_list.csv",head=F)#read in species list; remove "head=F" if have column name
library(ridigbio)
library(dismo)
library(XML)

i=2
gbifDat=gbif(tolower(spNames[i,1]),paste(spNames[i,2],"*",sep=""))
write.csv(gbifDat,file=paste("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps/","_OR.csv",sep=paste(spNames[i,1],spNames[i,2],sep="_")))
gbifDat= gbifDat[,c(72:79)]
gbifDat= gbifDat[,-c(2:7)]
gbifDat<- na.omit(gbifDat)
#write.csv(gbifDat,file=paste("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps/",".csv",sep=paste(spNames[i,1],spNames[i,2],sep="_")))

idigbioDat <- idig_search_records(rq=list(genus=paste(spNames[i,1],"",sep=""), specificepithet=paste(spNames[i,2],"",sep="")), fields = "geopoint")
idigbioDat<- na.omit(idigbioDat)
names(idigbioDat) <- c("lon","lat")

ORDat<-rbind(gbifDat,idigbioDat)

write.csv(ORDat,file=paste("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps/",".csv",sep=paste(spNames[i,1],spNames[i,2],sep="_")))
