setwd("C:/Users/lma243.NAU/Dropbox/SCAN_Collection Management_&_IPTs/LepNet_INST")
spNames=read.csv("LepNet_INST.csv",head=FALSE)#read in species list; remove "head=F" if have column name

i=1
for (i in 1:14){
  dir.create(paste("C:/Users/lma243.NAU/Dropbox/SCAN_Collection Management_&_IPTs",paste(spNames[i,1],sep="_"),sep="/"))
}