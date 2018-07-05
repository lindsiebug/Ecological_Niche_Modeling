sp.i=2

library(biomod2)
library(rgdal)

setwd("C:/Users/Lindsie/OneDrive")
spNames=as.data.frame(read.csv("moth_list.csv",head=T))


### reading in climate data
setwd("W:/BIOMOD/Climate/BIOCLIM")

envStack=raster("bio_1.bil")
for (i in c(2,3,4,6,7,8,9,18,19)){
  envStack=stack(envStack,raster(paste("bio",paste(i,"bil",sep="."),sep="_")))
}
names(envStack)=c("MAT","DinRng","Iso","TAR","TWetQrt","TDryQrt","PColQrt","PWetQrt","PDryQrt","TColQrt")
envStack=crop(envStack,extent(-130,-90,30,70))


library(dismo)
library(XML)




#######old code#######
#sp.i=2
### reading in occurrences and filtering to the raster
#setwd("C:/Users/Lindsie/OneDrive/moths")
#gbifDat=gbif(tolower(spNames[sp.i,1]),paste(spNames[sp.i,2],"*",sep=""),ext=extent(-142,-97,41,61))

#occurrences=gbifDat[complete.cases(gbifDat[,c(9,8)]),c(9,8)]
#occurrences=cbind(occurrences,rep(1,nrow(occurrences)))
#occurrences=read.csv(paste("C:/Users/Lindsie/OneDrive/moths/occurrence/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),paste(".csv"),sep=""))
#occurrences=read.csv("C:/Users/Lindsie/OneDrive/moths/occurrence/Arctia_caja.csv")
#occurrences=occurrences[complete.cases(occurrences),]
#occurrences=cbind(occurrences,rep(1,nrow(occurrences)))
#######old code########

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

occurrences<-rbind(gbifDat,idigbioDat)

write.csv(occurrences,file=paste("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps/","_cleaned.csv",sep=paste(spNames[i,1],spNames[i,2],sep="_")))



occurrences_filtered=rasterize(occurrences[,c(2,1)],envStack,field=occurrences[,3],fun=function(x,...)max(x))


#occurrences_filtered=rasterize(occurrences[,1:2],envStack,field=occurrences[,3],fun=function(x,...)max(x))

respXY=cbind(xyFromCell(occurrences_filtered,cell=which(occurrences_filtered[]==1)),c(rep(1)))
climDat=extract(envStack,respXY[,1:2])
respXY=respXY[complete.cases(climDat),]

### setting extent
climDat=climDat[complete.cases(climDat),]

climLim=vector()
for (i in 1:ncol(climDat)){
  lim.i=vector()
  for (j in 1:999){
    dist.i=rnorm(3*nrow(climDat),mean=mean(climDat[,i]),sd=sd(climDat[,i]))
    lim.i=cbind(lim.i,c(min(dist.i),max(dist.i)))
  }
  climLim=cbind(climLim,rowMeans(lim.i))
}
colnames(climLim)=colnames(climDat)
rownames(climLim)=c("min","max")

is.between=function(x,a,b){
  (x-a)*(b-x)>0
}

climLimRast=is.between(envStack[[1]],climLim[1,1],climLim[2,1])
for (i in 2:ncol(climLim)){
  climLimRast=climLimRast*is.between(envStack[[i]],climLim[1,i],climLim[2,i])
}

#removing climatic outlier presences
presRast=rasterize(respXY[,1:2],climLimRast,field=respXY[,3],fun=function(x,...)max(x))
presRast=mask(presRast,climLimRast,maskvalue=0)
presRast=mask(presRast,climLimRast)
presPoints=xyFromCell(presRast,cell=which(!is.na(presRast[])))
abstPoints=xyFromCell(presRast,cell=sample(which(climLimRast[]==1&is.na(presRast[])),3*nrow(presPoints)))
respXY=rbind(cbind(presPoints,rep(1,nrow(presPoints))),cbind(abstPoints,rep(0,nrow(abstPoints))))

### formatting data for BIOMOD
envUnstack=unstack(envStack)
envStack=stack(envUnstack)
rm(envUnstack)

dir.create(paste("C:/Users/Lindsie/OneDrive/moths/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""))
dir.create(paste(paste("C:/Users/Lindsie/OneDrive/moths/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"biomod_outputs",sep="/"))
setwd(paste(paste("C:/Users/Lindsie/OneDrive/moths/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"biomod_outputs",sep="/"))
myBiomodData=BIOMOD_FormatingData(resp.var=as.matrix(respXY[,3]),expl.var=envStack,resp.xy=respXY[,1:2],resp.name="model_results")

myBiomodOption=BIOMOD_ModelingOptions()

###runing
myBiomodModelOut=BIOMOD_Modeling(myBiomodData,models=c('GLM','CTA','ANN','RF'),models.options=myBiomodOption,NbRunEval=3,DataSplit=60,Yweights=NULL,VarImport=10,models.eval.meth=c("ROC","TSS","KAPPA"),SaveObj=TRUE,rescal.all.models=TRUE)
myBiomodModelEval=getModelsEvaluations(myBiomodModelOut)
write.csv(myBiomodModelEval,"model_evals_all_models.csv")

varImport=getModelsVarImport(myBiomodModelOut)
varImportMean=vector()
for (i in 1:8){
  varImportMean=cbind(varImportMean,rowMeans(varImport[,i,-11,]))
}
#colnames(varImportMean)=colnames(varImport)
write.csv(varImportMean,"varImportMean.csv")

myBiomodProj=BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = envStack, proj.name = "current", selected.models = "all", binary.meth = "ROC", compress = "xz", clamping.mask = F, keep.in.memory = T)

### creating ensemble projection
myBiomodEM=BIOMOD_EnsembleModeling(modeling.output = myBiomodModelOut, chosen.models = "all", em.by = "all", eval.metric = "all", eval.metric.quality.threshold = NULL, prob.mean = TRUE, prob.cv = FALSE, prob.ci = FALSE, prob.ci.alpha = 0.05, prob.median = FALSE, committee.averaging = FALSE, prob.mean.weight = FALSE, prob.mean.weight.decay = "proportional")
#save(myBiomodEM,file=paste(paste(levels(spNames[i,1]),levels(spNames[i,2]),sep="."),"ensemble.models.out",sep=".")
write.csv(getEMeval(myBiomodEM),"EM_evals.csv")

#MyBiomodEF=BIOMOD_EnsembleForecasting(projection.output = myBiomodProj, EM.output = myBiomodEM)







#modelEval=read.csv("C:/Users/Lindsie/OneDrive/moths/Arachnis_citra/biomod_outputs/model_evals_all_models.csv")
#current=raster("C:/Users/Lindsie/OneDrive/moths/Arachnis_citra/biomod_outputs/model.results/proj_current/proj_current_model.results_ensemble.grd")
current=raster(paste("C:/Users/Lindsie/OneDrive/moths/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),paste("/biomod_outputs/model.results/proj_current/proj_current_model.results_ensemble.grd"),sep=""))


######Range Code#######
library(raster)
library(fields)
setwd('W:/2015_Fall/Closed/Week13_Nov24/example_model')
respXY=read.csv('respXY.csv',row.names=1)
projCurrent=raster('proj_current_model.output.grd')
projCurrent[projCurrent<200]=0
projCurrent[projCurrent>0]=1

plot(projCurrent)
points(respXY[,1:2])

pseudoAbs=xyFromCell(projCurrent,cell=sample(which(projCurrent[]==0),3*nrow(respXY)))
pseudoAbs=cbind(pseudoAbs,rep(0,nrow(pseudoAbs)))
colnames(pseudoAbs)=colnames(respXY)

presAbs=rbind(respXY,pseudoAbs)

presKrig=Tps(presAbs[,1:2],presAbs[,3])

interp=interpolate(projCurrent,presKrig,xyOnly=FALSE)
interp=mask(interp,projCurrent)
plot(interp)
points(respXY[,1:3])

interp[interp<0.3]=0
plot(interp)
points(respXY[,1:3])

interp[interp>0]=1
plot(interp)
points(respXY[,1:3])





####Current sutable map####
jpeg(paste("C:/Users/Lindsie/OneDrive/moths/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),paste("/SDM2.jpg"),sep=""),width=6.5,height=6.5,units="in",res=600)
plot(current,col=c("grey","green"),legend=FALSE)
#points (respXY, pch=20, col="blue")
dev.off()








