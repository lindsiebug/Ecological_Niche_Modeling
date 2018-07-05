setwd("Y:/Projects/Bees/B_occidentalis/Current")
BC=read.csv("B_occidentalis_current_occurrences.csv",head=T)
library(ridigbio)
library(dismo)
library(XML)
library(biomod2)
library(rgdal)
library(raster)
library(fields)

BC= BC[,c("decimalLatitude","decimalLongitude")]
occurrences<- na.omit(BC)

occurrences$newcol <- rep(1,nrow(occurrences))

### reading in climate data
setwd("Y:/BIOMOD/Climate/BIOCLIM")

envStack=raster("bio_1.bil")
for (i in c(2,4,8,12,15,18)){
  envStack=stack(envStack,raster(paste("bio",paste(i,"bil",sep="."),sep="_")))
}
names(envStack)=c("MAT","TDiurn","TSeas","TWetQtr","MAP","PSeas","PWrmQtr")
envStack=crop(envStack,extent(-135,-85,25,70))


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

sp.i=2

#dir.create(paste("C:/Users/lma24/Dropbox/SFP Pollinators/modeling/Leps/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""))
#dir.create(paste(paste("Y:/Projects/Swallowtails/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"biomod_outputs",sep="/"))
#setwd(paste(paste("Y:/Projects/Swallowtails/",paste(spNames[sp.i,1],spNames[sp.i,2],sep="_"),sep=""),"biomod_outputs",sep="/"))
myBiomodData=BIOMOD_FormatingData(resp.var=as.matrix(respXY[,3]),expl.var=envStack,resp.xy=respXY[,1:2],resp.name="model_results")

myBiomodOption=BIOMOD_ModelingOptions()

###runing
myBiomodModelOut=BIOMOD_Modeling(myBiomodData,models=c('GLM','CTA','RF'),models.options=myBiomodOption,NbRunEval=3,DataSplit=70,Yweights=NULL,VarImport=10,models.eval.meth=c("ROC","TSS","KAPPA"),SaveObj=TRUE,rescal.all.models=TRUE)
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

setwd("Y:/Projects/Bees/B_occidentalis/Current/model.results")
myBiomodModelOut=get(load("model.results.1486233806.models.out"))
plot(myBiomodModelOut)
points(respXY)



dat <- matrix(stats::rnorm(2000), ncol = 2)
ch <- chull(gbifDat)
coords <- gbifDat[c(ch, ch[1]), ]  # closed polygon

plot(gbifDat, pch=19)
lines(coords, col="red", lty="solid")

data <- read.csv("occurrence-search-1316347695527.CSV",
                 header = T, sep = ",")

str(gbifDat)
require(maps)
png("test.png")
map("world")
points(gbifDat$lon, gbifDat$lat, col = 2, cex = 0.05)
text(-140, -50, "MYRICARIA\nGERMANICA")
dev.off()
getwd()

