setwd("Y:/Projects/Swallowtails")
spNames=read.csv("Swallowtail_list.csv",head=F)#read in species list; remove "head=F" if have column name
library(ridigbio)
### future models
i=1
library(biomod2)
#setwd(paste(paste("Y:/BIOMOD/Biological_Ranges",levels(spNames[i,3]),sep="/"),"biomod_outputs",sep="/"))
setwd("Y:/Projects/Swallowtails/Papilio_brevicauda/biomod_outputs/model.results")
#load(paste(paste(levels(spNames[i,1]),levels(spNames[i,2]),sep="."),".models.out",sep="."))
load("model.results.1483998626.models.out")
#myBiomodModelOut=((paste(paste(levels(spNames[i,1]),levels(spNames[i,2]),sep="."),".models.out",sep=".")))
myBiomodModelOut=("model.results.1483998626.models.out")


#cnrm_cm3 for 2080s
setwd("X:/GCMS/Statistically_Downscaled/A2/cnrm_cm3/2080s/nps_crop")###!!!! this is the wrong directory, map to cpbiennial
envStack=stack("envStack_cnrm_cm3_2080s_crop.gri")
names(envStack)=c("MAT","TDiurn","TSeas","TWetQtr","MAP","PSeas","PWrmQtr")

setwd("Y:/Projects/Swallowtails/Papilio_brevicauda")
cnrm_cm3_A2_2080s=BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = envStack, proj.name = 'cnrm_cm3_A2_2080s', selected.models = "all", binary.meth = "ROC", compress = "xz", clamping.mask = F, keep.in.memory = T)

cnrm_cm3_A2_2080s_EF=BIOMOD_EnsembleForecasting(projection.output=cnrm_cm3_A2_2080s,EM.output=myBiomodEM)
unlink("C:/Users/bjb329/AppData/Local/Temp/R_raster_bjb329/*")

#ukmo_hadgem for 2080s
#setwd("X:/GCMS/Statistically_Downscaled/A2/ukmo_hadgem1/2080s/nps_crop")
#envStack=stack("envStack_ukmo_hadgem1_2080s_crop.gri")
#names(envStack)=c("MAT","TDiurn","TSeas","TWetQtr","MAP","PSeas","PWrmQtr")

#setwd("Z:/SW_vegetation_resilience/Troy_SDMs/Achnatherum_hymenoides/biomod_output")
#ukmo_hadgem1_A2_2080s=BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = envStack, proj.name = 'ukmo_hadgem1_A2_2080s', selected.models = "all", binary.meth = "ROC", compress = "xz", clamping.mask = F, keep.in.memory = T)

#ukmo_hadgem1_A2_2080s_EF=BIOMOD_EnsembleForecasting(projection.output=ukmo_hadgem1_A2_2080s,EM.output=myBiomodEM)
#unlink("C:/Users/bjb329/AppData/Local/Temp/R_raster_bjb329/*")

#mpi_echam5 for 2080s
#setwd("X:/GCMS/Statistically_Downscaled/A2/mpi_echam5/2080s/nps_crop")
#envStack=stack("envStack_mpi_echam5_2080s_crop.gri")
#names(envStack)=c("MAT","TDiurn","TSeas","TWetQtr","MAP","PSeas","PWrmQtr")

#setwd("Z:/SW_vegetation_resilience/Troy_SDMs/Achnatherum_hymenoides/biomod_output")
#mpi_echam5_A2_2080s=BIOMOD_Projection(modeling.output = myBiomodModelOut, new.env = envStack, proj.name = 'mpi_echam5_A2_2080s', selected.models = "all", binary.meth = "ROC", compress = "xz", clamping.mask = F, keep.in.memory = T)

#mpi_echam5_A2_2080s_EF=BIOMOD_EnsembleForecasting(projection.output=mpi_echam5_A2_2080s,EM.output=myBiomodEM)
#unlink("C:/Users/bjb329/AppData/Local/Temp/R_raster_bjb329/*")

#mean of GCM ensembles
setwd("Z:/SW_vegetation_resilience/Troy_SDMs/Achnatherum_hymenoides/biomod_output/Achnatherum.hymenoides")
cnrm=raster("proj_cnrm_cm3_A2_2080s/proj_cnrm_cm3_A2_2080s_Achnatherum.hymenoides_ensemble")
#ukmo=raster("proj_ukmo_hadgem1_A2_2080s/proj_ukmo_hadgem1_A2_2080s_Achnatherum.hymenoides_ensemble")
#mpi=raster("proj_mpi_echam5_A2_2080s/proj_mpi_echam5_A2_2080s_Achnatherum.hymenoides_ensemble")
meanEM2080s=mean(cnrm)
writeRaster(meanEM2080s,"meanEM2080s.grd")

######Stop here##########
###Plot #####

modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_brevicauda/biomod_outputs/EM_evals.csv")
current=raster("Y:/Projects/Swallowtails/Papilio_brevicauda/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
current[current>=modelEval[2,3]]=1002
current[current<modelEval[2,3]]=1001
plot(current)

future=raster("meanEM2080s.grd")
future[future>=modelEval[2,3]]=1007
future[future<modelEval[2,3]]=1004

CurrPlus2080s=current+future

CPshp=readOGR("Y:/Projects/PJ_Woodland/CP_shapefile","CP_mask")
CPshp=spTransform(CPshp,CRS(proj4string(CurrPlus2080s)))

CurrPlus2080sCP=mask(CurrPlus2080s,CPshp)
CurrPlus2080sCP=crop(CurrPlus2080sCP,extent(CPshp))

jpeg("Z:/SW_vegetation_resilience/Troy_SDMs/Achnatherum_hymenoides/biomod_output/Current2080sComparison.jpg",width=6.5,height=6.5,units="in",res=600)
plot(CurrPlus2080s,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()

jpeg("Z:/SW_vegetation_resilience/Troy_SDMs/Achnatherum_hymenoides/biomod_output/Current2080sComparisonCP.jpg",width=6.5,height=6.5,units="in",res=600)
plot(CurrPlus2080sCP,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()

