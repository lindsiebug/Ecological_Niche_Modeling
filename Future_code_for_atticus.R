modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_machaon/biomod_outputs/EM_evals.csv")
currentPM=raster("Y:/Projects/Swallowtails/Papilio_machaon/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
currentPM[currentPM>=modelEval[2,3]]=1002
currentPM[currentPM<modelEval[2,3]]=1001

futurePM=raster("Y:/Projects/Swallowtails/Papilio_canadensis/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
futurePM[futurePM>=modelEval[2,3]]=1007
futurePM[futurePM<modelEval[2,3]]=1004

Papilio_Machaon=currentPM+futurePM
fullDistroDat=summary(as.factor(values(Papilio_Machaon)))
write.csv(fullDistroDat,"Y:/Projects/Swallowtails/Papilio_Machaon_stats.csv")

jpeg("Y:/Projects/Swallowtails/Papilio_machaon.jpg",width=6.5,height=6.5,units="in",res=600)
plot(Papilio_Machaon,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()



####################################################

modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_zelicaon/biomod_outputs/EM_evals.csv")
currentPZ=raster("Y:/Projects/Swallowtails/Papilio_zelicaon/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
currentPZ[currentPZ>=modelEval[2,3]]=1002
currentPZ[currentPZ<modelEval[2,3]]=1001
plot(currentPZ)

futurePZ=raster("Y:/Projects/Swallowtails/Papilio_machaon/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
futurePZ[futurePZ>=modelEval[2,3]]=1007
futurePZ[futurePZ<modelEval[2,3]]=1004

Papilio_zelicaon=currentPZ+futurePZ
fullDistroDat=summary(as.factor(values(Papilio_zelicaon)))
write.csv(fullDistroDat,"Y:/Projects/Swallowtails/Papilio_zelicaon_stats.csv")

jpeg("Y:/Projects/Swallowtails/Papilio_zelicaon.jpg",width=6.5,height=6.5,units="in",res=600)
plot(Papilio_zelicaon,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()



###########################################################################


modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_rutulus/biomod_outputs/EM_evals.csv")
currentPR=raster("Y:/Projects/Swallowtails/Papilio_rutulus/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
currentPR[currentPR>=modelEval[2,3]]=1002
currentPR[currentPR<modelEval[2,3]]=1001

futurePR=raster("Y:/Projects/Swallowtails/Papilio_indra/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
futurePR[futurePR>=modelEval[2,3]]=1007
futurePR[futurePR<modelEval[2,3]]=1004

Papilio_rutulus=currentPR+futurePR
fullDistroDat=summary(as.factor(values(Papilio_rutulus)))
write.csv(fullDistroDat,"Y:/Projects/Swallowtails/Papilio_rutulus_stats.csv")

jpeg("Y:/Projects/Swallowtails/Papilio_rutulus.jpg",width=6.5,height=6.5,units="in",res=600)
plot(Papilio_rutulus,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()


#########################################################################


modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_eurymedon/biomod_outputs/EM_evals.csv")
currentPE=raster("Y:/Projects/Swallowtails/Papilio_eurymedon/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
currentPE[currentPE>=modelEval[2,3]]=1002
currentPE[currentPE<modelEval[2,3]]=1001

futurePE=raster("Y:/Projects/Swallowtails/Papilio_rutulus/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
futurePE[futurePE>=modelEval[2,3]]=1007
futurePE[futurePE<modelEval[2,3]]=1004

Papilio_eurymedon=currentPM+futurePE
fullDistroDat=summary(as.factor(values(Papilio_eurymedon)))
write.csv(fullDistroDat,"Y:/Projects/Swallowtails/Papilio_eurymedon_stats.csv")

jpeg("Y:/Projects/Swallowtails/Papilio_eurymedon.jpg",width=6.5,height=6.5,units="in",res=600)
plot(Papilio_eurymedon,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()

#########################################################################

modelEval=read.csv("Y:/Projects/Swallowtails/Papilio_indra/biomod_outputs/EM_evals.csv")
currentPT=raster("Y:/Projects/Swallowtails/Papilio_indra/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
currentPT[currentPT>=modelEval[2,3]]=1002
currentPT[currentPT<modelEval[2,3]]=1001


futurePT=raster("Y:/Projects/Swallowtails/Parnassius_phoebus/biomod_outputs/model.results/proj_current/proj_current_model.results.grd")
futurePT[futurePT>=modelEval[2,3]]=1007
futurePT[futurePT<modelEval[2,3]]=1004

Papilio_indra=currentPT+futurePT
fullDistroDat=summary(as.factor(values(Papilio_indra)))
write.csv(fullDistroDat,"Y:/Projects/Swallowtails/Papilio_indra_stats.csv")


jpeg("Y:/Projects/Swallowtails/Papilio_indra.jpg",width=6.5,height=6.5,units="in",res=600)
plot(Papilio_indra,col=c("grey","red","blue","purple"),legend=FALSE)
dev.off()


setwd ("C:/Users/lma243.NAU/Dropbox/Ecological Niche Modeling/Leps_Modeling/current_sutable_habitat_maps")
jpeg("Papilio_indra_current.jpg",width=6.5,height=6.5,units="in",res=600)
plot(currentPT,legend=FALSE)
dev.off()
jpeg("Papilio_eurymedon_current.jpg",width=6.5,height=6.5,units="in",res=600)
plot(currentPE,legend=FALSE)
dev.off()
jpeg("Papilio_machaon_current.jpg",width=6.5,height=6.5,units="in",res=600)
plot(currentPM,legend=FALSE)
dev.off()
jpeg("Papilio_rutulus_current.jpg",width=6.5,height=6.5,units="in",res=600)
plot(currentPR,legend=FALSE)
dev.off()
jpeg("Papilio_zelicaon_current.jpg",width=6.5,height=6.5,units="in",res=600)
plot(currentPZ,legend=FALSE)
dev.off()