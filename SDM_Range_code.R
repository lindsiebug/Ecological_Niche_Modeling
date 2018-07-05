######Range Code#######
library(raster)
library(fields)
setwd('W:/2015_Fall/Closed/Week13_Nov24/example_model') #set your working directory
respXY=read.csv('respXY.csv',row.names=1) #this needs to be the respXY csv for the speices you are working with 
projCurrent=raster('proj_current_model.output.grd')#change this to the grd file of the species you want to do the ranges with 
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
