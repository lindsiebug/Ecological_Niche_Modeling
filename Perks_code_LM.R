library(raster)

#########Old Code###########
swwp=read.csv('X:/2015_Fall/Open/Sean Perks/SWWP Collections/All SWWP Locations.csv')
respXY=swwp[,2:1]
MAT=raster('//cpbc/BIOCLIM/BIOCLIM_1950_2000/bio_1.bil')
MTWM=raster('//cpbc/BIOCLIM/BIOCLIM_1950_2000/bio_5.bil')
MAP=raster('//cpbc/BIOCLIM/BIOCLIM_1950_2000/bio_12.bil')
PDQ=raster('//cpbc/BIOCLIM/BIOCLIM_1950_2000/bio_17.bil')
envStackf=stack(MAT,MTWM,MAP,PDQ)
envStackf=crop(envStackf,extent(-125,-65,7.21,50))

envBrick.i=brick(envStackf)
b <- writeStart(envBrick.i, filename="X:/2015_Fall/Open/Sean Perks/bricks/envbrick.grd", format="raster",overwrite=TRUE)
tr <- blockSize(b)
for (i in 1:tr$n) {
  v <- getValuesBlock(envBrick.i, row=tr$row[i], nrows=tr$nrows[i])
  b <- writeValues(b, v, tr$row[i])
b <- writeStop(b)


##########Revised Code---Use This#####################


library(rgdal)

### reading in climate data
setwd("W:/BIOMOD/Climate/BIOCLIM")

envStack=raster("bio_1.bil")
for (i in c(5,12,17)){
  envStack=stack(envStack,raster(paste("bio",paste(i,"bil",sep="."),sep="_")))
}
names(envStack)=c("MAT","MTWM","MAP","PDQ")
envStack=crop(envStack,extent(-125,-65,7,50))

setwd("W:/RCodes/test")
writeRaster(envStack, filename="envBrick.grd")




######Below is for when I'm done with above####



library(biomod2)
library(rgdal)
library(rgeos)
library(maptools)
library(SDMTools)
library(rgbif)
respXY=read.csv('//cefnsshares/Homes/NAU-STUDENTS/sp392/Desktop/SWWP_GIS/All SWWP Locations.csv')
envStackf=brick('//cefnsshares/Homes/NAU-STUDENTS/sp392/Desktop/SWWP_GIS/envbrick.grd')
envUnstack=unstack(envStackf)
envStackf=stack(envUnstack)
rm(envUnstack)
pres_filtered=rasterize(respXY[,2:1],envStackf,field=respXY[,3],fun=function(x,...)max(x))

respXY=cbind(xyFromCell(pres_filtered,cell=which(pres_filtered[]==1)),c(rep(1)))#recreating the presence coordinates
climDat=extract(envStack,respXY[,1:2])#extracting climate data from filtered presences
respXY=respXY[complete.cases(respXY),]#getting rid of any NAs in presences
climDat=climDat[complete.cases(climDat),]#ditto for climate data

exts=c("SD_1.0","SD_1.5","SD_2.0","SD_2.5","SD_3.0","Convex_polygon","Full")#Different background extent algorithms we might want to use
k=6
extMethod=exts[k]

if ((k<6)=="TRUE"){
  
  ########### SD approach
  SD=as.numeric(strsplit(extMethod,"_")[[1]][2])#getting the numeral for the SD
  
  climLim=vector()
  for (i in 1:ncol(climDat)){#for each climate varialbe
    lim.i=vector()
    for (j in 1:999){#and for 999 random samples
      dist.i=rnorm(3*nrow(climDat),mean=mean(climDat[,i]),sd=sd(climDat[,i])*SD)#generate a random normal distribution with the same mean value as the presences for climate variable i, but with 3x the number of points and with a wider standard deviation (in this case, 1.5SD)
      lim.i=cbind(lim.i,c(min(dist.i),max(dist.i)))#calculate the range for the null distribution
    }
    climLim=cbind(climLim,rowMeans(lim.i))#take the average of the 999 random samples
  }
  colnames(climLim)=colnames(climDat)
  rownames(climLim)=c("min","max")#Now we have a dataframe with the limits for our background in each climate dimension
  
  is.between=function(x,a,b){#function to identify points that lie within the univariate range for each climate dimension
    (x-a)*(b-x)>0
  }
  
  climLimRast=is.between(envStack[[1]],climLim[1,1],climLim[2,1])#applying the is.between function to the first climate dimension to convert any pixels outside of the specified background range for dimension one to zeros, and any inside that range to ones
  for (i in 2:ncol(climLim)){
    climLimRast=climLimRast*is.between(envStack[[i]],climLim[1,i],climLim[2,i])#now apply this function for each climate dimension, successively removing pixels that are outside the specific bounds
  }
  
  #setting background extent
  envStack.i=mask(envStack,climLimRast,maskvalue=0)#masking the environmental stack to our new climate-defined background
  envUnstack.i=unstack(envStack.i)
  envStack.i=stack(envUnstack.i)
  
}else{
  if (k==6){
    ########### minimum convex polygon approach
    presShp=as.data.frame(respXY)#creating a point shapefile of presence records
    coordinates(presShp)=c('x','y')
    proj4string(presShp)=proj4string(envStack)
    MCP=gConvexHull(presShp)#calculating the minimum convex polygon (convex hull) in 2 dimensions (lon/lat) that spans all presence points
    MCPRaster=rasterize(MCP,envStack)#rasterizing to the extent of the envStack
    MCPRaster=crop(MCPRaster,MCP)#cropping to extent of mask
    envStack.i=crop(envStack,MCP)#cropping the climate layers to the extent of the mask
    envStack.i=mask(envStack.i,MCPRaster,maskvalue=NA)#masking to the MCP
    envUnstack.i=unstack(envStack.i)
    envStack.i=stack(envUnstack.i)
    rm(MCPRaster)
  }else{
    ########## full extent (whatever your original envStack was cropped/masked to)
    envStack.i=envStack
  }
}