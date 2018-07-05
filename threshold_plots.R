setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data")
dat <- read.csv("Data_set_final.csv", header=T, fill=T,  na.string=" ")
library(lattice)
plot(activity ~ temp, col=Genus, data=dat)
plot(activity ~ temp, col=SN, data=dat)
plot(activity ~ temp, col=Family, data=dat)

library(nlme)
library (mgcv)
model <- glm(activity ~ temp, data= dat)
summary (model)
model
par (mfrow=c(1,2))
plot.gam (model)

model <- loess (activity ~ temp, data= dat)
summary (model)

xv<- seq (4, 45, 1)
yv<- perdict (model, data.frame (temp=xv))
lines (xv, yv)

library (tree)
thresh <- tree (activity ~ temp, data= dat)
print (thresh)
