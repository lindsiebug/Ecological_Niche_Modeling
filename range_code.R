setwd('S:/AAA_Pollinators/AAA_Lindsie_Abbott/Data/Trait Data')
commDat=as.data.frame(read.csv('Bee_2013.csv',head=T,row.names=1))
traitDat=as.data.frame(read.csv('bee_trait_data.csv',head=T,row.names=1))
commDat=t(commDat)

commDat=as.data.frame(read.csv('bee_communiteis2.csv',head=T,row.names=1))
traitDat=as.data.frame(read.csv('bee_traits.csv',head=T,row.names=1))
commDat=t(commDat)

commDat=as.data.frame(read.csv('Bee_Ponderosa.csv',head=T,row.names=1))
traitDat=as.data.frame(read.csv('Trait_Bee_Ponderosa.csv',head=T,row.names=1))
commDat=t(commDat)

hist(traitDat$Volume)
hist(log10(traitDat$Volume))
hist(traitDat$Darkness)
hist(log10(traitDat$Darkness))

plot(traitDat$Volume,traitDat$Darkness)
cor(traitDat$Volume,traitDat$Darkness)
cor.test(traitDat$Volume,traitDat$Darkness)
cor.test(log10(traitDat$Volume),log10(traitDat$Darkness))


rnge=function(comm,trait){
  present=names(comm)[comm!=0]
  traitVal=trait[match(present,names(trait))]
  return(max(traitVal)-min(traitVal))
}
Darkness=traitDat$Darkness
names(Darkness)=rownames(traitDat)
DarknessRnge=apply(commDat,1,rnge,Darkness)
hist(DarknessRnge)


SR=function(x){#creating a function to calculate species richness
  return(length(x[x!=0]))
}
obsSR=apply(commDat,1,SR)
sampleSR=as.numeric(levels(as.factor(obsSR)))#these are the levels of species richness for which we have to generate random communities

rndSample=list()#this is the first time we've encountered a "list"; this is a way of storing multiple matrices within the same (list) object
for (i in sampleSR){#going to create random communities for each level of observed species richness
  rndSample[[i]]=matrix(rep(0,ncol(commDat)*999),nrow=999,ncol=ncol(commDat))#creating a matrix of zeros that we will fill in row by row as we randomly sample
  colnames(rndSample[[i]])=colnames(commDat)
  for (j in 1:9999){#doing this 99 times for each levels of species richness
    sample.ij=sample(colnames(commDat),i)#randomly samply i species
    rndSample[[i]][j,match(sample.ij,colnames(rndSample[[i]]))]=1#changing the values of the randomly sampled species to 1 in row j
  }
}

rndDarknessRnge=vector()
for (i in sampleSR){
  rndDarknessRnge=cbind(rndDarknessRnge,apply(rndSample[[i]],1,rnge,Darkness))
}
colnames(rndDarknessRnge)=sampleSR
head(rndDarknessRnge)

DarknessSES=vector()
for (i in 1:length(DarknessRnge)){
  rndDat=rndDarknessRnge[,colnames(rndDarknessRnge)==obsSR[i]]
  ses=(DarknessRnge[i]-mean(rndDat,na.rm=T))/sd(rndDat,na.rm=T)#calculating the standardized effect size
  pVal=2*pnorm(-abs(ses))#calculating the P-value
  DarknessSES=rbind(DarknessSES,c(ses,pVal))
}
colnames(DarknessSES)=c("SES","P")
rownames(DarknessSES)=rownames(commDat)
DarknessSES=as.data.frame(DarknessSES)
hist(DarknessSES$SES)


write.csv (DarknessSES, file="C:/Users/Lindsie/OneDrive/Documents/Manuscripts/Trait_data_Paper_data/Trait Data/Final Data sets/Darkness_all_bees1.csv")

FlyVol<-read.csv ("Darkness_fly.csv")

png(file="Fly_Darkness.png",width=400, height=350)
bar(dv = SES, 
    factors = c(Habitat, LifeZone), 
    dataframe = FlyVol, 
    errbar = TRUE, 
    ylim=c(-3, 3), col=c('Blue','darkorange'), main='Fly Darkness')
abline(h = -2, col = "red", lty = 1)
abline(h = 2, col = "red", lty = 1)
text(c(1.5,2.5,4.5,5.5,7.5,8.5),c(1.1,0.1,1.5,1.8,0.2,1.6),labels=c("a","b","a","c","c","c"))
dev.off()

ANOVA <- aov(SES~LifeZone*Habitat, data=FlyVol)
summary (ANOVA)
TukeyHSD(ANOVA)

read.png ("Fly_Volume.png")