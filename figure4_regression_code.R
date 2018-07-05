setwd("C:/Users/Lindsie/OneDrive/Documents")
CC<-read.csv("Canopy_Cover_Regression.csv")
head(CC)
reg1 <- lm(X.AD~X.CC, data=CC)
attach(CC)
plot(X.CC, X.AD, main="Abundance", 
     xlab="% Canopy Cover ", ylab="% Abundance (Bee-Fly) ", pch=19)
abline(reg1, col="blue")


reg2 <- lm(X.SRD~X.CC, data=CC)
attach(CC)
plot(X.CC, X.AD, main="Species Richness", 
     xlab="% Canopy Cover ", ylab="% Species Richness (Bee-Fly) ", pch=19)
abline(reg2, col="blue")

