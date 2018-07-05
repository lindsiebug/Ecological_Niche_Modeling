library(meta) 
library (metafor)

####ALL####
setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data/raw_data")
dat <- read.csv("Data_set_final_low-high.csv")

### calculate r-to-z transformed correlations and corresponding sampling variances
#dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat, slab=paste(authors, year, sep=", "))
dat <- escalc(measure="ZCOR", ri=r, ni=n, data=dat, slbab=paste(Genuis, species, sep=" "))
write.csv (dat, file = "Effect_size_data_set.csv")
### meta-analysis of the transformed correlations using a random-effects model
res <- rma(yi, vi, data=dat)
res
### average correlation with 95% CI
predict(res, digits=3, transf=transf.ztor)
### forest plot
forest(res, addcred=TRUE, atransf=transf.ztor)
#text(-1.6, 18, "Author(sat=transf.rtoz(c(-.4,-.2,0,.), Year", pos=4, cex=.8)
#text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)
forest(res)
### funnel plot
funnel(res)








####APIDAE#####
setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data/raw_data")
dat_api <- read.csv("Data_set_final_apidae.csv")

### calculate r-to-z transformed correlations and corresponding sampling variances
#dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat, slab=paste(authors, year, sep=", "))
dat_api <- escalc(measure="ZCOR", ri=r, ni=n, data=dat_api, slbab=paste0(Genuis, species, sep=" "))
dat_api
### meta-analysis of the transformed correlations using a random-effects model
res_api <- rma(yi, vi, data=dat_api)
res_api
write.csv (dat_api, file = "Apidae_effect_size.csv")
### average correlation with 95% CI
predict(res_api, digits=3, transf=transf.ztor)
### forest plot
forest(res_api, atransf=transf.ztor)
text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)



####MEGACHILE#####
setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data/raw_data")
dat <- read.csv("Data_set_final_megachilidae.csv")
head(dat)

### calculate r-to-z transformed correlations and corresponding sampling variances
#dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat, slab=paste(authors, year, sep=", "))
dat <- escalc(measure="ZCOR", ri=r, ni=n, data=dat, slbab=paste0(Genuis, species, sep=" "))
dat
### meta-analysis of the transformed correlations using a random-effects model
res <- rma(yi, vi, data=dat)
res
write.csv (dat, file = "Megachilidae_effect_size.csv")
### average correlation with 95% CI
predict(res, digits=3, transf=transf.ztor)
### forest plot
forest(res, addcred=TRUE, atransf=transf.ztor, digits=c(2,1))
#text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
#text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)




####OTHER#####
setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data/raw_data")
dat_meg <- read.csv("Data_set_final_others.csv")
head(dat_meg)
### calculate r-to-z transformed correlations and corresponding sampling variances
#dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat, slab=paste(authors, year, sep=", "))
dat_meg <- escalc(measure="ZCOR", ri=r, ni=n, data=dat_meg, slbab=paste0(Genuis, species, sep=" "))
dat_meg
### meta-analysis of the transformed correlations using a random-effects model
res_meg <- rma(yi, vi, data=dat_meg)
res_meg
write.csv (dat_meg, file = "Other_effect_size.csv")
### average correlation with 95% CI
predict(res_meg, digits=3, transf=transf.ztor)
### forest plot
forest(res_meg, addcred=TRUE, atransf=transf.ztor, digits=c(2,1))
#text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
#text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)


forest (res)



setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data/raw_data")
box <- read.csv ("box_plot_data.csv")
head(box)
boxplot(cc~Family,data=box, 
        xlab="Family", ylab="Correlation coefficient", col="gray")
text (c(1,2,3), 1.03, c("A","B","B"))
text (c(1,2,3), -.97, c("n=65","n=9","n=15"))

ANOVA <- aov(cc~Family, data=box)
summary (ANOVA)
TukeyHSD(ANOVA)


boxplot(cc~body,data=box, 
        xlab="Family", ylab="Correlation coefficient", col="gray")
text (c(1,2,3), 1.03, c("A","A","A"))
text (c(1,2,3), -.97, c("n=44","n=28","n=17"))

ANOVA <- aov(cc~body, data=box)
summary (ANOVA)
TukeyHSD(ANOVA)



boxplot(cc~method,data=box, ylab="Correlation coefficient", col="gray")
text (c(1,2), 1.03, c("A","A"))
text (c(1,2), -.97, c("n=50","n=39"))

ANOVA <- aov(cc~method, data=box)
summary (ANOVA)
TukeyHSD(ANOVA)

