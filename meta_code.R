library(meta) 
library (metafor)
setwd ("C:/Users/Lindsie/Dropbox/SFP Pollinators/Meta_Analysis/Data")
dat <- read.csv("Data_set_final.csv")

### calculate r-to-z transformed correlations and corresponding sampling variances
#dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat, slab=paste(authors, year, sep=", "))
dat <- escalc(measure="ZCOR", ri=activity, ni=n, data=dat)
dat
### meta-analysis of the transformed correlations using a random-effects model
res <- rma(yi, vi, data=dat)
res
### average correlation with 95% CI
predict(res, digits=3, transf=transf.ztor)
### forest plot
forest(res, addcred=TRUE, xlim=c(-1.6,1.6), atransf=transf.ztor,
       at=transf.rtoz(c(-.4,-.2,0,.2,.4,.6)), digits=c(2,1), cex=.8)
text(-1.6, 18, "Author(s), Year", pos=4, cex=.8)
text( 1.6, 18, "Correlation [95% CI]", pos=2, cex=.8)
### funnel plot
funnel(res)

#The data set with column headings in ???rst row. Save as a comma separated ???le with ???le name example,
#that is, example.csv to use with the code. These data are hypothetical.