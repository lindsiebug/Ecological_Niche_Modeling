
install.packages("devtools")
library(devtools)

install_github("duncantl/XMLRPC")
install_github("duncantl/RWordPress")
install.packages("knitr")

library(XMLRPC)
library(RWordPress)
library(knitr)

options(WordpressLogin=c(Lindsie_McCabe="Ladybug90"),
        WordpressURL="http://cpbc.bio.nau.edu/pollinator-wp/pollinator/")

setwd("C:/Users/lma243.NAU/OneDrive/PhD_Documents/R_Code")
knit2wp('test_file.Rmd', 
        title = 'Blog Posting from R Markdown to WordPress',publish = T )


opts_knit$set(upload.fun = function(file){library(RWordPress);
  uploadFile(file)$url;})

  

#knit2wpCrayon("test_file.Rmd", 
 #             title = "Create blog posts from RStudio to Wordpress",
 #             publish = F, upload = TRUE)
