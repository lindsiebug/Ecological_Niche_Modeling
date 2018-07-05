library(dismo)

setwd("C:/Users/lma24/Dropbox/Biodiversity_Arthropods_NA/Biodiversity_NA_2017/Data_Downloads_2017/Lindsie_GBIF_Download_Scripts_&_Data")
codeName= read.csv("SCAN_Collections_January_2017.csv")

#########Hemiptera#################

HYM_US<-gbif("Hemiptera", args=c('country=US')) 
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Hemiptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Hemiptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")

#########Lepidoptera#################

HYM_US<-gbif("Lepidoptera", args=c('country=US')) 
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Lepidoptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Lepidoptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")


#########Diptera#################

HYM_US<-gbif("Diptera", args=c('country=US')) 
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Diptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Diptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")


#########Coleoptera#################

HYM_US<-gbif("Coleoptera", args=c('country=US')) 
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Coleoptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Coleoptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")


#########Hymenoptera#################

HYM_US<-gbif("Hymenoptera", args=c('country=US')) 
for (i in 1:67) {
HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Hymenoptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Hymenoptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")


#########Orthoptera#################

HYM_US<-gbif("Orthoptera", args=c('country=US')) 
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_United_State.csv")

HYM_CA <-gbif("Orthoptera", args=c('country=CA'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Canada.csv")

HYM_ME <- gbif("Orthoptera", args=c('country=MX'))
for (i in 1:67) {
  HYM_US2 <- HYM_US[HYM_US$institutionCode != "paste(codeName[i,2])", ]}
write.csv(HYM_US, "Hymenoptera_Mexico.csv")









