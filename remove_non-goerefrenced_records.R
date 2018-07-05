setwd ("C:/Users/lma243.NAU/Documents/webreq_DwC-A (1)") #set pathway to where you stored your dawrin core csv
records = read.csv ("occurrences.csv") 
head (records)

df = records[!is.na (records$decimalLatitude),] #removes records that don't have latitude
df = records[!is.na (records$decimalLongitude),] #remove records that don't have longitude

write.csv (df, "C:/Users/lma243.NAU/Documents/webreq_DwC-A (1)/georefrenced_records.csv") #use this if you want a file to refrence later


