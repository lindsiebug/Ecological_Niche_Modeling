library(car)
View (mtcars)

setwd("C:/Users/lma24/Dropbox/SFP Pollinators/All_year_data")
AllDat = read.csv ("All_year_peaks_data_combind.csv")
scatterplot(Canopy.Cover ~ Abundance | ID, data=AllDat, 
            xlab="Abundance", ylab="Canopy Cover", 
            labels=row.names(AllDat))

scatterplot(P.Abundance ~ Elevation | ID, data=AllDat, 
            xlab="Abundance", ylab="Canopy Cover", 
            labels=row.names(AllDat))

scatterplot(P.Richness ~ Elevation | ID, data=AllDat, 
            xlab="Abundance", ylab="Canopy Cover", 
            labels=row.names(AllDat))
