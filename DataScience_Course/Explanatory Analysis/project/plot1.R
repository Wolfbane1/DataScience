
#Reading the data files.
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Changing the output directory
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis/project")

##PLOT 1:
# Has PM2.5 decreased along time?

#Calculating total Emissions
p <- tapply(NEI$Emissions, NEI$year, sum)/1000000

#Printing
par(mfrow=c(1,1), mar=c(4,4,2,1))
png(filename = "plot1.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(names(p), p, type = "l", xaxt="n", xlab = "Year", ylab="Million Tons", xlim=c(1998, 2009))
points(names(p), p, pch=20)
title("Progression of PM2.5 in time", cex.main = 0.8)
axis(1, at=names(p), labels=names(p))
dev.off()
rm(p)
