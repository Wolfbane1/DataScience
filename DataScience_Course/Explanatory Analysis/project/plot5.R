
#Reading the data files.
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Changing the output directory
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis/project")

##PLOT 5:
# How have emissions from motor vehicle sources changed along time in Baltimore City?

#Calculating total Emissions in Baltimore from all Motor Vehicle
Baltimore <- subset(NEI, NEI$fips=="24510" & NEI$type=="ON-ROAD", c("Emissions", "year"))
p <- tapply(Baltimore$Emissions, Baltimore$year, sum)

#Printing
par(mfrow=c(1,1), mar=c(4,4,2,1))
png(filename = "plot5.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(names(p), p, type = "l", xaxt="n", xlab = "Year", ylab="Tons", xlim=c(1998, 2009))
points(names(p), p, pch=20)
title("Total Emissions in Baltimore from all Motor Vehicle", cex.main = 0.8)
axis(1, at=names(p), labels=names(p))
dev.off()

rm(p)
rm(Baltimore)

