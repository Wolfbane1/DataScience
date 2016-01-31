
#Reading the data files.
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Changing the output directory
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis/project")

##PLOT 6:
# Compare emissions from vehicle sources in Baltimore City and Los Angeles?

#Calculating total Emissions in Baltimore from all Motor Vehicle
dat <- subset(NEI, (NEI$fips=="24510" | NEI$fips=="06037") & NEI$type=="ON-ROAD", 
                          c("Emissions", "fips", "year"))
p <- aggregate(Emissions ~ year + fips, data=dat, FUN = sum)
pB <- subset(p, p$fips=="24510")
pLA<- subset(p, p$fips=="06037")
ylim <- c(floor(min(p$Emissions)-0.1*min(p$Emissions)), floor(max(p$Emissions)+0.1*max(p$Emissions)))

#Printing
par(mfrow=c(1,1), mar=c(4,4,2,1))
png(filename = "plot6.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(pB$year, pB$Emissions, type="l", ylim = ylim, xlim=c(1998, 2009),
     xaxt="n", xlab = "Year", ylab="Tons", col="red")
points(pB$year, pB$Emissions, pch=20, col="red")
legend("center", title="Countys", ncol=2, col=c("red", "blue"), pch=20, legend=c("Baltimore", "Los Angeles"))
title("Comparation of Emissions in Baltimore and LA from all Motor Vehicle", cex.main = 0.8)
axis(1, at=unique(p$year), labels=unique(p$year))
par(new = TRUE)
plot(pLA$year, pLA$Emissions, type="l", ylim = ylim, xlim=c(1998, 2009),
     xaxt="n", yaxt="n", xlab="", ylab="", col="blue")
points(pLA$year, pLA$Emissions, pch=19, col="blue")
dev.off()

rm(p)
rm(dat)
rm(pB)
rm(pLA)
rm(ylim)


