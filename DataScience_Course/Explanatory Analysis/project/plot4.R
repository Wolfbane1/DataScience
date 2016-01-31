
#Reading the data files.
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Changing the output directory
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis/project")

##PLOT 4:
# How have emissions from coal combustion-related sources changed along time across USA?

#Calculating total Emissions across USA from all Fuel Comb Coal
is.combustion.coal <- grepl("Fuel Comb.*Coal", SCC$EI.Sector)
scc_coal <- SCC[is.combustion.coal, "SCC"]
coal.comb <- subset(NEI, NEI$SCC %in% scc_coal, c("Emissions", "SCC", "year"))
p <- tapply(NEI$Emissions, NEI$year, sum)/1000000

#Printing
par(mfrow=c(1,1), mar=c(4,4,2,1))
png(filename = "plot4.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(names(p), p, type = "l", xaxt="n", xlab = "Year", ylab="Million Tons", xlim=c(1998, 2009))
points(names(p), p, pch=20)
title("Progression across USA of coal combustion-related sources", cex.main = 0.8)
axis(1, at=names(p), labels=names(p))
dev.off()

rm(p)
rm(scc_coal)
rm(coal.comb)
rm(is.combustion.coal)

