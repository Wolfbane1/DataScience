
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")

df1999 <- read.csv("annual_all_1999.csv", sep=",")
head(df1999)
table(df1999$Parameter.Name)

#Question 1: ¿Parameter Code for Parameter PM2.5 - Local Condition?

s <- subset(df1999, df1999$Parameter.Name == "PM2.5 - Local Conditions")
head(s$Parameter.Code)

#Answer: 88101

#############
####PROJECT
#############

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

##PLOT 2:
# Has PM2.5 decrease along time in Baltimore

#Calculating total emissions in Baltimore
Baltimore <- subset(NEI, NEI$fips == "24510", c("Emissions", "year"))
p <- tapply(Baltimore$Emissions, Baltimore$year, sum)/1000

#Printing
par(mfrow=c(1,1), mar=c(4,4,2,1))
png(filename = "plot2.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(names(p), p, type = "l", xaxt="n", xlab = "Year", ylab="Thousands Tons", xlim=c(1998, 2009))
points(names(p), p, pch=20)
title("Progression of PM2.5 in time within Baltimore", cex.main = 0.8)
axis(1, at=names(p), labels=names(p))
dev.off()

rm(p)
rm(Baltimore)

##PLOT 3:
# Which of the type sources has increased or decreased in Baltimore?

#Calculating total emissions in Baltimore
Baltimore <- subset(NEI, NEI$fips == "24510", c("Emissions", "type", "year"))
p <- aggregate(Emissions ~ year + type, data=Baltimore, FUN = sum)
p$Emissions <- round(p$Emissions)/1000
p$year <- factor(p$year)
p$type <- factor(p$type)

#Printing
png(filename = "plot3.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
g <- ggplot(p, aes(y=Emissions, x=year, group=type, col=type)) + geom_line() + 
  theme(axis.text.x = element_text(size = 8)) + 
  theme(title = element_text(size = 8, colour = "black")) + 
  labs(x="Year", y="Thousands Tons",
       title="Progression of type sources along time in Baltimore")
print(g)
dev.off()

rm(Baltimore)
rm(p)
rm(g)

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


