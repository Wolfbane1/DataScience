
#Reading the data files.
setwd("/Users/zzddfge/Desktop/Compartida/DataScience/Exploration Analysis/data")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Changing the output directory
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis/project")

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

