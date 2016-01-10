
####################
## READING THE DATA
####################
#read the data from the project.
setwd("/Users/zzddfge/Desktop/Compartida/Data Science/Exploratory Analysis")
d <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", rep("numeric",7)),
              na = "?")

#filtering to have only the Date specified.
d <- subset(d, Date == "1/2/2007" | Date == "2/2/2007")

#set workind directory to generate png file
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis")

#plot 1 - Global Active Power
png(filename = "plot1.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
hist(d$Global_active_power, 
     col = "red", 
     main = "Global Active Power", 
     xlab = "Global Active Power (kilowatts)",
     breaks = 12, ylim = c(0, 1200))
dev.off()
