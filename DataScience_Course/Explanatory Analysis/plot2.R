
####################
## READING THE DATA
####################
#read the data from the project.
setwd("/Users/zzddfge/Desktop/Compartida/Data Science/Exploratory Analysis")
d <- read.csv("household_power_consumption.txt", sep=";", colClasses = c("character", "character", rep("numeric",7)),
              na = "?")

#filtering to have only the Date specified.
d <- subset(d, Date == "1/2/2007" | Date == "2/2/2007")

#Creating a new column
x <- paste(d$Date, d$Time)
DateTime <- strptime(x, "%d/%m/%Y %H:%M:%S")
d <- cbind(d, DateTime)
rm(x) 
rm(DateTime)

####################
## PRINTING PLOTs
####################

#set workind directory to generate png file
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis")

#plot 2 - Global Active Power
png(filename = "plot2.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(d$DateTime, d$Global_active_power, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.off()

rm(d)

