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

#set workind directory to generate png file
setwd("/Users/zzddfge/Documents/github/DataScience/DataScience_Course/Explanatory Analysis")

png(filename = "plot3.png", 
    width = 480, height = 480, 
    units = "px", bg = "transparent")
plot(d$DateTime, d$Sub_metering_1, type="l", ylab="Energy sub metering", xlab="")
lines(d$DateTime, d$Sub_metering_2, col="red")
lines(d$DateTime, d$Sub_metering_3, col="blue")
legend("topright",
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"),
       lwd=1
       )
dev.off()

