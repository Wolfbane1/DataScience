#Part 1
#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. 
#
#The function 'pollutantmean' takes three arguments: 
#   'directory', 
#   'pollutant',
#   'id'. 
#
#Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate matter data 
#from the directory specified in the 'directory' argument and returns the mean of the pollutant 
#across all of the monitors, ignoring any missing values coded as NA. 
#A prototype of the function is as follows
pollutantmean <- function(directory, pollutant, id = 1:332) {
  f <- as.data.frame(c())
  for( i in id ) {
    fichero <- paste("00", as.character(i), ".csv", sep="")
    fichero <- substring(fichero, nchar(fichero) - 6, nchar(fichero))
    fichero <- file.path(directory, fichero)
    a <- read.csv(fichero, header=TRUE) 
    f <- rbind(f, a)
  }
  
  return( round(mean(f[,pollutant], na.rm=TRUE), digits=3) )
}
