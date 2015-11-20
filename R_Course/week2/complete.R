

#Part 2
#Write a function that reads a directory full of files and reports the number of completely 
#observed cases in each data file. The function should return a data frame where the first column 
#is the name of the file and the second column is the number of complete cases. 
#A prototype of this function follows
## Return a data frame of the form:
## id nobs
## 1  117
## 2  1041
## ...
## where 'id' is the monitor ID number and 'nobs' is the
## number of complete cases
complete <- function(directory, id = 1:332) {
  group <- rbind(c(), c(NA,NA))
  rownames(group)[1] <- c(0)
  colnames(group) <- c("id", "nobs")
  j <- 2
  
  #read the files
  for( i in id ) {
    fichero <- paste("00", as.character(i), ".csv", sep="")
    fichero <- substring(fichero, nchar(fichero) - 6, nchar(fichero))
    fichero <- file.path(directory, fichero)
    a <- read.csv(fichero, header=TRUE) 
    group <- rbind(group, c(i, nrow(a[is.na(a["sulfate"])==FALSE & is.na(a["nitrate"])==FALSE,])))
    rownames(group)[j] <- j - 1
    j <- j + 1
  }
  group <- subset(group, rownames(group) > 0) 
  return( as.data.frame(group) )
}

