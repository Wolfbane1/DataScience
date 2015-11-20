
#Part 3
#Write a function that takes a directory of data files and a threshold for complete cases and 
#calculates the correlation between sulfate and nitrate for monitor locations where the number 
#of completely observed cases (on all variables) is greater than the threshold. 
#The function should return a vector of correlations for the monitors that meet the threshold 
#requirement. 
#If no monitors meet the threshold requirement, then the function should return a numeric vector 
#of length 0. A prototype of this function follows

corr <- function(directory, threshold = 0) {
  f <- numeric(0)
  ficheros <- dir(directory)
  
  for( fichero in ficheros) {
    #read file and concatenate to all where all obs are complete.
    fic <- file.path(directory, fichero)
    a <- na.omit(read.csv(fic, header=TRUE))
    
    if ( nrow(a) > threshold ) {
      f <- c(f, round(cor(a[["sulfate"]], a[["nitrate"]]), digits=5))
      #f <- c(f, cor(a[["sulfate"]], a[["nitrate"]]))
    } 
  }
  
  return(f)
}


corr2 <- function(directory, threshold = 0) {
  # --- Assert 'directory' is a character vector of length 1 indicating the
  # location of the CSV files.  'threshold' is a numeric vector of length 1
  # indicating the number of completely observed observations (on all
  # variables) required to compute the correlation between nitrate and
  # sulfate; the default is 0.  Return a numeric vector of correlations.
  
  # --- Assert create an empty numeric vector
  corrsNum <- numeric(0)
  
  # --- Assert get a data frame as ID = 1:332
  nobsDfr <- complete("specdata")
  
  # --- Assert apply threshold
  nobsDfr <- nobsDfr[nobsDfr$nobs > threshold, ]
  
  for (cid in nobsDfr$id) {
    # --- Assert get a data frame as ID in $id
    monDfr <- getmonitor(cid, directory)
    
    # --- Assert calculate correlation between $sulfate and $nitrate
    corrsNum <- c(corrsNum, cor(monDfr$sulfate, monDfr$nitrate, use = "pairwise.complete.obs"))
  }
  
  # --- Assert return value is a numeric vector of correlations
  return(corrsNum)
}

complete <- function(directory, id = 1:332) {
  # --- Assert 'directory' is a character vector of length 1 indicating the
  # location of the CSV files.  'id' is an integer vector indicating the
  # monitor ID numbers to be used Return a data frame of the form: id nobs 1
  # 117 2 1041 ...  where 'id' is the monitor ID number and 'nobs' is the
  # number of complete cases
  
  # --- Assert create an empty vector
  nobsNum <- numeric(0)
  
  for (cid in id) {
    # --- Assert get data frame as ID
    cDfr <- getmonitor(cid, directory)
    
    # --- Assert count the number of complete cases and append to numeric
    # vector
    nobsNum <- c(nobsNum, nrow(na.omit(cDfr)))
  }
  
  # --- Assert return value is a data frame with TWO (2) columns
  data.frame(id = id, nobs = nobsNum)
}

getmonitor <- function(id, directory, summarize = FALSE) {
  # --- Assert 'id' is a vector of length 1 indicating the monitor ID
  # number. The user can specify 'id' as either an integer, a character, or
  # a numeric.  'directory' is a character vector of length 1 indicating the
  # location of the CSV files 'summarize' is a logical indicating whether a
  # summary of the data should be printed to the console; the default is
  # FALSE
  
  # --- Assert construct file name Directory is pre-appended to file name.
  # Use sprintf() to add leading zeroes.  E.g. 'specdata/001.csv'
  fileStr <- paste(directory, "/", sprintf("%03d", as.numeric(id)), ".csv", 
                   sep = "")
  
  # --- Assert read csv
  rawDfr <- read.csv(fileStr)
  
  # --- Assert summary if true
  if (summarize) {
    print(summary(rawDfr))
  }
  
  # --- Return value is a data frame
  return(rawDfr)
}