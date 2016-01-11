library(sqldf)

#Write a function called rankhospital that takes three arguments: the 2-character abbreviated 
#name of a state (state), an outcome (outcome), and the ranking of a hospital in that state for 
#that outcome (num). 
#
#The function reads the outcome-of-care-measures.csv file and returns a 
#character vector with the name of the hospital that has the ranking specified by the num 
#argument. 
#
#For example, the call
#    rankhospital("MD", "heart failure", 5)
#
#would return a character vector containing the name of the hospital with the 5th lowest 30-day 
#death rate for heart failure. 
#
#The num argument can take values “best”, “worst”, or an integer 
#indicating the ranking (smaller numbers are better). If the number given by num is larger than 
#the number of hospitals in that state, then the function should return NA. 
#
#Hospitals that do not have data on a particular outcome should be excluded from the set of 
#hospitals when deciding the rankings.
#
rankhospital <- function(state, outcome, rank) {
  #read the data
  d <- read.csv(file.path(getwd(), "data", "outcome-of-care-measures.csv"), colClasses = "character")
  
  #returnable variable
  r <- NA
  
  #Columns definition.
  c <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
         'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', 
         'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')
  col <- matrix(c, nrow=1, ncol=3)
  colnames(col) <- c('heart attack', 'pneumonia', 'heart failure')
  rm(c)
  
  #if validation is ok, continue
  m <- validateParameters(state, outcome, d[d$State == state,'State'][1], col)
  
  if ( m == "OK" ) {
    suppressWarnings(d[d$State==state, col[,outcome]] <- as.numeric(d[d$State==state, col[,outcome]]))
    
    #Validation if rank is OK
    rv <- validateRank( d[d$State==state & ! is.na(d[,col[,outcome]]), 'State'], rank)

    if ( !is.na(rv) ) {
      #Ok, everything is ok, so I can make the things.
      #Convert to force the NAs
      y <- as.data.frame(cbind(d[d$State==state, col[,outcome]], d[d$State== state,"Hospital.Name"]))
      colnames(y) <- c("rate", "name")
      
      #take away NAs.
      y <- y[! is.na(y$rate), ]
      y[,'rate'] <- as.numeric.factor(y[,'rate'])
      x <- sqldf("select rate, name from y order by rate, name")
      r = as.character(x[rv, 'name'])
    }
  }else { #if validations doesn't was ok then stop.
    stop(m)
  }
  
  return (r)
}


validateRank <- function(o, rank) {
  v <- NA
  
  num_hospital <- as.numeric(tapply(o, o, length))

  #Checking is 
  if ( is.character(rank) ) {
    if ( rank == "best" ) {
      v <- 1
    }else if ( rank == "worst" ) {
      v <- num_hospital
    }
  }else {
    r <- as.numeric(rank)
    
    if ( r <= length(o) ) {
      v <- r
    }
  }
  
  return ( v )
}

validateParameters <- function(state, outcome, existe, col) {
  message <- "OK"
  
  #Checking the state
  if ( is.numeric(state) | nchar(state) != 2 | is.na(existe) ) {
    message <- "invalid state"
  }
  
  #Checking the outcome 
  if (!outcome %in% colnames(col) ) {
    message <- "invalid outcome"
  }
  
  return (message)
}

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

