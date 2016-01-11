##
#####2 Finding the best hospital in a state
##
#Write a function called best that take two arguments: 
#  a) the 2-character abbreviated name of a state
#  b) an outcome name. 
#
#The function reads the outcome-of-care-measures.csv file and returns a character vector 
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the 
#specified outcome in that state. 
#
#The hospital name is the name provided in the Hospital.Name variable. 
#The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. 
#Hospitals that do not have data on a particular outcome should be excluded from the set of 
#hospitals when deciding the rankings.
#
#Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital 
#names should be sorted in alphabetical order and the first hospital in that set should be 
#chosen (i.e. if hospitals “b”, “c”, and “f” are tied for best, then hospital “b” should be 
#returned).
#
#The function should use the following template.
#best <- function(state, outcome) {
#  ## Read outcome data
#}
#
#Check that state and outcome are valid. Return hospital name in that state with lowest 30-day death rate
#
#The function should check the validity of its arguments. 
#  If an invalid state value is passed to best, the function should throw an error via the stop 
#    function with the exact message “invalid state”. 
#  If an invalid outcome value is passed to best, the function should throw an error via the 
#    stop function with the exact message “invalid outcome”.
#Here is some sample output from the function.
#> source("best.R")
#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
#> best("BB", "heart attack")
#Error in best("BB", "heart attack") : invalid state
#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
#>


best <- function(state, outcome) {
  ## Read outcome data
  d <- read.csv(file.path(getwd(), "data", "outcome-of-care-measures.csv"), colClasses = "character")
  
  #Checking the state
  if ( is.numeric(state) | nchar(state) != 2 | is.na(d[d$State == state,'State'][1])) {
    stop("invalid state")
  }
  c <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
           'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', 
           'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')
  col <- matrix(c, nrow=1, ncol=3)
  colnames(col) <- c('heart attack', 'pneumonia', 'heart failure')
  rm(c)
  
  #Checking the outcome 
  if (!outcome %in% colnames(col) ) {
    stop("invalid outcome") 
  }
  
  #Ok, everything is ok, so I can make the things.
  #Convert to force the NAs
  suppressWarnings(d[d$State==state, col[,outcome]] <- as.numeric(d[d$State==state, col[,outcome]]))
  y <- as.data.frame(cbind(d[d$State== state,"Hospital.Name"],
                           d[d$State== state, col[,outcome]]))
  colnames(y) <- c("Hospital.Name", "rate")
  
  #take away NAs.
  y <- y[! is.na(y$rate), ]
  min <- min(as.numeric(levels(y$rate)))
  f <- sort(as.character(y[y$rate == min, 1]))
  return ( f[1] )
}

