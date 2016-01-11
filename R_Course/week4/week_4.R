#R assigment WEEK 4

#The Hospital Compare web site contains a lot of data and we will only look at a small subset 
#for this assignment. The zip file for this assignment contains three files
#• outcome-of-care-measures.csv: Contains information about 30-day mortality and readmission rates for heart attacks, heart failure, and pneumonia for over 4,000 hospitals.
#• hospital-data.csv: Contains information about each hospital.
#• Hospital_Revised_Flatfiles.pdf: Descriptions of the variables in each file (i.e the code book).
#
                                                                                                                                                                              Tables 19 and 11) to have next to you while you work on this assignment. In particular, the numbers of the variables for each table indicate column indices in each table (i.e. “Hospital Name” is column 2 in the outcome-of-care-measures.csv file).

####
######Hospital Data.csv
####
#This table provides general Hospital information in response to a Hospital Compare search.
#1. Provider Number: varchar (6) Lists the hospitals by their provider identification number. 
#2. Hospital Name: varchar (50) Lists the name of the hospital.
#3. Address 1: varchar (50) Lists the first line of the street address of the hospital.
#4. Address 2: varchar (50) Lists the second line of the street address of the hospital.
#5. Address 3: varchar (50) Lists the third line of the street address of the hospital. 
#6. City: varchar (28) Lists the city in which the hospital is located.
#7. State: varchar (2) Lists the 2 letter State code in which the hospital is located. 
#8. ZIP Code: char (5) Lists the 5 digit numeric ZIP for the hospital.
#9. County Name: char (15) Lists the county in which the hospital is located.
#10. Phone Number: char (10) Lists the 10-digit numeric telephone number, including area code, 
#    for the Hospital.
#11. Hospital Type: char (25) Lists the type of hospital. The values are: 
#    a. Acute Care Hospital
#    b. Acute Care – VA Medical Center 
#    c. Critical Access Hospital
#    d. Children’s Hospital
#12. Hospital Owner: varchar (44) Lists the type of ownership the Hospital falls under. 
#    The values are:
#    a. Government – Federal
#    b. Government – Hospital District or Authority 
#    c. Government – Local
#    d. Government – State
#    e. Proprietary
#    f. Voluntary non-profit – Church
#    g. Voluntary non-profit – Other
#    h. Voluntary non-profit – Private
#    i. Not Available
#13. Emergency Services: char (3) Returns “Yes” or “No” to specify whether or not the hospital 
#    provides emergency services.

####
######Outcome of Care Measures.csv
####
#This table provides each hospital’s risk-adjusted 30-Day Death (mortality) 
#and 30-Day Readmission category and rate.
#1. Provider Number: varchar (6) Lists the hospitals by their provider identification number.
#2. Hospital Name: varchar (50) Lists the name of the hospital.
#3. Address 1: varchar (50) Lists the first line of the street address of the hospital.
#4. Address 2: varchar (50) Lists the second line of the street address of the hospital.
#5. Address 3: varchar (50) Lists the third line of the street address of the hospital.
#6. City: varchar (28) Lists the city in which the hospital is located.
#7. State: varchar (2) Lists the 2 letter State code in which the hospital is located.
#8. ZIP Code: char (5) Lists the 5 digit numeric ZIP for the hospital.
#9. County Name: char (15) Lists the county in which the hospital is located.
#10. Phone Number: char (10) Lists the 10-digit numeric telephone number, including area code, for the Hospital.
#11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the risk adjusted rate (percentage) for each hospital.
#12. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
#    varchar (50) Lists the mortality and readmission category in which the hospital falls. 
#    The values are:
#    • Better than U.S. National Average
#    • No Different than U.S. National Average • Worse than U.S. National Average
#    • Number of Cases too Small*
#13. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#14. Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
#    Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#15. Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
#    varchar (5) Lists the number of Medicare patients treated for Heart Attack by the Hospital.
#16. Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the footnote value when appropriate, as related to the Heart Attack Outcome of Care at the hospital.
#17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the risk adjusted rate 
#    (percentage) for each hospital.
#18. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are:
#    a. Better than U.S. National Average
#    b. No Different than U.S. National Average 
#    c. Worse than U.S. National Average
#    d. Number of Cases too Small*
#19. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#20. Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Heart Failure: 
#    Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#21. Number of Patients - Hospital 30-Day Death (Mortality) Rates from Heart Failure: 
#    varchar (5) Lists the number of Medicare patients treated for Heart Failure by the Hospital.
#22. Footnote - Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the footnote 
#    value when appropriate, as related to the Heart Failure Outcome of Care at the hospital.
#23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
#    Lists the risk adjusted rate (percentage) for each hospital.
#24. Comparison to U.S. Rate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
#    varchar (50) Lists the mortality and readmission category in which the hospital falls. The values are:
#    • Better than U.S. National Average
#    • No Different than U.S. National Average • Worse than U.S. National Average
#    • Number of Cases too Small*
#25. Lower Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#26. Upper Mortality Estimate - Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
#    Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#27. Number of Patients - Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
#    varchar (5) Lists the number of Medicare patients treated for Pneumonia by the Hospital.
#28. Footnote - Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the footnote value 
#    when appropriate, as related to the Pneumonia Outcome of Care at the hospital.
#29. Hospital 30-Day Readmission Rates from Heart Attack: Lists the risk adjusted rate 
#    (percentage) for each hospital.
#30. Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Attack: 
#    varchar (50) Lists the mortality and readmission category in which the hospital falls. 
#    The values are:
#    • Better than U.S. National Average
#    • No Different than U.S. National Average • Worse than U.S. National Average
#    • Number of Cases too Small*
#32. Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#33. Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Attack: 
#    Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#34. Number of Patients - Hospital 30-Day Readmission Rates from Heart Attack: varchar (5) 
#    Lists the number of Medicare patients treated for Heart Attack.
#35. Footnote - Hospital 30-Day Readmission Rates from Heart Attack: Lists the footnote value 
#    when appropriate, as related to the Heart Attack Outcome of Care at the hospital.
#36. Hospital 30-Day Readmission Rates from Heart Failure: Lists the risk adjusted rate 
#    (percentage) for each hospital.
#37. Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Heart Failure: 
#    varchar (50) Lists the mortality and readmission category in which the hospital falls. 
#    The values are:
#    • Better than U.S. National Average
#    • No Different than U.S. National Average • Worse than U.S. National Average
#    • Number of Cases too Small*
#38. Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#39. Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Heart Failure: 
#    Lists the upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#40. Number of Patients - Hospital 30-Day Readmission Rates from Heart Failure: varchar (5) 
#    Lists the number of Medicare patients treated for Heart Failure.
#41. Footnote - Hospital 30-Day Readmission Rates from Heart Failure: Lists the footnote value 
#    when appropriate, as related to the Heart Failure Outcome of Care at the hospital.
#42. Hospital 30-Day Readmission Rates from Pneumonia: Lists the risk adjusted rate (percentage)
#    for each hospital.
#43. Comparison to U.S. Rate - Hospital 30-Day Readmission Rates from Pneumonia: varchar (50) 
#    Lists the mortality and readmission category in which the hospital falls. The values are:
#    • Better than U.S. National Average
#    • No Different than U.S. National Average • Worse than U.S. National Average
#    • Number of Cases too Small*
#44. Lower Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia: 
#    Lists the lower bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#45. Upper Readmission Estimate - Hospital 30-Day Readmission Rates from Pneumonia: Lists the 
#    upper bound (Interval Estimate) for each hospital’s risk-adjusted rate.
#46. Number of Patients - Hospital 30-Day Readmission Rates from Pneumonia: varchar (5) Lists 
#    the number of Medicare patients treated for Pneumonia.
#47. Footnote - Hospital 30-Day Readmission Rates from Pneumonia: Lists the footnote value when 
#    appropriate, as related to the Pneumonia Outcome of Care at the hospital.


#Submit File. 
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript3.R")

#There are many columns in this dataset. You can see how many by typing ncol(outcome) 
#(you can see the number of rows with the nrow function). In addition, you can see the names of 
#each column by typing names(outcome) (the names are also in the PDF document.
#
#To make a simple histogram of the 30-day death rates from heart attack (column 11 in the 
#outcome dataset), run
#> outcome[, 11] <- as.numeric(outcome[, 11])
#> ## You may get a warning about NAs being introduced; that is okay
#> hist(outcome[, 11])
#>1
#Because we originally read the data in as character (by specifying colClasses = "character" we 
#need to coerce the column to be numeric. You may get a warning about NAs being introduced but 
#that is okay.
                                                     
                                                     


#Set working directory
setwd("/Users/zzddfge/Desktop/Compartida/Curso_R_Coursera/Semana 4")

#Read the outcome data into R via the read.csv function and look at the first few rows.
d <- read.csv(file.path(getwd(), "data", "outcome-of-care-measures.csv"), colClasses = "character")
c <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
       'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia', 
       'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')
col <- matrix(c, nrow=1, ncol=3)
colnames(col) <- c('heart attack', 'pneumonia', 'heart failure')
rm(c)

state <- "TX"
outcome <- "heart failure"

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
return ( f[1]) )


source("best.R")
                                                                                                                                                                                                                            
best("BB", "heart attack")
best("AL", "heat attack")
best("AL", "heart attack") 

#> best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart attack")

#> best("TX", "heart failure")
#[1] "FORT DUNCAN MEDICAL CENTER"
best("TX", "heart failure")

#> best("MD", "heart attack")
#[1] "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "heart attack")

#> best("MD", "pneumonia")
#[1] "GREATER BALTIMORE MEDICAL CENTER"
best("MD", "pneumonia")

#> best("BB", "heart attack") 
#Error in best("BB", "heart attack") : invalid state
best("BB", "heart attack")

#> best("NY", "hert attack")
#Error in best("NY", "hert attack") : invalid outcome
best("NY", "hert attack")                                                                                                                                                                                                                                                                         

Course ID: rprog-034
Password: QJguZCEER7








suppressWarnings(d[d$State==state, col[,outcome]] <- as.numeric(d[d$State==state, col[,outcome]]))



state <- "TX"
outcome <- "heart failure"
rank = "1"
rv <- validateRank(d[d$State==state, 'State'], rank)

suppressWarnings(d[d$State==state, col[,outcome]] <- as.numeric(d[d$State==state, col[,outcome]]))
y <- as.data.frame(cbind(d[d$State==state, col[,outcome]], d[d$State== state,"Hospital.Name"]))
colnames(y) <- c("rate", "name")

#take away NAs.
y <- y[! is.na(y$rate), ]
y[,'rate'] <- as.numeric.factor(y[,'rate'])
x <- sqldf("select rate, name from y order by rate, name")
r = as.character(x[rv, 'name']) 

source("rankhospital.R")

rankhospital("BB", "heart attack", 1)
rankhospital("BB", "heart attak", 1)

state <- "TX"
outcome <- "heart failure"
rank <- "worst"
m <- validateParameters(state, outcome, d[d$State == state,'State'][1], col)
rv <- validateRank( d[d$State==state & ! is.na(d[,col[,outcome]]), 'State'], rank)
rankhospital(state, "heart attack", 100)


#>rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"
rankhospital("TX", "heart failure", 4)

#> rankhospital("MD", "heart attack", "worst")
#[1] "HARFORD MEMORIAL HOSPITAL"
rankhospital("MD", "heart attack", "worst")

#> rankhospital("MN", "heart attack", 5000)
#[1] NA
rankhospital("MN", "heart attack", 5000)



source("rankall.R")

head(rankall("heart attack", 20), 10)
pp<- rankall("pneumonia", "worst")
tail(rankall("heart failure"), 10)
