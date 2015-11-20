
#The zip file contains 332 comma-separated-value (CSV) files containing pollution monitoring data 
#for fine particulate matter (PM) air pollution at 332 locations in the United States. 
#Each file contains data from a single monitor and the ID number for each monitor is contained 
#in the file name. For example, data for monitor 200 is contained in the file "200.csv". 
#Each file contains three variables:
#  Date: the date of the observation in YYYY-MM-DD format (year-month-day)
#  sulfate: the level of sulfate PM in the air on that date (measured in micrograms per cubic meter)
#  nitrate: the level of nitrate PM in the air on that date (measured in micrograms per cubic meter)
#For this programming assignment you will need to unzip this file and create the directory 
#'specdata'. Once you have unzipped the zip file, do not make any modifications to the files in 
#the 'specdata' directory. In each file you'll notice that there are many days where either 
#sulfate or nitrate (or both) are missing (coded as NA). This is common with air pollution 
#monitoring data in the United States.
setwd("/Users/zzddfge/Desktop/Compartida/Curso_R_Coursera/Semana 2")


#PART 1
source("pollutantmean.R")

pollutantmean("specdata", "sulfate", 1:10)
## should be [1] 4.064
# [1] 4.064

pollutantmean("specdata", "nitrate", 70:72)
## should be [1] 1.706
# [1] 1.706

pollutantmean("specdata", "nitrate", 23)
## should be [1] 1.281
# [1] 1.281

#Part 2
source("complete.R")

complete("specdata", 1)
##    id nobs
## 1  1  117
#
# OK

complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
#
# OK

complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
#
# OK

complete("specdata", 3)
##   id nobs
## 1  3  243
#
# OK

#Part 3
source("corr.R")
source("complete.R")
cr <- corr("specdata", 150)
head(cr)
## [1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630

cr <- corr("specdata", 400)
head(cr)
## [1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630

cr <- corr("specdata", 5000)
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 
length(cr)
## [1] 0

cr <- corr("specdata")
summary(cr)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000
length(cr)
## [1] 323

#Submit
source("http://d396qusza40orc.cloudfront.net/rprog%2Fscripts%2Fsubmitscript1.R")



#QUIZ
cube <- function(x, n) {
  x^3
}
cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3)

x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
