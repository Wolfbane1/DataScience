
library(swirl)
swirl()

setwd("/Users/zzddfge/Desktop/Compartida/Curso_R_Coursera")
train <- read.csv('hw1_data.csv', header = TRUE, dec = ",", sep=",", quote = "\"")

x <- 4
class(x)
x <- c(4, TRUE)
x <- c(1,3, 5)
y <- c(3, 2, 10)
cbind(x, y)
x <- list(2, "a", "b", TRUE)
x[[2]]
class(x[[2]])
x <- 1:4 
y <- 2
x + y
names(train)
head(train, 2)
nrow(train)
tail(train, 2)
train[47,]
nrow(train[is.na(train$Ozone)==TRUE,])
summary(train[train$Ozone>=31 & train$Temp>90,]$Solar.R)
nrow(train[!is.na(train$Ozone),])
summary(train[train$Month==6,]$Temp)
summary(train[train$Month==5,]$Ozone)
