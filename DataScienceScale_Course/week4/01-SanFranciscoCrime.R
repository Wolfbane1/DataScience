library(dplyr)
library(ggplot2)
library(readr)
library(data.table)
library(reshape2)
library(lubridate)
library(cluster)
library(sqldf)

#Leemos el fichero de prueba.
detach()
setwd("/Users/zzddfge/Desktop/Compartida/Kaggle/San Francisco Crime/datos")
train <- read.csv('train.csv', header = TRUE, dec = ".", quote = "\"")

attach(train)


#Part 1: Problem Description. Give the name of the competition you selected and write a few 
#        sentences describing the competition problem as you interpreted it. You want your 
#        writeup to be self-contained so your peer-reviewer does not need to go to Kaggle to 
#        study the competition description. Clarity is more important than detail. What's the 
#        overall goal? What does the data look like? How will the results be evaluated?

#FGE
#Given a dataset with 12 years of crime reports, the competition ask us for several things:
# 1.- To explore the dataset visually, so we can learn about the Top Crimes Map.
# 2.- Predict the category of crime given time and location. 
#
#We have a train and a diferent test data set. The train dataset has the following characterisch:
#878.049 obs.
#9 variables:
#  Dates -> Date + Hour
#  Category -> Type of crime
#  Descript -> Description
#  DayOfweek -> Day of Week for the Date
#  PdDistrict -> District
#  Resolution -> Resolution of the crime
#  Address -> Address where the crime was committed.
#  x, y -> geolocation coordenades.
#
#Solution are going to be evaluated by comparing the percentage of correct answers on a test 
#dataset.

#PART 1: Analysis Approach.  Write a few sentences describing how you approached the problem. 
#What techniques did you use? Clarity is more important than technical depth in this exercise. 
#
#FGE
#FIRST, I'm going to analyze the data to search NAs, look for outliers, etc.
#

#######Dates
### No tiene valores NA
### 389.257 datos distintos
### Valores entre 06/01/2013 y 13/05/2015
table(is.na(Dates))
length(unique(Dates))
range(as.Date(Dates))
head(sort(Dates))
tail(sort(Dates))

#######Category
### No tiene valores NA
### 39 datos distintos
### 
table(is.na(Category))
length(unique(Category))
c <- table(Category)
cat <- sort(c, decreasing = TRUE)
qplot(cat, xlab="Categoría", ylab= "Número")
plot(cat, type="s")
rm(c)
rm(cat)

#######DayOfWeek
### No tiene valores NA
### 7 datos distintos.
### Viernes el día que más delito, pero más o menos constante.
### 
table(is.na(DayOfWeek))
length(unique(DayOfWeek))
table(DayOfWeek)
plot(DayOfWeek)
table(DayOfWeek, Category)
c <- melt(table(DayOfWeek, Category))
rm(c)

#######PdDistrict
### No tiene valores NA
### 10 valores distintos.
### Southern la zona que más delito, pero más o menos constante.
### 
table(is.na(PdDistrict))
length(unique(PdDistrict))
table(PdDistrict)
plot(PdDistrict)
table(PdDistrict, Category)
c <- melt(table(DayOfWeek, Category))
rm(c)

#SECOND, as there are 39 different crime category I am going to simplify grouping types of crime in four
#category: Blue Crime, White Crime, Sexual Crime and Others. So, we are going to have four 
#diferent categories to predict.
#

#Hacemos una agrupación del tipo de crímenes en función de la categoría vista en Google
crimen_blanco=c("FRAUD", "FORGERY/COUNTERFEITING", "BAD CHECKS" , "EXTORTION", "EMBEZZLEMENT", "SUSPICIOUS OCC",
                "BRIBERY")
crimen_azul=c("VANDALISM", "LARCENY/THEFT", "STOLEN PROPERTY", "ROBBERY", "DRIVING UNDER THE INFLUENCE",
              "DISORDERLY CONDUCT", "LIQUOR LAWS", "VEHICLE THEFT", "ASSAULT", "KIDNAPPING", "TRESPASS", 
              "ARSON", "RECOVERED VEHICLE")

crimen_sexual=c("SEX OFFENSES NON FORCIBLE", "SEX OFFENSES FORCIBLE", "PORNOGRAPHY/OBSCENE MAT")

other_crime=c("MISSING PERSON", "RUNAWAY", "FAMILY OFFENSES", "WEAPON LAWS", "DRUNKENNESS", 
              "SUICIDE", "TREA", "DRUG/NARCOTIC", "LOITERING")

#Adding to the variable
type <- rep(NA, length(Category))
train <- cbind(train, type)
colnames(train)[10] <- "type" 
rm(type)

"%ni%" <- Negate("%in%")

#Setting the value
train[Category %in% crimen_blanco,"type"] <- "WHITE CRIMEN"
train[Category %in% crimen_azul,"type"] <- "BLUE CRIMEN"
train[Category %in% crimen_sexual,"type"] <- "SEX CRIMEN"
train[Category %ni% crimen_azul & Category %ni% crimen_blanco & Category %ni% crimen_sexual, "type"] <- "OTHER_CRIME"

rm("%ni%")

#Check don't have any NA in the new variable.
table(train[,"type"])

#THIRD, I'm going to do is create four new variables:
# a) Number of days from the crime date until 20/11/2015.
# b) Number of hours between crime hour until 23.
# c) Hourly Range inside the day: 
#    06 - 09 --> 1
#    10 - 13 --> 2
#    14 - 16 --> 3
#    16 - 18 --> 4
#    19 - 21 --> 5
#    22 - 05 --> 6
# d) Season Weather (Winter, Summer, Spring, Autumn)
#

#a) Number of days from the crime date until 22/11/2015
today <- as.Date("20/11/15", "%d/%m/%y")
days_until_today <- lapply(Dates, function(x) difftime(today, x, units="days"))
rm(today)

#b) Number of hours until 23
hourly_range <- as.numeric(format(strptime(as.character(Dates), format = "%Y-%m-%d %H:%M:%S"), "%H"))
last_minute <- 23-hourly_range

#c) Hourly range inside the day
hourly_range <- cbind(hourly_range, rep(6, dim(train)[1]))
colnames(hourly_range)[1] <- "hora"
colnames(hourly_range)[2] <- "rango"
hourly_range[hourly_range[,1] >= 6 & hourly_range[,1] <= 9,2] <- 1
hourly_range[hourly_range[,1] >= 10 & hourly_range[,1] <= 13,2] <- 2
hourly_range[hourly_range[,1] >= 14 & hourly_range[,1] <= 16,2] <- 3
hourly_range[hourly_range[,1] >= 16 & hourly_range[,1] <= 18,2] <- 4
hourly_range[hourly_range[,1] >= 19 & hourly_range[,1] <= 21,2] <- 5

# d) Season Weather (Winter, Summer, Spring, Autumn)

#FOURTH, I'm going to transform the variable DayOfWeek to numeric values.
#   1 -> Monday     -> Weekday (1)
#   2 -> Tuesday    -> Weekday (1)
#   3 -> Wednesday  -> Weekday (1)
#   4 -> Thursday   -> Pre-Weekend (2)
#   5 -> Friday     -> Pre-Weekend (2)
#   6 -> Saturday   -> Weekend (3)
#   7 -> Sunday     -> Weekend (3)
dayweek <- as.numeric(DayOfWeek)
dayweek <- cbind(dayweek, rep(3, dim(train)[1]))
colnames(dayweek)[1] <- "weekday"
colnames(dayweek)[2] <- "type_day"
dayweek[dayweek[,1] >= 1 & dayweek[,1] <= 3,2] <- 1
dayweek[dayweek[,1] >= 4 & dayweek[,1] <= 5,2] <- 2

#PART 3: Initial Solution. Write a few sentences describing how you implemented your approach. 
#Think of it as a whiteboard conversation or a descriptive forum post rather than a full 
#technical report. Try to provide enough detail for someone with some experience to follow your 
#recipe and reproduce your results: Describe how you prepared the data, the method(s) you 
#applied, and any tools you used (not detailed code).    
#
#What languages and libraries did you use? What challenges did you run into? 

#FIRST, I'm going to normalize the variables: Boolean values for categorical variables, stand for
#numerical variables to analyze correlation. 
#

stand=function(x) {
  (x-mean(x))/sd(x)
}

#Target variable
   #train$type
target <- train$type

t_crimen_azul <- ifelse(train[,"type"] == "BLUE CRIMEN", 1, 0)
t_crimen_blanco <- ifelse(train[,"type"] == "WHITE CRIMEN", 1, 0)
t_crimen_sexual <- ifelse(train[,"type"] == "SEX CRIMEN", 1, 0)
t_crimen_otros <- ifelse(train[,"type"] == "OTHER_CRIME", 1, 0)

#Categorical variables
#  dayweek
#  hourly_range

#    dayweek
day1 <- ifelse(dayweek[,1] == 1, 1, 0)
day2 <- ifelse(dayweek[,1] == 2, 1, 0)
day3 <- ifelse(dayweek[,1] == 3, 1, 0)
day4 <- ifelse(dayweek[,1] == 4, 1, 0)
day5 <- ifelse(dayweek[,1] == 5, 1, 0)
day6 <- ifelse(dayweek[,1] == 6, 1, 0)
day7 <- ifelse(dayweek[,1] == 7, 1, 0)

weekend3 <- ifelse(dayweek[,2] == 3, 1, 0)
weekend2 <- ifelse(dayweek[,2] == 2, 1, 0)
weekend1 <- ifelse(dayweek[,2] == 1, 1, 0)

#    hourly_range
range1 <- ifelse(hourly_range[,2] == 1, 1, 0)
range2 <- ifelse(hourly_range[,2] == 2, 1, 0)
range3 <- ifelse(hourly_range[,2] == 3, 1, 0)
range4 <- ifelse(hourly_range[,2] == 4, 1, 0)
range5 <- ifelse(hourly_range[,2] == 5, 1, 0)
range6 <- ifelse(hourly_range[,2] == 6, 1, 0)

#Numeric variables
#  days_until_today
#  last_minute
#  x, y
x <- stand(train$X)
y <- stand(train$Y)
days <- stand(as.numeric(days_until_today))
min <- stand(as.numeric(last_minute))

d <- as.data.frame(cbind(t_crimen_azul,t_crimen_blanco,t_crimen_sexual, t_crimen_otros, day1, day2, day3, day4, day5, day6, day7, weekend3, weekend2, weekend1, range1, range2, range3, range4, range5, range6, x, y, days, min))

#delete from the rest of variables
rm(dayweek)
rm(hourly_range)
rm(crimen_sexual)
rm(crimen_azul)
rm(crimen_blanco)
rm(day1)
rm(day2)
rm(day3)
rm(day4)
rm(day5)
rm(day6)
rm(day7)
rm(days)
rm(range1)
rm(range2)
rm(range3)
rm(range4)
rm(range5)
rm(range6)
rm(min)
rm(x)
rm(y)
rm(weekend1)
rm(weekend2)
rm(weekend3)
rm(last_minute)
rm(days_until_today)
rm(t_crimen_azul)
rm(t_crimen_blanco)
rm(t_crimen_sexual)
rm(t_crimen_otros)
rm(other_crime)

#Analyze the correlation between the variables to have a first aproach.
#crimen_azul
cor(d, d$t_crimen_azul)
#There is very little "relations" with the targeted variable. 
#day1             0.002084959
#day2            -0.006088027
#day3             0.026571189
#day4             0.027019582
#day5            -0.012553346
#day6            -0.015237662
#day7            -0.021025929
#weekend3        -0.028162842
#weekend2         0.010714203
#weekend1         0.016109139
#range1          -0.051482120   --> Pos 4: 5,14%
#range2          -0.053203179   --> Pos 3: 5,53%
#range3          -0.022957741
#range4           0.011402686
#range5           0.066688350   --> Pos 1: 6,66%
#range6           0.037475146
#x               -0.020178386
#y                0.002117130
#days            -0.007508884
#min             -0.055351030   --> Pos 2: 5,53%

cor(d, d$t_crimen_blanco)
#day1             0.0051901848
#day2             0.0080692630
#day3            -0.0119772056
#day4            -0.0144127744
#day5             0.0015416406
#day6             0.0054841307
#day7             0.0056351325
#weekend3         0.0086266430
#weekend2        -0.0097479246
#weekend1         0.0008911239
#range1           0.0111849497
#range2           0.0427977009    --> Pos 2: 4,27%
#range3           0.0085937808
#range4          -0.0161409629
#range5          -0.0364354314    --> Pos 3: 3,64%
#range6          -0.0087057155
#x               -0.0217593338    --> Pos 4: 2,17%
#y               -0.0014445880
#days             0.0067814019
#min              0.0543320603    --> Pos 1: 5,43%

cor(d, d$t_crimen_sexual)
#t_crimen_azul   -6.617300e-02
#t_crimen_blanco -1.970632e-02
#t_crimen_sexual  1.000000e+00
#t_crimen_otros  -6.792529e-02
#day1            -1.818092e-03
#day2             8.492648e-05
#day3             1.159707e-03
#day4             4.816233e-03
#day5            -1.999000e-03
#day6            -1.165792e-03
#day7            -8.831505e-04
#weekend3        -1.587957e-03
#weekend2         2.096500e-03
#weekend1        -4.362492e-04
#range1          -4.616328e-03
#range2          -7.439894e-03
#range3          -3.927782e-03
#range4          -8.989056e-03
#range5          -5.684658e-03
#range6           2.601744e-02
#x               -1.229334e-03
#y                6.290970e-04
#days            -5.086820e-03
#min              2.334306e-02

cor(d, d$t_crimen_otros)
#day1            -0.004459739
#day2             0.001958529
#day3            -0.020592828
#day4            -0.020327719
#day5             0.012032439
#day6             0.012585721
#day7             0.018245208
#weekend3         0.023947732
#weekend2        -0.006035768
#weekend1        -0.016468380
#range1           0.046356142   --> Pos 2: 4,63%
#range2           0.032396691   --> Pos 4: 3,23%
#range3           0.019106395
#range4          -0.001874029
#range5          -0.047202721   --> Pos 1: 4,72%
#range6          -0.036719440   --> Pos 3: 3,67%
#x                0.031386754   
#y               -0.001468632
#days             0.004776564
#min              0.024239372

#First variables to use: range5, min, range2, range1, x, range6
d <- as.data.frame(cbind(d, target))
rm(target)
f <- formula(target ~ range1 + range2 + range5 + range6 + x + min)

#SECOND, I'm going to try differente methods: SVM, Random Forest, etc. 
#
#
library("rpart")
library("tree")
library("randomForest")
library("e1071")
library("lattice")
library("caret")
library(rpart.plot)

#Sample data for train and validation. 
set.seed(100)
data.part <- createDataPartition(d$target, times=2, p=.5)
train.df <- d[ data.part$Resample1, ]
test.df <- d[ data.part$Resample2, ]
rm(data.part)

crimen_test_names <- c("BLUE CRIMEN","WHITE CRIMEN","SEX CRIMEN","OTHER_CRIME")

#DECISION TREE: Ahora vamos a por el árbol
#tree
tree <- rpart(f, method="class", data=train.df)

#visualization for the tree
print(tree)
rpart.plot(tree,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="green",split.col="red")

#prediction of the tree
tree_pred <- predict(tree, newdata = test.df)

#Hacemos un bucle para detectar cuál es la mayor predicción para un registro dado.
tree_crimen_test <- c()
for (i in 1:nrow(tree_pred)) {
  tree_crimen_test <- c(tree_crimen_test, crimen_test_names[which.max(tree_pred[i,])])
} 
rm(i)

tree_result <- as.vector(test.df$target) == tree_crimen_test
table(tree_result)
table(predition = tree_crimen_test, real= test.df$target)
tree_acc <- sum(tree_result) / length(tree_crimen_test)
tree_acc

#RANDOM FOREST: 
rf <- randomForest(f, data=train.df)
plot(rf)

rf_pred <- predict(rf, type="prob", newdata=test.df)
rf_crimen_test <- c()
for (i in 1:nrow(rf_pred)) {
  rf_crimen_test <- c(rf_crimen_test, crimen_test_names[which.max(rf_pred[i,])])
} 

rf_result <- as.vector(test.df$target) == rf_crimen_test
table(rf_result)
table(rf_crimen_test, test.df$target)
rf_acc <- sum(rf_result) / length(rf_crimen_test)
rf_acc

importance(rf)

#SUPPORT VECTOR MACHINE
svmc <- svm(f, data=test.df)

pred_svm <- predict(support_vector_machine, newdata=sfTest)
table(pred = pred_svm, true =sfTest$pop)

accuracy_svm <- (46+5524+10102+8963+8577) / length(sfTest$pop)
accuracy_svm

table(pred = pred_svm, true = sfTest$pop)

#Part 4: Initial Solution Analysis. Write a few sentences assessing your approach. Did it work? 
#What do you think the problems were? 

#Decision Tree
table(predition = tree_crimen_test, real= test.df$target)
#           real
#predition      BLUE CRIMEN OTHER_CRIME SEX CRIMEN WHITE CRIMEN
#BLUE CRIMEN        98584       82685       1197        11281
#WHITE CRIMEN      101732      123336       1082        19129

#The decision tree prediction is very poor, only 26,81231%. And the Decision tree could only 
#predict BLUE CRIMEN and WHITE CRIMEN with the following accuracy:
# Blue Crimen
#   True Positive: 95.584
#   False Positive: 82.685 + 1.197 + 11.281
#   Accuracy = 95584/(82685+1197+11281+95584) = 50,11%
# White Crimen
#   True Positive: 19.129
#   False Positive: 123.336 + 1.082 + 19.129
#   Accuracy = 19129/(101732+123336+1082+19129) = 7,798874%

#Random Forest
table(predition = rf_crimen_test, real= test.df$target)
#           real
#predition      BLUE CRIMEN OTHER_CRIME SEX CRIMEN WHITE CRIMEN
#BLUE CRIMEN       107210       83425       1122        12073
#WHITE CRIMEN       93106      122596       1157        18337

#The random forest prediction is still very poor, only 28,59671%. And the random fores could 
#predict SEX CRIMEN and OTHER_CRIMEN neither. 
# Blue Crimen
#   True Positive: 107.210
#   False Positive: 83.425 + 1.122 + 12.073
#   Accuracy = 107210 / (107210+83425 + 1122 + 12073) = 52,59%
# White Crimen
#   True Positive: 18.337
#   False Positive: 93.106 + 122.596 + 1.157
#   Accuracy = 18337/(18337+93106 + 122596 + 1157) = 7,796476%

#Part 5: Revised Solution and Analysis.  Write a few sentences describing how you improved on 
#your solution, and whether or not it worked. 

#Trying to improve results with bootstrapping. 
#Sample data for train and validation. 
table(train.df$target)

#BLUE CRIMEN  OTHER_CRIME   SEX CRIMEN WHITE CRIMEN 
#200.316       206.021         2.279        30.410 

set.seed(100)

#We get bootstrapping from Sex Crimen.
d_sc <- train.df[train.df$target=="SEX CRIMEN",]
boot_sex_crimen <- createResample(d_sc$target, 1)
train2.df <- d_sc[ boot_sex_crimen$Resample1, ]
for (i in 1:75) {
  boot_sex_crimen <- createResample(d_sc$target, 1)
  train2.df <- rbind(train2.df, d_sc[ boot_sex_crimen$Resample1, ])
}

train_final.df <- train.df
train_final.df <- rbind(train_final.df, train2.df)
rm(train2.df)
rm(boot_sex_crimen)
rm(d_sc)

#We get bootstrapping from White Crimen.
d_wc <- train.df[train.df$target=="WHITE CRIMEN",]
boot_white_crimen <- createResample(d_wc$target, 1)
train2.df <- d_wc[ boot_white_crimen$Resample1, ]
for (i in 1:6) {
  boot_white_crimen <- createResample(d_wc$target, 1)
  train2.df <- rbind(train2.df, d_wc[ boot_white_crimen$Resample1, ])
}

train_final.df <- rbind(train_final.df, train2.df)
rm(d_wc)
rm(train2.df)
rm(boot_white_crimen)
rm(i)

#Saved the prepared data just for sure.
write.csv(train_final.df, "train_final.csv")

#DECISION TREE: Ahora vamos a por el árbol
#tree
tree <- rpart(f, method="class", data=train_final.df)

#visualization for the tree
print(tree)
rpart.plot(tree,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="green",split.col="red")

#prediction of the tree
tree_pred <- predict(tree, newdata = test.df)

#Hacemos un bucle para detectar cuál es la mayor predicción para un registro dado.
tree_crimen_test <- c()
for (i in 1:nrow(tree_pred)) {
  tree_crimen_test <- c(tree_crimen_test, crimen_test_names[which.max(tree_pred[i,])])
} 
rm(i)

tree_result <- as.vector(test.df$target) == tree_crimen_test
table(tree_result)
table(predition = tree_crimen_test, real= test.df$target)
tree_acc <- sum(tree_result) / length(tree_crimen_test)
tree_acc

#RANDOM FOREST: 
rf <- randomForest(f, data=train.df)
plot(rf)

rf_pred <- predict(rf, type="prob", newdata=test.df)
rf_crimen_test <- c()
for (i in 1:nrow(rf_pred)) {
  rf_crimen_test <- c(rf_crimen_test, crimen_test_names[which.max(rf_pred[i,])])
} 

rf_result <- as.vector(test.df$target) == rf_crimen_test
table(rf_result)
table(rf_crimen_test, test.df$target)
rf_acc <- sum(rf_result) / length(rf_crimen_test)
rf_acc

importance(rf)

#Decision Tree
table(predition = tree_crimen_test, real= test.df$target)

#           real
#rf_crimen_test BLUE CRIMEN OTHER_CRIME SEX CRIMEN WHITE CRIMEN
#BLUE CRIMEN       85342       72495        704         8279
#OTHER_CRIME       85131      106089        915        17030
#SEX CRIMEN        29843       27437        660         5101
#> tree_acc
#> tree_acc <- sum(tree_result) / length(tree_crimen_test)
#[1] 0.437539


#Random Forest
table(predition = rf_crimen_test, real= test.df$target)

#The random forest prediction is still very poor, only 28,59671%. And the random fores could 
#predict SEX CRIMEN and OTHER_CRIMEN neither. 
# Blue Crimen
#   True Positive: 107.210
#   False Positive: 83.425 + 1.122 + 12.073
#   Accuracy = 107210 / (107210+83425 + 1122 + 12073) = 52,59%
# White Crimen
#   True Positive: 18.337
#   False Positive: 93.106 + 122.596 + 1.157
#   Accuracy = 18337/(18337+93106 + 122596 + 1157) = 7,796476%
