#install.packages("caret")
#install.packages("rpart")
#install.packages("tree")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("ggplot2")

library("rpart")
library("tree")
library("randomForest")
library("e1071")
library("ggplot2")
library("lattice")
library("caret")
library(rpart.plot)

sample <- read.csv("/Users/zzddfge/Desktop/Compartida/Data Science at Scale/Practic Predict /Week 2/seaflow_21min.csv", header = TRUE)

attach(sample)

#file_id: The data arrives in files, where each file represents a three-minute window; this field represents which file the data came from. 
#         The number is ordered by time, but is otherwise not significant.

head(file_id)
summary(file_id)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#203.0   204.0   206.0   206.2   208.0   209.0 
hist(file_id)
table(file_id)
#file_id
#203   204   205   206   207   208   209 
#10454  9435  8376  9215 11444 11995 11424 

#time: This is an integer representing the time the particle passed through the instrument. 
#         Many particles may arrive at the same time; time is not a key for this relation.
head(time)
summary(time)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#12.0   174.0   362.0   341.5   503.0   643.0 
hist(time)
table(time)
boxplot(time)

#cell_id: A unique identifier for each cell WITHIN a file. (file_id, cell_id) is a key for this relation.
head(cell_id)
summary(cell_id)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    7486   15000   15010   22400   32080 
hist(cell_id)
table(cell_id)
boxplot(cell_id)

#d1, d2: Intensity of light at the two main sensors, oriented perpendicularly. 
#        These sensors are primarily used to determine whether the particles are properly centered
#        in the stream. Used primarily in preprocesssing; they are unlikely to be useful for 
#        classification.

head(d1)
summary(d1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1328    7296   17730   17040   24510   54050 
hist(d1)
table(d1)
boxplot(d1)

head(d2)
summary(d2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32    9584   18510   17440   24660   54690 
hist(d2)
table(d2)
boxplot(d2)

#fsc_small, fsc_perp, fsc_big: Forward scatter small, perpendicular, and big. 
#                               These values help distingish different sizes of particles.
head(fsc_small)
summary(fsc_small)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#10000   31340   35480   34920   39180   65420 
hist(fsc_small)
table(fsc_small)
boxplot(fsc_small)

head(fsc_perp)
summary(fsc_perp)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0   13500   18070   17650   22240   63460 
hist(fsc_perp)
table(fsc_perp)
boxplot(fsc_perp)

head(fsc_big)
summary(fsc_big)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#32380   32400   32400   32410   32420   32460 
hist(fsc_big)
table(fsc_big)
boxplot(fsc_big)

#pe: A measurement of phycoerythrin fluorescence, which is related to the wavelength associated 
#    with an orange color in microorganisms
head(pe)
summary(pe)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    1635    2421    5325    5854   58680 
hist(pe)
table(pe)
boxplot(pe)

qplot(pe, chl_small, data=sample, color=pop, alpha=1/8)
qplot(pe, chl_small, data=sample, facets=pop~., color=pop, alpha=1/8)

#chl_small, chl_big: Measurements related to the wavelength of light corresponding to chlorophyll.
head(chl_small)
summary(chl_small)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#3485   22520   30510   30160   38300   64830 
hist(chl_small)
table(chl_small)
boxplot(chl_small)

head(chl_big)
summary(chl_big)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0    2800    7744    8328   12880   57180 
hist(chl_big)
table(chl_big)
boxplot(chl_big)

#pop: This is the class label assigned by the clustering mechanism used in the production system.
#     It can be considered "ground truth" for the purposes of the assignment, but note that there
#     are particles that cannot be unambiguously classified, so you should not aim for 100% 
#     accuracy. The values in this column are crypto, nano, pico, synecho, and ultra

head(pop)
summary(pop)
#crypto    nano    pico synecho   ultra 
#102   12698   20860   18146   20537 
hist(pop)
table(pop)
boxplot(pop)


#####
## REGRESION LOGICA 
#####

#Dividimos el conjunto de datos en Train y Test
set.seed(1234)
trainIndex <- createDataPartition(sample$cell_id, p = 0.5, list=FALSE, times=1)
sfTrain <- sample[trainIndex, ]
sfTest <- sample[-trainIndex, ] 
rm(trainIndex)

summary(sfTrain$time)
        

#Lanzamos la regresión lógica
#sf_glm <- glm(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small,
sf_glm <- glm(pop ~ pe + chl_small,
             data=sfTrain,
              family=binomial("logit"))

summary(sf_glm)
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#3-2.47311   0.00012   0.00032   0.00087   2.48891  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.693e+01  3.693e+00   7.293 3.03e-13 ***
#  pe          -3.245e-04  4.668e-05  -6.952 3.61e-12 ***
#  chl_small   -2.822e-04  4.518e-05  -6.246 4.20e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 836.630  on 36171  degrees of freedom
#Residual deviance:  63.575  on 36169  degrees of freedom
#AIC: 69.575
#
#Number of Fisher Scoring iterations: 14

#Vamos a calcular el grado de predicción bueno
y=ifelse(sf_glm$fitted.values<=0.5, 0, 1)
table(sfTrain$pop, y)

head(sf_glm$fitted.values)
table(pop)

#Montamos un árbol
pop_glm <- rpart(sf_glm, method="class", data=sfTrain) 
print(pop_glm)

table(pe)

#1) root 72343 51483 pico (0.0014 0.18 0.29 0.25 0.28)  
#2) pe< 5004 52672 31904 pico (0 0.22 0.39 3.8e-05 0.38)  
#4) chl_small< 32164 22730  3897 pico (0 0.00018 0.83 8.8e-05 0.17) *
#  5) chl_small>=32164 29942 13585 ultra (0 0.39 0.065 0 0.55)  
#10) chl_small>=41297.5 10372  1326 nano (0 0.87 9.6e-05 0 0.13) *
#  11) chl_small< 41297.5 19570  4538 ultra (0 0.13 0.099 0 0.77) *
#  3) pe>=5004 19671  1527 synecho (0.0052 0.053 0.0047 0.92 0.015)  
#6) chl_small>=38030.5 1297   275 nano (0.079 0.79 0 0.065 0.069) *
#  7) chl_small< 38030.5 18374   314 synecho (0 0.0012 0.005 0.98 0.011) *

# plots decision tree
plot(pop_glm, main="Classification Tree of Ocean Microbes using Seaflow")
text(pop_glm, use.n=TRUE, all=TRUE, cex=0.8)

#use fitted glm model to predict pop in sfTest
pred_glm <- predict(pop_glm, newdata=sample, type="class") # type="class" outputs the highest probability class

#the following code gives raw probabilities for each class
#pred_glm_raw <- predict(pop_glm, newdata=sfTest) 

pred_glm <- as.data.frame(pred_glm) #convert matrix to df
sfTest <- cbind(sample, pred_glm)
#calculate the accuracy by dividing correct predictions by the number of observations in the test dataset 
accuracy_glm <- sum(pred_glm$pred_glm == sfTest$pop)/NROW(sfTest)
accuracy_glm

#####RESPUESTASSSSS

s <- read.csv("/Users/zzddfge/Desktop/Compartida/Data Science at Scale/Practic Predict /Week 2/seaflow_21min.csv", header = TRUE)

#Q2. How many "synecho" particules are there?
# 18146
table(s$pop)

#Q3. What is the 3rd Quantile of the field fsc_small? (the summary function computes this on your behalf)
# 39180
summary(s$fsc_small)

#Q4. What is the mean of the variable "time" for your training set?
# 341.7
set.seed(100)
#trainIndex <- createDataPartition(s$pop, p = 0.5, list=FALSE, times=1)
testIndex <- sample( nrow(s), nrow(s)/2 )
sfTest <- s[testIndex, ]
sfTrain <- s[-testIndex, ] 

data.part <- createDataPartition(s$pop, times=2, p=.5)
train.df <- s[ data.part$Resample1, ]
test.df <- s[ data.part$Resample2, ]

#342
summary(train.df$time)

#set.seed(100)
#test_subscript=sample(nrow(om),nrow(om)/2)
#om_test=om[test_subscript,]
#om_train=om[-test_subscript,]
#
#mean(om_train$time)

table(sfTrain$pop)
table(sfTest$pop)
table(s$pop)
rm(testIndex)
summary(sfTrain$time)
summary(sfTest$time)
summary(s$time)

#Q5. In the plot of pe vs. chl_small, the particles labeled ultra should appear to be 
#somewhat "mixed" with two other populations of particles. Which two populations?
#  pico, nano.
qplot(pe, chl_small, data=s, color=pop, alpha=1/8)
qplot(pe, chl_small, data=s, facets=pop~., color=pop, alpha=1/8)

#Q6. Use print(model) to inspect your tree. Which populations, if any, is your tree 
#incapable of recognizing? (Which populations do not appear on any branch?)
#  crypto
#Tenemos que montar un árbol en base a una fórmula  
# model <- train(formula, dataset)
# siendo la fórmula del tipo: formula <- formula(response ~ var1 + var2 + var3)
# hay que montar una formula del tipo: fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small

#Vamos a por la fórmula
#d <- as.data.frame(cbind(sfTrain$pop, sfTrain$fsc_small, sfTrain$fsc_perp, sfTrain$fsc_big, sfTrain$pe, sfTrain$chl_big, sfTrain$chl_small))
#colnames(d) <- c("pop", "fsc_small", "fsc_perp", "fsc_big", "pe", "chl_big", "chl_small")
attach(s)
f <- formula(pop ~ fsc_small + fsc_perp + fsc_big + pe + chl_big + chl_small) 

#Ahora vamos a por el árbol
arbol <- rpart(f, method="class", data=sfTrain)
arbol2 <- rpart(f, method="class", data=train.df)

print(arbol)
print(arbol2)

rpart.plot(arbol,branch=0,branch.type=2,type=1,extra=102,shadow.col="pink",box.col="green",split.col="red")
rpart.plot(arbol2,branch=0,branch.type=2,type=1,extra=102,shadow.col="blue",box.col="green",split.col="red")

table(sfTrain$pop)

#Q7. What is the value of the threshold on the pe field learned in your model?
# 5004

#Q8. Based on your decision tree, which variables appear to be most important in predicting the class population?
# pe, chl_small

#Q9. How accurate was your decision tree on the test data? Enter a number between 0 and 1.
# 0.855409

#Hacemos la predicción contra el juego de Test.
pred <- predict(arbol, newdata = sfTest)

#Hacemos un bucle para detectar cuál es la mayor predicción para un registro dado.
pop_test <- c()
pop_names <- c("crypto","nano","pico","synecho","ultra")

for (i in 1:nrow(pred)) {
  pop_test <- c(pop_test, pop_names[which.max(pred[i,])])
} 

result <- as.vector(sfTest$pop) == pop_test
table(result)
table(pop_test, sfTest$pop)
accuracy <- sum(result) / length(pop_test)
accuracy

#Q10. What was the accuracy of your random forest model on the test data? 
#Enter a number between 0 and 1.
#  0.9187471
arbol_random <- randomForest(f, data=sfTrain)
plot(arbol_random)

pred_arbol_random <- predict(arbol_random, type="prob", newdata=sfTest)
pop_test_arbol_random <- c()

for (i in 1:nrow(pred_arbol_random)) {
  pop_test_arbol_random <- c(pop_test_arbol_random, pop_names[which.max(pred_arbol_random[i,])])
} 

result_arbol_random <- as.vector(sfTest$pop)==pop_test_arbol_random
table(result_arbol_random)
table(pop_test_arbol_random, sfTest$pop)
accuracy_arbol_random <- sum(result_arbol_random) / length(pop_test_arbol_random)
accuracy_arbol_random

#Q11. After calling importance(model), you should be able to determine which variables appear 
#to be most important in terms of the gini impurity measure. Which ones are they?
#
importance(arbol_random)
#MeanDecreaseGini
#fsc_small        2653.6971
#fsc_perp         2102.1039
#fsc_big           199.1639
#pe               8921.2405
#chl_big          4994.6412
#chl_small        8016.4628

#Q12. What was the accuracy of your support vector machine model on the test data? 
#Enter a number between 0 and 1.
# 0.9181941
support_vector_machine <- svm(f, data=sfTrain)

pred_svm <- predict(support_vector_machine, newdata=sfTest)
table(pred = pred_svm, true =sfTest$pop)

accuracy_svm <- (46+5524+10102+8963+8577) / length(sfTest$pop)
accuracy_svm

table(pred = pred_svm, true = sfTest$pop)

#Q13. Construct a confusion matrix for each of the three methods using the table function. 
#What appears to be the most common error the models make?
#

#Matriz de confusión del Arbol de Decisión
table(pred=pop_test, true=sfTest$pop)

#Matriz de confusión del arbol ramdon
table(pred=pop_test_arbol_random, true=sfTest$pop)

#Matriz de confusion del svm
table(pred = pred_svm, true = sfTest$pop)


#0.9726917
#0.0544976

om_clean=s[s$file_id!=208,]
set.seed(100)
test_subscript2=sample(nrow(om_clean),nrow(om_clean)/2)
om_clean_test=om_clean[test_subscript2,]
om_clean_train=om_clean[-test_subscript2,]
model_4 <- svm(f, data=om_clean_train)
om_clean_predict_4=predict(model_4,newdata=om_clean_test)
table(pred = om_clean_predict_4, true =om_clean_test$pop)

accuracy_svm2 <- (42+4624+9800+7013+7871) / length(om_clean_test$pop)
accuracy_svm2

table(s$fsc_big)

