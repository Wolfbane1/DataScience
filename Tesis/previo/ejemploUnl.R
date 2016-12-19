#Librería que lee la estructura de un fichero de matlab.
library(R.matlab) 

setwd("/Users/zzddfge/Desktop/Compartida/Tesis")
file_aae_2009 <- file.path(getwd(), "AAE 2009.unl")
file_hos_2009 <- file.path(getwd(), "HOS 2009.unl")

##########
###Ficheros UNL
##########

aae_2009 <- read.csv(file_aae_2009, header=TRUE, sep="|")
hos_2009 <- read.csv(file_hos_2009, header=TRUE, sep="|")



##########
###AAE
##########






