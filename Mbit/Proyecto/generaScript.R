
library(R.matlab)

setwd("/Users/zzddfge/Desktop/Compartida/Proyecto_Master")
origenMat <- file.path(getwd(), "data", "Matr_ATC5el_Diabeticos")
origenCSV <- file.path(getwd(), "data", "csv")
origenData <- file.path(getwd(), "data")

ficheros <- dir(origenMat)
csv <- read.csv(file.path(origenData, "names.csv"))
names <- dimnames(csv)[2]

estructura <- cbind(ficheros, substr(ficheros, 1, nchar(ficheros)-4), paste("data/", ficheros, ".csv", sep=""))

#Hacemos el script para cargar todos los ficheros
cargar <- paste("load('data/", estructura[,1], "');", sep = "")
write.csv(cargar, "cargar.m")

convertir <- paste("struct2csv(", estructura[,2], ",'", estructura[,3], "');", sep="")
write.csv(convertir, "convertir.sm")






m <- as.matrix(ATC)
nombres <- rapply(names[1], c)
m4 <- matrix(rep(0), nrow = 2174, ncol = 746)
m4[1,1]
m[1,1]

dimnames(m)[2] <- names[2]
m1 <- m[1,1]
m2 <- matrix(m, nrow=2174, ncol=746)
m3 <- matrix(m)

for (i in ncols(m))




df <- as.data.frame(cbind(ID, sex, edad, m))

class(m)
class(m4)
m5 <- matrix(ATC)
m5[1,1]
class(c)
u <- unlist(ATC)
m1 <- matrix(u, nrows = 2174, ncols = 746)
u[1,1]


v <- c(1,2,3,4,5,6,7,8)
m1 <- matrix(as.vector(v), nrows = 2, ncols = 4, byrow = TRUE)
dim(v) <- c(2, 4)


class(mat)
u<- unlist(mat)
u <- mat[1]
u$MatrATC5el.norepe.DiabN5424X.11[1]

m6 <- m5[1]
m6 <- unlist(m5[1])
p <- m6[7*746+13]

fila <- length(m6)/746
matriz <- matrix(rep(-1), nrow=fila, ncol=746)
names(matriz) <- nombres
f = 1
c = 1
for (i in 1:length(m6)) {
  if ( f > fila ) {
    f <- 1
    c <- c + 1
  }
  
  matriz[f, c] <- m6[i]
  f <- f + 1
}

m7 <- m6

#dimnames(m7) <- list(seq(1:fila), nombres)
                     
                     
f = 1
c = 1
for (i in 1:length(m6)) {
  if ( f > fila ) {
    f <- 1
    c <- c + 1
  }
  
  m7[f, c] <- m6[i]
  f <- f + 1
}

n <- seq(1:746)
dimnames(n) <- nombres

mat

eval( parse (text="matriz <- unlist(mat$MatrATC5el.norepe.DiabN5424X.11[4])") )
dim(matriz) <- c(fila, 746)

cadena <- "MatrATC5el.norepe.DiabN5424X.11"
pp <- regexpr("[1-9]*X", cadena)
substr(cadena, pp, pp+3)

