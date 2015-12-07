
## Estructura de datos
# Id    --> Viene en el fichero .mat
# Sexo  --> Viene en el fichero .mat
# Edad  --> Viene en el fichero .mat
# RangoEdad --> Lo añadimos para que se rellene después. 
# Anyo  --> Lo añadimos a partir de la organización de los ficheros. 
# CRG   --> Lo añadimos a partir del nombre del fichero. 
# ATCs  --> Son 746 columnas que vienen en los ficheros .mat
# El objetivo es montar una matriz con toda la información necesaria para tener una única tabla.
####

##############################################################################
#### EJECUCION OBLIGATORIA 
##Cabecera --> Establece variables "necesarias" sobre directorios de trabajo 
####
##############################################################################

#Directorio de trabajo lo asignamos al raiz. 
setwd("/Users/zzddfge/Desktop/Compartida/Proyecto_Master")

#Este programa espera que bajo el directorio raíz tenga la siguiente estructura
#      data/mat --> Aquí estarán los ficheros .mat ordenados por año.
#      data/csv --> Aquí se escribirá el fichero .csv con toda la información.
origenData <- file.path(getwd(), "data")
origenMat <- file.path(getwd(), "data", "mat")
destinoCSV <- file.path(getwd(), "data", "csv")
ficheroDestino <- file.path(destinoCSV, "pacientesDiabetes.csv")
ficheroTomanA10Destino <- file.path(destinoCSV, "tomanA10.mat")
ficheroNoTomanA10Destino <- file.path(destinoCSV, "noTomanA10.mat")

######################################################################
#### EJECUCION SOLO PARA PROCESAR .MAT
## Código para procesar todos los ficheros .mat y generar el CSV. 
####
######################################################################

source("procesaMat.R")

#procesamos todos los ficheros y generamos el CSV final.
procesa(origenData, origenMat, destinoCSV)

rm(procesa)
rm(procesaAnyo)
rm(procesaCabecera)
rm(procesaFicheroMat)

######################################################################
#### EJECUCION SOLO PARA HACER VISUALIZACION DESCRIPTIVA
## Código Revisión estadísticas básicas de los ficheros. 
####
######################################################################

source("analisisbasico.R")

csv <- procesaCSV(ficheroDestino)

#Liberamos sobre todo para liberar el espacio del environment dentro del RStudio.
rm(ficheroDestino)
rm(destinoCSV)
rm(origenData)
rm(origenMat)
rm(ficheroTomanA10Destino)
rm(ficheroNoTomanA10Destino)
rm(estadisticosATCsVacios)
rm(estadisticosEdad)
rm(estadisticosId)
rm(estadisticosNivelCRG)
rm(estadisticosPosATCsVacios)
rm(estadisticosSexo)
rm(procesaCSV)

#Comparativas entre variables. 
ejecutaComparativa(csv)
csv <- ejecutaRangoEdad(csv)

rm(obtieneRangoEdad)
rm(ejecutaComparativa)
rm(ejecutaRangoEdad)
rm(ejecutaComparativaRangoEdad)

#TODO en esta parte:
#  --> relación de existencia / probabilidad de ocurrencia.
#  --> hacer zoom sobre los grupos con más número: 6144 y 5424
#  --> búsqueda de DBSCAN para R. 
#  --> cálculo del PCA. Cuando se tengan los PCAs, seguramente haya que evaluar rango de edad otra vez.
#  --> análisis de correspondencia. 




#TODO a Futuro
# --> Incorporar 2012. Hay que sumar 1 a la Edad de los pacientes que están en 2011.
# --> Analizar evolución de 2011 a 2012. Hay que ver los que cambian de CRG y habría que ver 
#      evolución de medicamentos. 
# --> 




######################################################################
## Análisis de Correspondencia - Nivel de CRG y Género
######################################################################

#Montamos la matriz de correspondencia
m <- matrix(table(csv2$nivel, csv2$Genero), nrow = 21, ncol=2)
rownames(m) <- unique(csv2$nivel)
colnames(m) <- c("1", "2")

#############
## METODO 1
#############

# Se calculan los porcentajes
#ncol<-2
#nrow<-21
#n<-sum(m)
#
#rtot<-apply(m,1,sum)
#ctot<-apply(m,2,sum)
#xrtot<-cbind(rtot,rtot)
#xrtot<-m/xrtot
#xctot<-rbind(ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot,ctot)
#xctot<-m/xctot
#rdot<-rtot/n
#cdot<-ctot/n
#
## Se calculan las matrices de distancias entre columnas
#dcols <- matrix(0,ncol,ncol)
#for(i in 1:ncol) {
#  for(j in 1:ncol) {
#    d<-0
#    dcols[i,j] <- sqrt(d)
##    for(k in 1:nrow) d<-d+(xctot[k,i]-xctot[k,j])^2/rdot[k]
#  }
#}

# Se calculan las matrices de distancias entre filas
#drows <- matrix(0,nrow,nrow)
#for(i in 1:nrow) {
#  for(j in 1:nrow) {
#    d<-0
#    for(k in 1:ncol) d<-d+(xrtot[i,k]-xrtot[j,k])^2/cdot[k]
#    drows[i,j] <- sqrt(d)
#  }
#}
#
#rm(i)
#rm(k)
#rm(j)
#
# Se aplica el MDS metrico
#r1 <- cmdscale(dcols, eig=TRUE)
#r1$points
#r1$eig
#
#c1<- cmdscale(drows,eig=TRUE)
#c1$points
#c1$eig
#xrtot
#
# Se dibujan las coordenadas en un dos dimensiones
#par(pty="s") 
#plot(r1$points,xlim=range(r1$points[,1],c1$points[,1]),
#     ylim=range(r1$points[,1],c1$points[,1]),
#     type="n",
#     xlab="Género 1",ylab="Género 2",lwd=2) 
#text(r1$points,labels=c("ED1","ED2","ED3","ED4","ED5"),lwd=2) 
#text(c1$points,labels=c("Nopar","parnS","parS"),lwd=4)
#abline(h=0,lty=2)
#abline(v=0,lty=2)

distancia <- dist(m)

mds2 <- cmdscale(distancia, eig = TRUE, k = 2)
mds1 <- cmdscale(distancia, k = 2)

# plot
par(pty="s") 
plot(mds1[,1], mds1[,2], type = "n", xlab = "Género 1", ylab = "Género 2", axes = TRUE,
         main = "cmdscale (stats)")
text(mds1[,1], mds1[,2], labels(distancia), cex = 0.6, xpd = TRUE)
abline(h=0,lty=2)
abline(v=0,lty=2)

plot(m[,1], m[,2])

qplot(mds1[,1], mds1[,2], xlab = "Género 1", ylab = "Género 2")
text(mds1[,1], mds1[,2], labels(distancia), cex = 0.9, xpd = TRUE)




#############
## METODO 2
#############

#Dividimos la matriz entre la suma.
m.prop <- m/sum(m)

#Calculamos la suma de las filas y de las columnas
m.total_fila <- apply(m.prop, 1, sum)
m.total_columna <- apply(m.prop, 2, sum)

#Se calculan las matrices E de la matriz de fila y columna que se usará después.
diag.fila <- diag(1/sqrt(m.total_fila))
diag.columna <- diag(1/sqrt(m.total_columna))


#Calculamos la suma de las filas y de las columnas
m.total_fila <- as.matrix(m.total_fila)
m.total_columna <- as.matrix(m.total_columna)

#Calculamos la matriz E
E <- diag.fila%*%(m.prop-m.total_fila%*%t(m.total_columna))%*%diag.columna

#Calculamos la desviación
m.sva <- svd(E)

#Se calcula raizlambda
raizlambda <- m.sva$d
U <- m.sva$u[,1:2]
V <- m.sva$v[,1:2]

#Aplicamos el modelo y la fórmula
U[,1] <- raizlambda[1]*U[,1]/sqrt(m.total_fila)
U[,2] <- raizlambda[2]*U[,2]/sqrt(m.total_fila)
V[,1] <- raizlambda[1]*V[,1]/sqrt(m.total_columna)
V[,2] <- raizlambda[2]*V[,2]/sqrt(m.total_columna)

#Se calcula la inercia
inercia <- sum(raizlambda[raizlambda > 0]*raizlambda[raizlambda > 0])

pc1 <- (raizlambda[1] * raizlambda[1]/inercia)*100
pc2 <- (raizlambda[2] * raizlambda[2]/inercia)*100.0

#Pintamos los datos
options(digits=5)
plot(U[,1], U[,2], type = "n",
      xlab=paste("Género 1 % inercia=", format(pc1)),
      ylab=paste("Género 2 % inercia=", format(pc2)),
      axes=TRUE
     )
text(U[,1],U[,2], labels=c(dimnames(m)[[1]]), cex = 0.6, xpd = TRUE)
abline(h=0,lty=2)
abline(v=0,lty=2)

plot(U[,1], U[,2], type = "n",
     xlab=paste("Género 1 % inercia=", format(pc1)),
     ylab=paste("Género 2 % inercia=", format(pc2)),
     axes=TRUE
)
text(U[,1],U[,2], labels=c(dimnames(m)[[2]]), cex = 0.6, xpd = TRUE)
abline(h=0,lty=2)
abline(v=0,lty=2)


T<- rbind(U, V)
plot(T[,1], T[,2], type = "n",
     xlab=paste("Género 1 % inercia=", format(pc1)),
     ylab=paste("Género 2 % inercia=", format(pc2)),
     axes=TRUE
)
text(T[,1],T[,2], labels=c(dimnames(m)[[2]], dimnames(m)[[2]]), cex = 0.6, xpd = TRUE)
abline(h=0,lty=2)
abline(v=0,lty=2)

library(ca)

summary(ca(m))

######################################################################
## Código para probar un único fichero de forma unitaria. NO VALIDO. 
######################################################################

fichero <- "MatrATC5el_norepe_DiabN6120X_11.mat"
pathCompleto <- file.path(origenMat, "2011", fichero)
anyo <- "2011"

#Cargamos el fichero
mat <- readMat(pathCompleto)

#Obtenemos las variables directas.

#Obtenemos el patrón para la ejecución dinámica.
matriz <- substr(fichero, 1, nchar(fichero) - 4)           # quitamos el .mat
matriz <- gsub("[_]", ".", matriz)                        # cambiamos _ por .

#"Id"
objeto <- paste("Id <- rapply(mat$", matriz, "[1], c)")    # evaluamos la expresión
eval( parse (text=objeto) )

#"Sexo"
objeto <- paste("Sexo <- rapply(mat$", matriz, "[2], c)")  # evaluamos la expresión
eval( parse (text=objeto) )

#"Edad"
objeto <- paste("Edad <- rapply(mat$", matriz, "[3], c)")  # evaluamos la expresión
eval( parse (text=objeto) )

#Obtenemos las variables derivadas
RangoEdad <- rep(-1, length(Id))
Anyo <- rep( as.numeric(anyo), length(Id) )

#Obtenemos el CRG. 
matriz2 <- "MatrATC5el.norepe.DiabN5424X.11"
pos <- regexpr("N[1-9]+X", matriz)
CRG <- substr(matriz, pos, pos+3)
CRGs <- rep(CRG, length(Id))

#Obtenemos la matriz de ATCs.
objeto <- paste("ATCs <- unlist(mat$", matriz, "[4])")     # evaluamos la expresión
eval( parse (text=objeto) )
dim(ATCs) <- c( length(Id), 746)                            # convertimos en matriz

#Concatenamos todos los datos.
dato <- cbind(Id, Sexo, Edad, RangoEdad, Anyo, CRGs, ATCs)



#anyo <- read.csv(file.path(origenCSV,f), header = FALSE, )
#csv <- read.csv(file.path(origenCSV,ficheros[1]))



#matriz para calcular esto.

total <- cbind(c(), c())
for ( i in 1:nrow(csv2)) {
  diabetes <- 0
  noDiabetes <- 0
  
  for (j in 7:400 ) {
    if ( colnames(csv2)[j] %in% ATCsDiabeticos) {
      diabetes <- diabetes + csv2[i,j]
    }
    else {
      noDiabetes <- noDiabetes + csv2[i,j]
    }
  }
  
  total <- rbind(total, c(diabetes, noDiabetes))
}
colnames(total) <- c("Diabetes", "No Diabetes")
rownames(total) <- csv2[,"Id"]
table(total[,1])

rm(i)
rm(j)




##
# Histograma
##
qplot(csv2$nivel, geom="histogram", fill = csv2$RangoEdad,
      xlab="Edad", ylab = "Número Pacientes", 
      main = "Distribución de Pacientes por CRG y Rango de Edad [Q]")

df <- sqldf("select Genero, RangoEdad, nivel, count(*) as Num from csv2 group by Genero, RangoEdad, nivel")

##
# Jitter
##
#Juntos
qplot(csv2$RangoEdad, csv2$nivel, geom=c("jitter"), colour = csv2$Genero,
      xlab="Edad", ylab = "Nivel de CRG",
      main = "Distribución de Pacientes por Rango de Edad [K], Nivel CRG y Género")

csvG1 <- csv2[csv2$Genero == 1, ]
csvG2 <- csv2[csv2$Genero == 2, ]

#Mujeres
qplot(csvG1$RangoEdad, csvG1$nivel, geom=I("jitter"), colour = I("red"),
      xlab="Rango de Edad", ylab = "Nivel de CRG",
      main = "Distribución de Pacie ntes de Género 1por Rango de Edad [K], Nivel CRG.")

#Hombres
qplot(csvG2$RangoEdad, csvG2$nivel, geom=I("jitter"), colour = I("blue"),
      xlab="Rango de Edad", ylab = "Nivel de CRG",
      main = "Distribución de Pacientes de Género 2 por Rango de Edad [Q], Nivel CRG.")


rm(df)
rm(csvG1)
rm(csvG2)

##
# Puntos
##
df <- sqldf("select Genero, RangoEdad, nivel, count(*) as Num from csv2 group by Genero, RangoEdad, nivel")

#Mujeres
qplot(df[df$Genero == 1, "RangoEdad"], df[df$Genero == 1, "nivel"], geom=c("point"), 
      xlab="Rango de Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes de Género 1 por Rango de Edad, Nivel CRG.",
      colour = I("red"), size=df[df$Genero == 1, "Num"]
)

#Hombres
qplot(df[df$Genero == 2, "RangoEdad"], df[df$Genero == 2, "nivel"], geom=c("point"), 
      xlab="Rango de Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes de Género 2 por Rango de Edad, Nivel CRG.",
      colour = I("blue"), size=df[df$Genero == 2, "Num"]
)

rm(df)

