
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
ficheroFiltro <- file.path(destinoCSV, "ID_CS(NA).csv")

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

ficheros <- c(ficheroDestino, ficheroFiltro)
csv <- procesaCSV(ficheros)

#Sacar datos del 2011
#estadisticos(2011,ficheroTomanA10Destino, ficheroNoTomanA10Destino)

#Sacar datos del 2012
#estadisticos(2012,ficheroTomanA10Destino, ficheroNoTomanA10Destino)

#Liberamos sobre todo para liberar el espacio del environment dentro del RStudio.
rm(ficheros)
rm(ficheroDestino)
rm(destinoCSV)
rm(origenData)
rm(origenMat)
rm(ficheroTomanA10Destino)
rm(ficheroNoTomanA10Destino)

#d <- csv

#estadisticosId(d$Id)
#estadisticosSexo(d$Sexo)
#estadisticosEdad(d)
#estadisticosNivelCRG(d$nivel)
#revisaDispensacionATCsDiabeticos(d, ficheroTomanA10Destino, ficheroNoTomanA10Destino)
#ejecutaComparativa(d)
#ejecutaRangoEdad(d)

rm(estadisticosATCsVacios)
rm(estadisticosEdad)
rm(estadisticosId)
rm(estadisticosNivelCRG)
rm(estadisticosPosATCsVacios)
rm(estadisticosSexo)
rm(modificaDatos)
rm(procesaCSV)
rm(procesaFicheroFiltro)
rm(obtieneRangoEdad)
rm(ejecutaComparativa)
rm(ejecutaRangoEdad)
rm(ejecutaComparativaRangoEdad)
rm(estadisticos)
rm(revisaDispensacionATCsDiabeticos)
rm(imprime)
rm(cuentaMedicamentos)
rm(cuentaCRGAño)


d11 <- subset(csv, csv$Anyo == 2011)
d12 <- subset(csv, csv$Anyo == 2012)

#Vemos los pacientes que están en 2011 y en 2012.
Si_2011_Si_2012 <- d11[d11$Id %in% d12$Id, c("Id","Genero", "Edad", "CRG")]
n <- nrow(Si_2011_Si_2012)
Si_2011_Si_2012 <- cbind(Si_2011_Si_2012, rep(0, n), rep(0, n), rep(0, n), rep(0, n))
colnames(Si_2011_Si_2012) <- c("Id", "Sexo_11", "Edad_11", "CRG_11", "Id_12", "Sexo_12", "Edad_12", "CRG_12")

for (i in 1:nrow(Si_2011_Si_2012) ) {
  Si_2011_Si_2012[i, c("Id_12", "Sexo_12", "Edad_12", "CRG_12")] <- d12[d12$Id == Si_2011_Si_2012[i,"Id"], c("Id", "Sexo", "Edad", "CRG")]
}


#Gráfica Distribución por Año.
Anyo <- factor(csv$Anyo)
cuentas <- suppressMessages(sqldf("select Anyo, count(*) as total from csv group by Anyo"))
plot(Anyo, xlab="Año", ylab="Número de pacientes", ylim=c(0, 12000), col=colorAnyo)
title(main="Gráfica 1.- Distribución datos por Año", cex.main=0.7)
text(1, 11500, labels=c(cuentas[1,2]), adj=0.5, font=4, cex=0.9)
text(2, 11500, labels=c(cuentas[2,2]), adj=0.5, font=4, cex=0.9)
abline(h=n, lwd=3, col="red")
text(1, n+300, paste(n, " pacientes coincidentes entre ambos años"), col="red")

#Gráfica Distribución por CRG de nuevos pacientes. 
No_2011_Si_2012 <- d12[!d12$Id %in% d11$Id, c("Id","Genero", "Edad", "CRG")]
No_2011_Si_2012$Genero <- factor(No_2011_Si_2012$Genero)
plot(table(No_2011_Si_2012$CRG), 
     xlab = "CRG-base", ylab="Número de Pacientes", 
     cex.axis = 0.7, col=colorCRG, type="l")
title(main=paste("Distribución por CRG de los 'nuevos' pacientes en 2012.",
                 "Total pacientes nuevos: ", nrow(No_2011_Si_2012)), cex.main=0.7)

Si_2011_No_2012 <- d11[!d11$Id %in% d12$Id, c("Id","Genero", "Edad", "CRG")]
Si_2011_No_2012$Genero <- factor(Si_2011_No_2012$Genero)
barplot(table(Si_2011_No_2012$CRG), 
        xlab = "CRG-base", ylab="Número de Pacientes", 
        cex.axis = 0.7, col=colorCRG)
title(main=paste("Distribución por CRG de los pacientes que desaparecen en 2012.",
                 "Total pacientes que no están: ", nrow(Si_2011_No_2012)), cex.main=0.7)

rm(i)
rm(d11)
rm(d12)


#Sacamos las gráficas de distribución por Edad, coloreando el Género.  
cuentas <- sqldf("select Anyo, Edad, Sexo, count(*) as total from csv group by Anyo, Edad, Sexo")

colorGenero <- c("red", "blue")
par(mfrow=c(2,1), mar=c(3,3,2,2))
for (anyo in unique(cuentas$Anyo)) {
  d <- subset(cuentas, cuentas$Anyo==anyo)
  plot(subset(d, Sexo==1)$Edad, subset(d, Sexo==1)$total, 
       xlab = "Edad", ylab="Número de Pacientes", xaxt="n",
       cex.axis = 0.7, type="l", col=colorGenero[1])
  lines(subset(d, Sexo==2)$Edad, subset(d, Sexo==2)$total, col=colorGenero[2])
  title(main=paste("Distribución de Edad en ", anyo, sep=""), cex.main=0.7)
  axis(1,  at = unique(d$Edad), labels=unique(d$Edad), cex.axis=0.7)
  #legend("topright", legend = c("Género 1", "Género 2"), fill=colorGenero)
  i<- i + 1
}

kt <- ks.test(d11$Edad, "pnorm", mean=mean(d11$Edad), sd=sd(d11$Edad)) 
p <- rnorm(1000)
kt <- ks.test(p, "pnorm") 
summary(kt)
kt$p.value
kt$statistic

kt <- lillie.test(d11$Edad)
kt$statistic
kt$p.value

Edad_Hombre <- subset(d11$Edad, d11$Genero==1)
Edad_Mujer <- subset(d11$Edad, d11$Genero==2)

kt <- wilcox.test(Edad_Mujer, Edad_Hombre)
kt$statistic
kt$p.value

for (anyo in unique(csv$Anyo)) {
  Edad <- subset(csv$Edad, Anyo == anyo)
  
  suppressWarnings(kt <- ks.test(Edad, "pnorm"))
  cat(paste("\n\n--> Test de Kolmogorov-Smirnov --> Año: ", anyo, 
            "; Estadístico: ", kt$statistic, "; p-value: ", kt$p.value, sep=""))
  
  kt <- lillie.test(Edad)
  cat(paste("\n\n--> Test de Lillieforst --> Año: ", anyo, 
            "; Estadístico: ", kt$statistic, "; p-value: ", kt$p.value, sep=""))
}


for (anyo in unique(csv$Anyo)) {
  for (i in unique(csv$nivel)) {
    mu1 <- subset(csv$Edad, csv$Anyo == anyo & csv$nivel==i & csv$Genero == 1)
    mu2 <- subset(csv$Edad, csv$Anyo == anyo & csv$nivel==i & csv$Genero == 2)
    if (length(mu1) > 4 & length(mu2) > 4 ) {
      prueba <- wilcox.test(mu1, mu2)
      cat(red(paste("\n\n\t\tCRG-base = ", i, ", p-Value: ", round(prueba$p.value, 5), 
                    ", Género 1: ", length(mu1), ", Género 2: ", length(mu2), sep="")))
    }
  }
}

rm(anyo)
for (anyo in unique(csv$Anyo)) {
  Edad <- subset(csv$Edad, Anyo == anyo)
  cat(paste("\n\nAño:", anyo, "; Edad:", length(Edad), "\n\n", sep=""))
  
  #suppressWarnings(kt <- ks.test(Edad, "pnorm"))
  #cat(paste("\n\n--> Test de Kolmogorov-Smirnov --> Año: ", anyo, 
  #          "; Estadístico: ", kt$statistic, "; p-value: ", kt$p.value, sep=""))
  
  #kt <- lillie.test(Edad)
  #cat(paste("\n\n--> Test de Lillieforst --> Año: ", anyo, 
  #          "; Estadístico: ", kt$statistic, "; p-value: ", kt$p.value, sep=""))
}

for (anyo in unique(csv$Anyo)) {
  cat(paste("\n\n Año ", anyo, "\n\n", sep=""))
  csv2 <- subset(csv, csv$Anyo == anyo)
  g <- ggplot(csv2, aes(x=nivel, y=Edad, fill=Genero)) + geom_boxplot(notch=TRUE) + 
    scale_fill_manual(values=colorGenero) +
    theme(axis.text.x = element_text(size = 7, colour = colorCRG)) + 
    theme(title = element_text(size = 7, colour = "black")) + 
    labs(x="CRG-base", y="Edad",
         title=paste("Distribución de pacientes según CRG-base y Sexo en ", anyo, sep=""))
  suppressMessages(print(g))
}
rm(anyo)
rm(csv2)
rm(g)

df <- csv[csv$Anyo == 2012 & csv$totalATCDiabeticos == 0, c("Id", "Anyo")]
write.csv("Id_Sin_ATC_Diabeticos.csv", sep=";")

df2 <- readMat("/Users/zzddfge/Desktop/Compartida/Proyecto_Master/data/csv/noTomanA10.mat")

revisaDispensacionATCsDiabeticos(csv, ficheroTomanA10Destino, ficheroNoTomanA10Destino)

#TODO en esta parte:
#  --> relación de existencia / probabilidad de ocurrencia.
#  --> hacer zoom sobre los grupos con más número: 6144 y 5424
#  --> cálculo del PCA. Cuando se tengan los PCAs, seguramente haya que evaluar rango de edad otra vez.
#  --> revisar si hay significancia estadística en la diferencia de grupos hombres y mujeres.

#ALTERN -1 ) Para cada CRG-base, obtener el número de ocurrencias de cada familia de ATC por 
#individuo durante 1 año y sumar el número de ocurrencias de familias de ATC para los individuos 
#asociados al mismo CRG. Transformar el resultado de la suma en una medida de probabilidad 
#(normalizar de modo que la suma de todos sea 1).

######################################################################
#### EJECUCION SOLO PARA HACER PCAs
## Código PCAs. 
####
######################################################################

#Tenemos que limpar las columnas que son ceros para 2011 y para 2012. 
source("PCAs.R")

d11 <- eliminaATCsVacios(csv, 2011, 7:(ncol(csv)-N))
d12 <- eliminaATCsVacios(csv, 2012, 7:(ncol(csv)-N))

source("clusterizacion.R")

calculaPCA_nivel_2(d11, 7:(ncol(d11)-N))

cat("###Análisis de Existencia ")
m <- calculaBurbujaATC(d11, m_existeFamilia)


csv2 <- subset(csv, csv$Anyo == anyo)
for (crg in unique(csv2$CRG)) {
  cat(paste("CRG-base : ", crg, "\n\n", sep=""))
  
  plot(csv2[csv2$CRG == crg, "CRG"])
  
}


rm(calculaPCA_nivel)
rm(cuentaFamilias)
rm(sumaFamilias)
rm(eliminaATCsVacios)
rm(obtieneExistencia)
rm(reduceMatrizATC)
rm(standMatrix)
rm(standVector)
rm(calculaBurbujaATC)
rm(calculaPCA_nivel1)
rm(calculaPCA_nivel2)


################################
########## Calcula Matrices
######Reducción de dimensiones 


d <- d11

#2012
ATC.nivel2 <- unique(substr(colnames(d12)[7:(ncol(d12)-N)], 1, 3))
m <- matrix(nrow=nrow(d12), ncol=length(ATC.nivel2))
d <- d12

#m.scaled <- scale(m, center=TRUE, scale=TRUE)

###### PCAs sobre el total. 



#Restamos 4 porque son las columnas que se han añadido hasta este punto por detrás de los ATCs.
m11 <- as.matrix(d11[, 7:(length(colnames(d11)) - 4)])
m12 <- as.matrix(d11[, 7:(length(colnames(d12)) - 4)])


m11.scaled <- standMatrix(m11)
m12.scaled <- standMatrix(m12)

pca11 <- calculaPCA(m11.scaled, m11)
pca12 <- calculaPCA(m12)

################################
########## Ejecución de Matrices
######Reducción de dimensiones 

matriz <- m

# Paso 1: Ejecución de los principales componentes.
pca <- prcomp(matriz, center=TRUE, scale=TRUE)

#Código para comprobar que los componentes principales provienen del SVD. 
#svd1 <- svd(scale(m))
#par(mfrow = c(1, 1))
#plot(pca$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
#abline(c(0, 1))

# Paso 2: Revisión de la agrupación de los PC
round(unclass(pca$rotation)[, 1:6], 2)

rotacion <- unclass(pca$rotation)


n <- 20 #número de PCs
m <- 20 #número de ATCs que queremos analizar. 
resultado <- as.data.frame(matrix(nrow=0, ncol=3))
df <- as.data.frame(matrix(nrow=m, ncol=3))
colnames(df) <- c("PC", "ATC", "EXPLICACION")
colnames(resultado) <- c("PC", "ATC", "EXPLICACION")

#Vamos a hacer una iteración para quedarnos con el TOP 50 de los ATCs.
for ( i in 1:ncol(rotacion) ) {
  #Calculamos el % de lo que aporta cada ATC al PCA. 
  pca.por <- round(rotacion[, i] / sum(abs(rotacion[, i])),4)
  
  #Ordenamos de mayor aportación a menor.
  pca.por.ord <- head(sort(pca.por, decreasing = TRUE), m)
  pca.por.name <- names(pca.por.ord)[1:m]
  pca.por.ord <- as.numeric(pca.por.ord)
  
  #Añadimos las filas
  #df[, 1] <- rep(colnames(rotacion)[i], length(pca.por.name))
  df[, 1] <- rep(i, length(pca.por.name))
  df[, 2] <- pca.por.name
  df[, 3] <- pca.por.ord
  resultado <- rbind(resultado, df)
}

sum(resultado[resultado$PC %in% paste("PC",1:n,sep=""), 3])
  
unique(resultado[resultado$PC %in% 1:n, 2])

  resultado <- rbind(resultado, m)

  #grado de explicación de PC vs ATC
  #r <- resultado[resultado$PC %in% paste("PC",1:n,sep=""), ]
  r <- resultado[resultado$PC %in% 1:n, ]
  r[,1] <- factor(r[,1])
  r[,2] <- factor(r[,2])
  qplot(r$PC, r$ATC, colour=r$PC, geom=c("point"), size=r$EXPLICACION)
  qplot(r$ATC, r$PC, colour=r$PC, geom=c("point"), size=r$EXPLICACION) + 
    theme(axis.text.x = element_text(size = 8, colour = "red", angle = 45))
  
  rr <- r[r$EXPLICACION > 0.03,]
  qplot(rr$ATC, rr$PC, colour=rr$PC, geom=c("point"), size=rr$EXPLICACION) + 
    theme(axis.text.x = element_text(size = 8, colour = "red", angle = 45))
  
  qplot(r[r$PC==1, "ATC"], r[r$PC==1, "EXPLICACION"], size=r[r$PC==1, "EXPLICACION"])
  qplot(r[r$PC==2, "ATC"], r[r$PC==2, "EXPLICACION"], size=r[r$PC==2, "EXPLICACION"])
  qplot(r[r$PC==3, "ATC"], r[r$PC==3, "EXPLICACION"], size=r[r$PC==3, "EXPLICACION"])
  qplot(r[r$PC==4, "ATC"], r[r$PC==4, "EXPLICACION"], size=r[r$PC==4, "EXPLICACION"])
  qplot(r[r$PC==5, "ATC"], r[r$PC==5, "EXPLICACION"], size=r[r$PC==5, "EXPLICACION"])
  qplot(r[r$PC==6, "ATC"], r[r$PC==6, "EXPLICACION"], size=r[r$PC==6, "EXPLICACION"])
  qplot(r[r$PC==7, "ATC"], r[r$PC==7, "EXPLICACION"], size=r[r$PC==7, "EXPLICACION"])
  qplot(r[r$PC==8, "ATC"], r[r$PC==8, "EXPLICACION"], size=r[r$PC==8, "EXPLICACION"])
  qplot(r[r$PC==9, "ATC"], r[r$PC==9, "EXPLICACION"], size=r[r$PC==9, "EXPLICACION"])
  qplot(r[r$PC==10, "ATC"], r[r$PC==10, "EXPLICACION"], size=r[r$PC==10, "EXPLICACION"])
  qplot(r[r$PC==11, "ATC"], r[r$PC==11, "EXPLICACION"], size=r[r$PC==11, "EXPLICACION"])
  qplot(r[r$PC==12, "ATC"], r[r$PC==12, "EXPLICACION"], size=r[r$PC==12, "EXPLICACION"])
  qplot(r[r$PC==13, "ATC"], r[r$PC==13, "EXPLICACION"], size=r[r$PC==13, "EXPLICACION"])
  
pca.por <- round(rotacion[, 2] / sum(abs(rotacion[, 2])),4)


dff <- sqldf("SELECT PC, ATC, sum(EXPLICACION) from resultado group by PC, ATC")
dff[,1] <- factor(dff[, 1])
qplot(dff$PC, dff$`sum(EXPLICACION)`, geom=c("bar"), fill=dff$ATC, stat="identity")

#dfff<- dff[dff$PC %in% paste("PC",1:n,sep=""),]
dfff<- dff[dff$PC %in% 1:n,]
dfff[,1] <- factor(dfff[, 1])

qplot(dfff$PC, dfff$`sum(EXPLICACION)`, geom=c("bar"), fill=dfff$ATC, stat="identity")

# Paso 3: Revisión de las varianzas de los PC.
# a) Eigenvalues
eig <- (pca$sdev)^2

# b) Varianza en %
variance <- eig*100/sum(eig)

# c) Varianza acumulativa.
cumvar <- cumsum(variance)

eig.active <- data.frame(eig = eig, variance = variance,
                         cumvariance = cumvar)
head(eig.active, 200)

summary(pca)

# d) Visualizamos gráficamente
#   % de Varianza.
barplot(eig.active[, 3], names.arg=1:nrow(eig.active), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de Varianza",
        col ="steelblue")
# Add connected line segments to the plot
abline(h=80,lty=6)

#
barplot(eig.active[, 2], names.arg=1:nrow(eig.active), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de Varianza",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.active), 
      eig.active[, 2], 
      type="b", pch=10, col = "red")

#   Eugenvalue.
barplot(eig.active[, 1], names.arg=1:nrow(eig.active), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Eigenvalues",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.active), 
      eig.active[, 1], 
      type="b", pch=10, col = "red")
abline(h=1,lty=6)


#PCA






#Proyecto

#Obtenemos la matriz de los ATCs.
m <- as.matrix(csv[,7:400])
m11 <- as.matrix(csv[csv$Anyo==2011,7:400])
m12 <- as.matrix(csv[csv$Anyo==2012,7:400])

#colnames(m) <- colnames(csv)[7:400]
#image(1:394, 1:9392, t(m)[, nrow(m):1])
 
#Tenemos que calcular la formula: X=UDV
# -> scale --> A cada elemento de una matriz, le resta la media y la divide por su varianza. Es decir, la normaliza.
# -> svd es una función que desglosa una matriz en sus valores singulares.
#   -> U: Vectores singulares Izquierdos. Matriz ortogonal.
#   -> V: Vectores singulares Derechos. Matriz ortogonal.
#   -> D: Matriz diagonal de valores singulares.
#
# PCA es una amplicación de SVD. Los PCAs son los vectores singulares derechos(V). Se necesita
# normalizar la matriz, que se hace con la función scale.
svd1 <- svd(scale(m))

par(mfrow = c(1, 3))
image(t(m)[, nrow(m):1], main = "Original Data")
plot(svd1$u[, 1], 9392:1, , ylab = "Row", xlab = "First left singular vector",
       +     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained",
           pch = 19)

#PCAs 

# Paso 1: Ejecución de los principales componentes.
pca <- prcomp(m, scale = TRUE)
pca11 <- prcomp(m11, scale = TRUE)

par(mfrow = c(1, 1))
plot(pca$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", ylab = "Right Singular Vector 1")
abline(c(0, 1))

# Paso 2: Revisión de la agrupación de los PC
round(unclass(pca$rotation)[, 1:6], 2)

# Paso 3: Revisión de las varianzas de los PC.
# a) Eigenvalues
eig <- (pca$sdev)^2

# b) Varianza en %
variance <- eig*100/sum(eig)

# c) Varianza acumulativa.
cumvar <- cumsum(variance)

eig.active <- data.frame(eig = eig, variance = variance,
                                    cumvariance = cumvar)
head(eig.active)

summary(pca)

# d) Visualizamos gráficamente
#   % de Varianza.
barplot(eig.active[, 2], names.arg=1:nrow(eig.active), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de Varianza",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.active), 
      eig.active[, 2], 
      type="b", pch=10, col = "red")

#   Eugenvalue.
barplot(eig.active[, 1], names.arg=1:nrow(eig.active), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Eigenvalues",
        col ="steelblue")
# Add connected line segments to the plot
lines(x = 1:nrow(eig.active), 
      eig.active[, 1], 
      type="b", pch=10, col = "red")
abline(h=1,lty=6)

#An eigenvalue > 1 indicates that PCs account for more variance than accounted by one of the 
# original variables in standardized data. This is commonly used as a cutoff point for which 
# PCs are retained.
#You can also limit the number of component to that number that accounts for a certain fraction 
# of the total variance. For example, if you are satisfied with 80% of the total variance 
# explained then use the number of components to achieve that.













#Método 2

# Paso 1 -> Preparar los datos (escalando y centrando)
m.scaled <- scale(m, center = TRUE, scale = TRUE)

# Paso 2 -> Calcular la matriz de correspondencias
m.cor <- round(cor(m.scaled), 4)

# Paso 3 -> Calcular los vectores y valores eigen de la matriz de correlación.
#  -> Valores Eigen: Números de la diagonal de la matriz de covarianza diagonalizada.
#  -> Vectores Eigen: Dirección de los nuevos ejes rotados.
# 
m.cor.eig <- eigen(m.cor)
round(m.cor.eig$values,2)

# Paso 4 -> Calcular el número de vectores de Eigen. Supongamos 12.
round(m.cor.eig$values[1:12],2)

# Paso 5 -> Ejecutar el nuevo juego de datos.
#  Transponer los vectores
eigenvectors.t <- t(m.cor.eig$vectors)

#  Transponer el dato ajustado
m.scaled.t <- t(m.scaled)

#  Cálculo del nuevo dataset.
m.new <- eigenvectors.t %*% m.scaled.t

# Transpose new data ad rename columns
m.new <- t(m.new)
colnames(m.new) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")


#Ejemplo
set.seed(12345)
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])



#TODO a Futuro
# --> Incorporar 2012. Hay que sumar 1 a la Edad de los pacientes que están en 2011.
# --> Analizar evolución de 2011 a 2012. Hay que ver los que cambian de CRG y habría que ver 
#      evolución de medicamentos. 
# --> Intersección de pacientes.


cat("\n\n##Edad - Datos descriptivos\n\n")
cat("Se muestra gráfica comparativa entre años 2011 y 2012 en la distribución por Edad\n\n")
cat("Gráfica 2.- Distribución de Edad por Año\n\n")
par(mfrow = c(length(unique(csv$Anyo)), 1), mar = c(4, 4, 2, 1))
i <- 1
for (anyo in unique(csv$Anyo)) {
  plot(table(subset(csv$Edad, csv$Anyo==anyo)), 
       main=paste("Distribución de Edad en ", anyo, sep=""), 
       xlab = "Edad", ylab="Número de Pacientes", 
       cex.axis = 0.7, type="h", col=colorAnyo[i])
  i<- i + 1
}
rm(i)
rm(Anyo)



####EVOLUCION
#Generamos el par old-new de los CRG.
oldnew_distintos <- Si_2011_Si_2012[Si_2011_Si_2012$CRG_11 != Si_2011_Si_2012$CRG_12, c("CRG_11", "CRG_12")]
oldnew_distintos[,"CRG_11"] <- as.character(oldnew_distintos[,"CRG_11"])
oldnew_distintos[,"CRG_12"] <- as.character(oldnew_distintos[,"CRG_12"])
l <- melt(table(oldnew_distintos))
l[,"CRG_12"] <- as.factor(l[,"CRG_12"])


unico <- unique(oldnew_distintos[,"CRG_11"])
for (i in 1:length(unico)) {
  p <- ggplot(l[l$CRG_11==unico[i],], aes(x=CRG_12, y=value, label=value)) +
    geom_bar(stat="identity", fill=I("blue")) +
    geom_text(size=4, color="red", hjust=0, vjust=-1) +
    labs(x="Nuevo CRG-base", y="Número de pacientes", 
         title=paste("Pacientes que han evolucionado desde el CRG-base", unico[i]))
#  dev.print(file=paste(unico[i],".png",sep=""), device=png, width=800)
#  dev.off()
#  ggsave(paste(unico[i],".png",sep=""), device = png, limitsize=FALSE)
  png(paste(unico[i],".png",sep=""))
  print(p)
  dev.off()
}

rm(Si_2011_Si_2012)
rm(oldnew_distintos)

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

