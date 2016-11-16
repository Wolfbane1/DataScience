
suppressMessages(library(foreach))
suppressMessages(library(doParallel))
library(e1071)

source("PCAs.R")


###########################
###########################
### FUNCIONES GENERALES
###########################
###########################

###############################################
### FUNCION: pasaAExistencia -> Dado un conjunto de datos (sanos o diabeticos) convierte la matriz en Existencia
#              y le concatena las variables de Edad y de Sexo. Además, luego añade la variable objetivo.
### PARAMETROS:
# dx --> Matriz de toma de existencia.
# tipo --> Variable que indica quién va a ser el 1. En este caso, el diabético será el caso de éxito.
#
pasaAExistencia <- function(dx, tipo = "DIABETICO") {
  #Posiciones de la lista de ATCs
  listaAtcs <- 7:(ncol(dx) - N)
  
  #Convertimos en Existencia
  m <- matrix(nrow=nrow(dx), ncol=length(listaAtcs))
  j <- 1
  for (i in listaAtcs) {
    m[, j] <- ifelse(dx[,i] > 0, 1, 0)
    j <- j + 1
  }
  m <- as.data.frame(m)
  colnames(m) <- colnames(dx)[listaAtcs]
  rownames(m) <- dx$Id
  
  #Añadimos el resto de campos necesarios para la regresión.
  m$Edad <- dx$Edad
  m$Sexo <- dx$Sexo
  
  #Añadimos los campos de si es diabético
  m$Diabetico <- ifelse(tipo == "DIABETICO", 1, 0)
  
  rm(i)
  rm(j)
  rm(listaAtcs)
  
  return(m)
}

###############################################
### FUNCION: standVectorEspecial -> Normaliza una matriz con los datos de la media y de la desviación típica
#              que se han tenido que encontrar antes. Los dos parámetros deben tener las mismas columnas.
### PARAMETROS:
# x --> Matriz con elementos a normalizar.
# datos --> Tabla con dos elementos: Elemento 1 --> Media, Elemento2 --> Desviación Típica
#
standVectorEspecialFila <- function(x, datos) {
  (x-datos[1,])/datos[2,]
}

#standVectorEspecialColumna <- function(x, datos) {
#  j <- j+1
#  return ( (x-datos[1,j-1])/datos[2,j-1])
#}

#normaliza <- function(d, datos) {
#  #Creamos la matriz al principio
#  resultado <- c()
#  
#  for(i in 1:ncol(d)) {
#    a <- (d[,i]-datos[1,i])/datos[2,i]
#    resultado <- cbind(resultado, a)
#  }
#  
#  return(resultado)
#}

###############################################
### FUNCION: valor -> Para ser usada con un Apply, donde transforma columas en filas.
### PARAMETROS:
#
valor <- function(x) {
  x
}

###########################
###########################
### SANOS vs DIABETICOS
###########################
###########################

##Pasos previos que hay que implementar
# 1.- Tener una matriz de todos los pacientes diabéticos.
# 2.- Tener una matriz aleatoria de los pacientes sanos.
# 3.- Chequear que la media y la sd no sea muy distintas. 
# 4.- Eliminar los ATCs vacíos.
# 5.- Normalizar.

###############################################
### FUNCION: chequeaSampleSanos -> Dado una muestra de sanos y los propios sanos, pinta la distribución de la 
#              media de los sanos y de la media de la muestra, para asegurar que más o menos cuadra. 
### PARAMETROS:
# sanosn --> Matriz de sanos, normalizado en ocurrencia.
# sanosn_s --> Muestra de los sanos, normalizado en ocurrencia y con el tamaño de la matriz de los diabéticos.
#
chequeaSampleSanos <- function(sanosn, sanosn_s) {
  #Cálculo de la muestra de los sanos.
  media_sample_sanos <- apply(sanosn_s[,1:748], 2, mean)
  sd_sample_sanos <- apply(sanosn_s[,1:748], 2, sd)
  
  #Cálculo de los sanos.
  media_sanos <- apply(sanosn[,1:748], 2, mean)
  sd_sanos <- apply(sanosn[,1:748], 2, sd)

  cat("\n\n<b>Gráfica Chequeo Media Sanos vs Sample</b>\n\n")
  par(mfrow=c(1,1))
  #png(filename = "completo_sample", 
  #    width = 1024, height = 768, 
  #    units = "px", bg = "transparent")
  #Veamos la media de los ATCs
  plot(media_sanos[1:746], type ="l", cex.axis = 0.7, xaxt="n", cex.main=0.7,
       main="Comparativa de Perfiles Sanos con Sample", ylab="Media de ocurrencia de ATCs", xlab="ATCs")
  points(media_sample_sanos[1:746], type="l", col="red")
  label=c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
  pos=c(92,128,207,273,314,335,415,452,484,563,586,629,663,748)
  axis(1, at=pos, labels= label, cex.axis = 0.5)
  #legend("topright", legend = c("Completo", "Muestra"), fill=c("black", "red"))
  #dev.off()
  
  cat("\n\n<b>Gráfica Chequeo SD Sanos vs Sample</b>\n\n")
  plot(sd_sanos[1:746], type ="l", cex.axis = 0.7, xaxt="n", cex.main=0.7,
       main="Comparativa de Perfiles Sanos con Sample", ylab="SD de ocurrencia de ATCs", xlab="ATCs")
  points(sd_sample_sanos[1:746], type="l", col="red")
  label=c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
  pos=c(92,128,207,273,314,335,415,452,484,563,586,629,663,748)
  axis(1, at=pos, labels= label, cex.axis = 0.5)
  cat("\n\n")
  
  rm(media_sanos)
  rm(sd_sanos)
  rm(media_sample_sanos)
  rm(sd_sample_sanos)
  rm(label)
  rm(pos)
}

###############################################
### FUNCION: ejecutaRegresion -> Dado un conjunto de datos que están ya normalizados, obtiene una muestra
#                                de esos conjuntos de datos y ejecuta una regresión logística. 
### PARAMETROS:
# d0 --> Data Frame con los pacientes normalizados para el 0 en ocurrencia con aquellos ATCs que no están a 0. 
# d1 --> Data Frame con los pacientes normalizados para el 1 en ocurrencia. También tiene aquellos ATCs que no están a 0.
# datos_pacientes --> Datos con la media de la población y la sd original.
# pacientes12 --> Matriz con los pacientes de 2012 para hacer el test. Debe tener las mismas columnas que las
#   variables diabn y sanosn, así como estar normalizada con los datos de pacientes11 calculados de forma previa.
# size --> Tamaño de la muestra
# realización --> Número de la realización
#
ejecutaRegresion <- function(d1, d0, pacientes12, size, realizacion=0) {
  #1.- Creamos la matriz al principio
  resultado <- matrix(nrow=0, ncol=7)
  colnames(resultado) <- c("Realizacion", "ATC", "pValue", "Media", "Sd", "Sensibilidad", "Especificidad")
  vectorLon <- 1:(length(d0)-1)
  
  #2.- Obtenemos los datos para 2011 y para 2012.
  m11 <- getMuestraPacientes(d1, d0, size)
  
  #3.- Obtenemos la media de la muestra para normalizar en 2012. 
  datos_pacientes <- rbind(apply(m11[,vectorLon], 2, mean), apply(m11[,vectorLon], 2, sd0))
  rownames(datos_pacientes) <- c("Media", "Sd")
  m11_n <- m11
  p12_n <- pacientes12
  
  #4.- Hay que normalizar tanto 2011 como 2012.
  #m11_n[,vectorLon] <- normaliza(m11[,vectorLon], datos_pacientes)
  #p12_n[,vectorLon] <- normaliza(pacientes12[,vectorLon], datos_pacientes)
  m11_n[,vectorLon] <- t(apply(m11[,vectorLon], 1, standVectorEspecialFila,  datos=datos_pacientes))
  p12_n[,vectorLon] <- t(apply(pacientes12[,vectorLon], 1, standVectorEspecialFila,  datos=datos_pacientes))
  
  #Preparamos la fórmula y ejecutamos la regresión.
  comando <- getCadenaRegresion(colnames(m11), "m11_n")
  suppressWarnings(eval( parse (text=comando) ))
  #r <- ifelse(glm$fitted.values <= 0.5, 0, 1)
  
  #Evaluamos el resultado y validamos con 2012.
  p <- suppressWarnings(predict.glm(glm, newdata=p12_n[,vectorLon], type="response"))
  
  #Calculamos la matriz de confución y la imprimimos en el resultado.
  r12 <- ifelse(p <= 0.5, 0, 1)
  matriz_confusion <- table(r12, p12_n$Diabetico)
  pintaSensibilidadEspecifidad(matriz_confusion, realizacion)
  
  #Quitamos la columna de diabético
  atcs <- colnames(m11[,c(-length(m11))])
  t <- round(summary(glm)$coef[,'Pr(>|t|)'], 6)
  x <- apply(datos_pacientes, 1, valor)
  sensibilidad <- round(matriz_confusion[2,2] / (matriz_confusion[2,2] + matriz_confusion[1,2]), 4)
  especificidad <- round(matriz_confusion[1,1] / (matriz_confusion[1,1] + matriz_confusion[2,1]), 4)
  resultado <- rbind(resultado, matrix(c(rep(realizacion, length(atcs)), 
                                       colnames(datos_pacientes), 
                                       t[atcs],
                                       x[,1],
                                       x[,2],
                                       rep(sensibilidad, length(atcs)),
                                       rep(especificidad, length(atcs))), 
                                       nrow=length(atcs), ncol=7))
  
  rm(comando)
  rm(matriz_confusion)
  rm(m11)
  rm(m11_n)
  rm(p12_n)
  rm(datos_pacientes)
  rm(atcs)
  rm(t)
  rm(r12)
  rm(p)
  rm(glm)
  rm(vectorLon)
  rm(x)
  rm(sensibilidad)
  rm(especificidad)
  
  return(resultado)
}

###############################################
### FUNCION: getMuestraPacientes -> Dado un conjunto de datos que están ya normalizados, obtiene una muestra
#                                de esos conjuntos de datos.
### PARAMETROS:
# diab --> Data Frame con los pacientes diabéticos.
# sanos --> Data Frame con los pacientes sanos.
# size --> Tamaño de la muestra.
#
getMuestraPacientes <- function(diabn, sanosn, size) {
  #Obtenemos n muestras de pacientes diabéticos y de sanos.
  d_s <- obtieneSampleMatrizRL(diabn, size)
  s_s <- obtieneSampleMatrizRL(sanosn, size)
  m <- rbind(d_s, s_s)
  rm(d_s)
  rm(s_s)
  
  return(m)
}

###############################################
### FUNCION: obtieneSampleMatrizRL -> Dado un conjunto de datos (sanos o diabeticos) que están ya 
#                 normalizados, obtiene una muestra de esos conjuntos de datos y ejecuta una regresión logística. 
### PARAMETROS:
# dx --> Matriz de toma de existencia.
# size --> Tamaño de la muestra.
#
obtieneSampleMatrizRL <- function(dx, size) {
  #Obtenemos la muestra de tamaño SIZE.
  Id <- rownames(dx)
  Id_s <- sample(Id, size=size, replace=FALSE)
  
  #Obtenemos los datos apropiados.
  res <- subset(dx, rownames(dx) %in% Id_s)  
  
  rm(Id)
  rm(Id_s)
  
  return(res)
}

###############################################
### FUNCION: getCadenaRegresion -> Dado todas las variables a tener en cuenta para la regresión, calculamos
#              de forma dinámica la generación de la llamada. Como la última variable es la objetivo, no la
#              podemos meter en la regresión.
### PARAMETROS:
# v --> resultado del colnames() de la matriz a usar para la regresión.
# nombreDS --> Nombre del dataset que tendrá los datos.
#
getCadenaRegresion <- function(v, nombreDS) {
  #Preparamos la fórmula. Hasta -2 porque el último elemento de la lista es la objetivo y el último 
  #elemento a contemplar no le queremos añadir el "+" detrás.
  f <- "Diabetico~"
  for (col in 1:(length(v)-2)) {
    f <- paste(f, v[col], "+", sep="")
  }
  f <- paste(f, v[length(v)-1], sep="")
  
  #Preparamos el comando.
  #comando <- paste("glm <- glm(", f, ", data=", nombreDS, ")", sep="")
  
  ##
  ##
  ##
  #comando <- paste("glm <- svm(", f, ", data =", nombreDS, ", kernel = ""radial"", nu=0.5, cost=1e5)")
  glm <- svm(formula(f), data=nombreDS, kernel="radial", nu=0.5, cost=1e1)
  
  rm(col)
  
  return(glm)
}

###############################################
### FUNCION: pintaSensibilidadEspecifidad -> Dado una matriz de confusión, calcula la Sensibilidad y la 
#             especificidad y la imprime por pantalla.
### PARAMETROS:
# m --> Matriz de confusión, 2x2
#
pintaSensibilidadEspecifidad <- function(m, realizacion) {
  #Sensibilidad = VP / (VP + FN)
  sensibilidad <- round(m[2,2] / (m[2,2] + m[1,2]), 4)
  especificidad <- round(m[1,1] / (m[1,1] + m[2,1]), 4)
  
  #matrizSensibilidad <- rbind(matrizSensibilidad, matrix(c(realizacion,sensibilidad, especificidad), 
  #∫                                                       nrow=1, ncol=3))
  
  #Imprimimos
  cat(paste("\n\n<b>Realización:", realizacion, "</b>\n\n"))
  pintaMC(m)
  cat(paste("\n\nSensibilidad -> ", sensibilidad, " ; Especificidad -> ", especificidad, "\n\n", sep=""))
  
  rm(sensibilidad)
  rm(especificidad)
}

######################################
######################################
######################################
### EVOLUCION EN CRG
######################################
######################################
######################################

###############################################
### FUNCION: getDatos2011PacientesEvolucion -> Dada una matriz con los datos de año especificadi y del siguiente
#             obtiene los Ids comunes y se devuelven los datos de para esos pacientes.
### PARAMETROS:
# diab --> Pacientes diabéticos 
# anyo --> Año del que devolver la información
#
getDatos2011PacientesEvolucion <- function(diab, anyo, vector) {
  Ids <- getIds2011CRGs2012(diab, vector)
  
  return(diab[diab$Anyo == anyo & diab$Id %in% Ids$Id, ])
}

###############################################
### FUNCION: ejecutaRegresionEvolucion -> Dado un conjunto de datos que están ya normalizados, se queda con el 80% para train
#              y usa el 20% para Test. 
### PARAMETROS:
# d0 --> Data Frame con los pacientes normalizados para el 0 en ocurrencia con aquellos ATCs que no están a 0. 
# d1 --> Data Frame con los pacientes normalizados para el 1 en ocurrencia. También tiene aquellos ATCs que no están a 0.
# porcentajeMuestra --> % de la muestra.
# realización --> Número de la realización
#
ejecutaRegresionEvolucion <- function(d1, d0, porcentajeMuestra, realizacion=0) {
  #1.- Creamos la matriz al principio
  resultado <- matrix(nrow=0, ncol=7)
  colnames(resultado) <- c("Realizacion", "ATC", "pValue", "Media", "Sd", "Sensibilidad", "Especificidad")
  vectorLon <- 1:(length(d0)-1)
  
  #2.- Obtenemos los datos para Train y Test
  size <- as.integer(porcentajeMuestra*min(nrow(d1), nrow(d0))/100)
  #2.- Obtenemos los datos para 2011 y para 2012.
  m <- getMuestraPacientes(d1, d0, size)
  train5424 <- subset(m, m$Diabetico==0)
  trainOtros <- subset(m, m$Diabetico==1)
  test5424 <- d0[!rownames(d0) %in% rownames(train5424),]
  testOtros <- d1[!rownames(d1) %in% rownames(trainOtros),]
  train <- m
  test <- rbind(test5424, testOtros)
  rm(train5424)
  rm(trainOtros)
  rm(test5424)
  rm(testOtros)
  rm(size)
  rm(m)
  
  #3.- Obtenemos la media de la muestra para normalizar en 2012. 
  datos_pacientes <- rbind(apply(train[,vectorLon], 2, mean), apply(train[,vectorLon], 2, sd0))
  rownames(datos_pacientes) <- c("Media", "Sd")
  train_n <- train
  test_n <- test
  
  #4.- Hay que normalizar tanto 2011 como 2012.
  train_n[,vectorLon] <- t(apply(train[,vectorLon], 1, standVectorEspecialFila,  datos=datos_pacientes))
  test_n[,vectorLon] <- t(apply(test[,vectorLon], 1, standVectorEspecialFila,  datos=datos_pacientes))
  
  
  #Preparamos la fórmula y ejecutamos la regresión.
  #comando <- getCadenaRegresion(colnames(train), "train")
  glm <- getCadenaRegresion(colnames(train), train)
  #suppressWarnings(eval( parse (text=comando) ))
  
  #Evaluamos el resultado y validamos con 2012.
  p <- suppressWarnings(predict(glm, newdata=test[,vectorLon], type="response"))
  
  #Calculamos la matriz de confución y la imprimimos en el resultado.
  r12 <- ifelse(p <= 0.5, 0, 1)
  matriz_confusion <- table(r12, test$Diabetico)
  pintaSensibilidadEspecifidad(matriz_confusion, realizacion)
  
  #Quitamos la columna de diabético
  atcs <- colnames(train[,c(-length(train))])
  #t <- round(summary(glm)$coef[,'Pr(>|t|)'], 6)
  x <- apply(datos_pacientes, 1, valor)
  sensibilidad <- round(matriz_confusion[2,2] / (matriz_confusion[2,2] + matriz_confusion[1,2]), 4)
  especificidad <- round(matriz_confusion[1,1] / (matriz_confusion[1,1] + matriz_confusion[2,1]), 4)
  resultado <- rbind(resultado, matrix(c(rep(realizacion, length(atcs)), 
                                         colnames(datos_pacientes), 
                                         #t[atcs], 
                                         rep(realizacion, length(atcs)),
                                         x[,1],
                                         x[,2],
                                         rep(sensibilidad, length(atcs)),
                                         rep(especificidad, length(atcs))), 
                                       nrow=length(atcs), ncol=7))
  
  rm(comando)
  rm(glm)
  rm(matriz_confusion)
  rm(test)
  rm(train)
  rm(test_n)
  rm(train_n)
  rm(atcs)
  #rm(t)
  rm(r12)
  rm(p)
  rm(x)
  rm(sensibilidad)
  rm(especificidad)
  rm(vectorLon)
  rm(datos_pacientes)
  
  return(resultado)
}

###############################################
### FUNCION: getPorcentajeTrain -> Dado un conjunto de datos devuelve el % especificado de registros aleatoriamente.
### PARAMETROS:
# d --> Data Frame con los pacientes diabéticos.
# porcentajeMuestra --> Data Frame con los pacientes sanos.
# size --> Tamaño de la muestra.
#
getPorcentajeTrain <- function(d, porcentajeMuestra) {
  #Obtenemos n muestras de pacientes diabéticos y de sanos.
  n <- as.integer(nrow(d)*porcentajeMuestra/100)
  x <- obtieneSampleMatrizRL(d, n)
  rm(n)
  return(x)
}

######################################
######################################
######################################
### Funciones auxiliares para pintar
######################################
######################################
######################################


###############################################
### FUNCION: pintaMC -> Dado una matriz de confusión, la imprime en la pantalla.
### PARAMETROS:
# m --> Matriz de confusión, 2x2
#
pintaMC <- function(m) {
  cat("\n\nDatos resumen de la matriz de confusión [Predicción / 2012]\n\n")
  s <- "__   "
  for ( a in as.character(colnames(m))) {
    s <- paste(s, "[", a ,"] ", sep="")
  }
  cat(paste(s, "\n\n", sep=""))
  for( i in 1:nrow(m)) {
    s <- ""
    for( a in as.character(m[i,])) {
      s <- paste(s, a)
    }
    cat(paste("[", rownames(m)[i], "] ", s, "\n\n", sep=""))
  }
  
  rm(a)
  rm(i)
  rm(s)
}

eliminaATCsVacios <- function(r) {
  #Eliminamos los ATCs que están vacíos.
  ATCs_vacios <- c()

  for (i in 1:ncol(r)) {
    if ( min(r[, i]) == 0 & max(r[, i]) == 0) {
      ATCs_vacios <- rbind(ATCs_vacios, colnames(r)[i])
    }
  } 

  rm(i)

  return(r[,!colnames(r) %in% ATCs_vacios])
}

pintaGraficaDistribucionATC <- function(resultado, familia, patron) {
  atcsSignificativos <- c()
  
  for (f in familia) {
    s <- resultado[grep(paste("^", f, patron, sep=""), resultado$ATC), c("ATC", "pValue")]
    s$ATC <- factor(as.character(s$ATC))
    s$pValue <- as.numeric(as.character(s$pValue))
    
    atcsSignificativos <- c(atcsSignificativos, pintaGrafricaDistribucionSeleccion(s, f))
  }
  
  rm(f)
  rm(s)
  
  return(atcsSignificativos)
}

pintaGraficaDistribucionResumen <- function(atcsSignificativos, patron) {
  for (a in atcs) {
    s <- resultado[resultado$ATC %in% atcsSignificativos, c("ATC", "pValue")]
    s$ATC <- factor(as.character(s$ATC))
    s$pValue <- as.numeric(as.character(s$pValue))
    
    atcsSignificativos <- c(atcsSignificativos, pintaGrafricaDistribucionSeleccion(s, f))
  }
  
  rm(f)
  rm(s)
  
  return(atcsSignificativos)
}

pintaGrafricaDistribucionSeleccion <- function(s, f) {
  atcsSignificativos <- c()
  
  #Seleccionamos los colores de las etiquetas.
  colores=c()
  for (atc in sort(unique(s$ATC))) {
    c = ifelse(quantile(s[s$ATC == atc, "pValue"], 0.975, names=FALSE, na.rm=TRUE) <= 0.05, "red", "black" )
    colores = c(colores, c)
    
    if (!is.na(c) & c == "red") {
      atcsSignificativos <- c(atcsSignificativos, atc)
    }
  }
  
  g <- ggplot(s, aes(x=ATC, y=pValue)) + geom_boxplot(fill="steelblue") + 
    scale_y_continuous(name = "Valor del pValue", 
                       breaks=c(0, 0.05, 0.25, 0.50, 0.75, 1.00)) +
    theme(axis.text.x = element_text(size = 8, colour = colores, angle=90)) + 
    theme(title = element_text(size = 10, colour = "black")) + 
    geom_hline(aes(yintercept=0.05, colour="red"), show.legend = FALSE) + 
    labs(title=paste("Distribución por variables [", f, "] en 1000 realizaciones", sep=""))
  print(g)
  
  rm(c)
  rm(colores)
  rm(atc)
  rm(g)
  
  return(atcsSignificativos)
}


imprimeSensibilidadEspecifidad <- function(d1, d0, pacientes12, size) {
  #Inicializamos para que los resultados sean repetibles.
  set.seed(102)

  #Creamos un cluster de 6 cores para dejar 2 cores para otras tareas.
  cluster <- makeCluster(1)
  registerDoParallel( cluster )
  resultado <- foreach(i=1:10, .combine = rbind) %do% {
    ejecutaRegresion(d1, d0, pacientes12, size, i)
  }
  stopCluster( cluster )
}

imprimeSensibilidadEspecifidadEvolucion <- function(d1, d0, porcentajeMuestra) {
  #Inicializamos para que los resultados sean repetibles.
  set.seed(102)
  
  #Creamos un cluster de 6 cores para dejar 2 cores para otras tareas.
  cluster <- makeCluster(1)
  registerDoParallel( cluster )
  resultado <- foreach(i=1:10, .combine = rbind) %do% {
    ejecutaRegresionEvolucion(d1, d0, porcentajeMuestra, i)
  }
  stopCluster( cluster )
}

###############################################
### FUNCION: sd0 -> Devuelve la desviación típica. Pero si es 0, devuelve 1e-12
### PARAMETROS:
# x --> Vector
#
sd0 <- function(x) {
 a<- sd(x)
 if (a==0) {
   a <- 1e-12
 }
 return(a)
}








pp <- function(){
  
  bucle <- 1
  
  #Marcamos la semilla para el sample. 
  set.seed(999)
  
  #Creamos la matriz al principio
  resultado <- matrix(nrow=0, ncol=3)
  colnames(resultado) <- c("Iteracion", "ATC", "pValue")
  
  #Iteramos n veces
  for( i in 1:bucle) {
    r <- ejecutaRegresion(diab, sanos, 2011, 2012, size, i)
    
    resultado <- rbind(resultado, r)
  }
  
  resultado <- as.data.frame(resultado)
  resultado$pValue <- as.numeric(as.character(resultado$pValue))
  
  r <- resultado[resultado$pValue <= 0.05,]
  
  index <- with(r, order(Iteracion, pValue))
  r[index, ]
  
  fichero <- paste(salidaDatos, "regresion.csv", sep="")
  write.csv(resultado, fichero)
  
  p
  d11 <- csv[csv$Anyo == 2011, c(7:(length(colnames(csv))-N))]
  d11 <- sanos[sanos$Anyo == 2011, c(7:(length(colnames(sanos))-N))]
  sum(d11)/nrow(d11)
  
  
  d12 <- csv[csv$Anyo == 2012, 7:(length(colnames(csv))-N)]
  d12 <- sanos[sanos$Anyo == 2012, 7:(length(colnames(sanos))-N)]
  sum(d12)/nrow(d12)
  
  
  71.8831*100/91.34606
  3.936726*100/4.291993
  
}







