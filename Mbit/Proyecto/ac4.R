
#Carga de librerías.
suppressPackageStartupMessages(library(ca))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(vcd))

#Importaciones de otros ficheros.
source("PCAs.R")

getCRGDiabHipertensionAtc <- function(csv, anyo, tipo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo)
  
  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, tipo)
  #Le concatenamos el código de CRG para filtrar por los códigos que queremos. 
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG)
  colnames(m_sumaFamilias)[15] <- "CRG"
  
  m_5424 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"CRG"] == 5424), 2, sum))
  colnames(m_5424) <- c("CRG_5424")
  m_6144 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"CRG"] == 6144), 2, sum))
  colnames(m_6144) <- c("CRG_6144")
  m_7071 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"CRG"] == 7071), 2, sum))
  colnames(m_7071) <- c("CRG_7071")
  
  #Quitamos de la fila la que servía para marcar el CRG.
  m <- cbind(m_5424[rownames(m_5424) != "CRG",], m_6144[rownames(m_6144) != "CRG",], m_7071[rownames(m_7071) != "CRG",])
  colnames(m) <- c("5424", "6144", "7071")
  rownames(m) <- colnames(m_sumaFamilias)[1:ncol(m_sumaFamilias)-1]
  
  pintaTC(m)
  
  #Eliminamos variables
  rm(dx)
  rm(m_sumaFamilias)
  rm(m_6144)
  rm(m_5424)
  rm(m_7071)
  
  return(m)
}

getAtcSexo <- function(csv, anyo, tipo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo)

  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, tipo)
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$Sexo)
  colnames(m_sumaFamilias)[15] <- "Sexo"
  
  #Le concatenamos el código de CRG para filtrar por los códigos que queremos. 
  m_S1 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"Sexo"] == 1), 2, sum))
  colnames(m_S1) <- c("Sexo_1")
  m_S2 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"Sexo"] == 2), 2, sum))
  colnames(m_S2) <- c("Sexo_2")

  #Quitamos de la fila la que servía para marcar el CRG.
  m <- cbind(m_S1[rownames(m_S1) != "Sexo",], m_S2[rownames(m_S2) != "Sexo",])
  colnames(m) <- c("Sexo_1", "Sexo_2")
  rownames(m) <- colnames(m_sumaFamilias)[1:ncol(m_sumaFamilias)-1]
  
  pintaTC(m)
  
  #Eliminamos variables
  rm(dx)
  rm(m_sumaFamilias)
  rm(m_S1)
  rm(m_S2)
  
  return(m)
}

getCRGsSexo <- function(csv, anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo & csv$CRG %in% c(5424, 6144, 7070, 7071))
  
  m <- xtabs(~ dx$CRG + dx$Sexo)
  colnames(m) <- c("Sexo_1", "Sexo_2")
  
  pintaTC(m)
  
  #Eliminamos variables
  rm(dx)

  return(m)
}

getCRGsEdad <- function(csv, anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo & csv$CRG %in% c(5424, 6144, 7071))
  
  m <- xtabs( ~ dx$CRG + dx$RangoEdad)
  
  pintaTC(m)
  
  #Eliminamos variables
  rm(dx)
  
  return(m)
}

getATCsCRGsEdad <- function(csv, anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo & csv$CRG %in% c(5424, 6144, 7071))
  
  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "EXISTENCIA")
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG)
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$RangoEdad)
  colnames(m_sumaFamilias)[15:16] <- c("CRG", "RangoEdad")
  
  EscribeDimensionalidadTabla(m_sumaFamilias)
  
  #Eliminamos variables
  rm(dx)
  
  return(as.data.frame(m_sumaFamilias))
}

getCRGsEdadSexo <- function(csv, anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo & csv$CRG %in% c(5424, 6144, 7071))
  
  m <- cbind(dx$CRG, dx$RangoEdad, dx$Sexo)
  colnames(m) <- c("CRG", "RangoEdad", "Sexo")
  
  EscribeDimensionalidadTabla(m)
  
  #Eliminamos variables
  rm(dx)
  
  return(as.data.frame(m))
}

getATCsCRGsSexo <- function(csv, anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo & csv$CRG %in% c(5424, 6144, 7071))
  
  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "EXISTENCIA")
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG)
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$Sexo)
  colnames(m_sumaFamilias)[15:16] <- c("CRG", "Sexo")
  
  EscribeDimensionalidadTabla(m_sumaFamilias)
  
  #Eliminamos variables
  rm(dx)
  
  return(as.data.frame(m_sumaFamilias))
}

getATCs2011CRGs2012 <- function(csv) {
  #Vamos a calcular los pacientes que son comunes en 2011 y en 2012.
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
  rm(i)
  rm(d11)
  rm(d12)
  
  #Con esto tengo el número de pacientes que están en ambos años. Me quedo con los que están en 2011.
  dx <- subset(Si_2011_Si_2012, Si_2011_Si_2012$CRG_11 == 5424 & 
                 Si_2011_Si_2012$CRG_12 %in% c(5424, 6144, 7071))
  dx2 <- subset(csv, csv$Anyo == 2011 & csv$Id %in% dx$Id )
  
  m_sumaFamilias <- reduceMatrizATC(dx2, 7:(ncol(dx2)-N), 1, "EXISTENCIA")
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG_12)
  colnames(m_sumaFamilias)[15] <- c("CRG")
  
  EscribeDimensionalidadTabla(m_sumaFamilias)
  
  #Eliminamos variables
  rm(dx)
  rm(dx2)
  
  return(as.data.frame(m_sumaFamilias))
}

getAtcEdad <- function(csv, anyo, tipo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo)
  
  m_sumaFamilias <- as.data.frame(reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, tipo))
  m_sumaFamilias <- as.data.frame(cbind(m_sumaFamilias, dx$RangoEdad))
  colnames(m_sumaFamilias)[15] <- "RangoEdad"
  
  #Le concatenamos el código de CRG para filtrar por los códigos que queremos. 
  
  m_R1 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R1", 1:14), 2, sum))
  #colnames(m_R1) <- c("[0, 49)")
  colnames(m_R1) <- c("R1")
  m_R2 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R2", 1:14), 2, sum))
  #colnames(m_R2) <- c("[49, 55)")
  colnames(m_R2) <- c("R2")
  m_R3 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R3", 1:14), 2, sum))
  #colnames(m_R3) <- c("[55, 59)")
  colnames(m_R3) <- c("R3")
  m_R4 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R4", 1:14), 2, sum))
  #colnames(m_R4) <- c("[59, 63)")
  colnames(m_R4) <- c("R4")
  m_R5 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R5", 1:14), 2, sum))
  #colnames(m_R5) <- c("[63, 68)")
  colnames(m_R5) <- c("R5")
  m_R6 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R6", 1:14), 2, sum))
  #colnames(m_R6) <- c("[68, 78)")
  colnames(m_R6) <- c("R6")
  m_R7 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R7", 1:14), 2, sum))
  #colnames(m_R7) <- c("[78, 103]")
  colnames(m_R7) <- c("R7")
  
  #Quitamos de la fila la que servía para marcar el CRG.
  m <- cbind(m_R1[rownames(m_R1) != "RangoEdad",], m_R2[rownames(m_R2) != "RangoEdad",],
             m_R3[rownames(m_R3) != "RangoEdad",], m_R4[rownames(m_R4) != "RangoEdad",],
             m_R5[rownames(m_R5) != "RangoEdad",], m_R6[rownames(m_R6) != "RangoEdad",],
             m_R7[rownames(m_R7) != "RangoEdad",])
  #colnames(m) <- c("[0, 49)", "[49, 55)", "[55, 59)", "[59, 63)", "[63, 68)", "[68, 78)", "[78, 103)")
  colnames(m) <- c("R1", "R2", "R3", "R4", "R5", "R6", "R7")
  rownames(m) <- colnames(m_sumaFamilias)[1:ncol(m_sumaFamilias)-1]
  
  pintaTC(m)
  
  #Eliminamos variables
  rm(dx)
  rm(m_sumaFamilias)
  rm(m_R1)
  rm(m_R2)
  rm(m_R3)
  rm(m_R4)
  rm(m_R5)
  rm(m_R6)
  rm(m_R7)
  
  return(m)
}

pintaTC <- function(m) {
  #m es nuestra tabla de contingencia. 
  cat("\n\n")
  cat("####Datos resumen de la matriz\n\n")
  cat(paste("--> Número Filas:", nrow(m), "\n\n", sep=""))
  cat(paste("--> Número Columnas:", ncol(m),"\n\n", sep=""))
  cat(paste("--> Total:", sum(m), "\n\n", sep=""))
  cat("--> Tabla de Contingencia:\n\n")
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



###################################
##### Funciones Datos Ficticios
###################################

pintaMosaicoATCs <- function(v, rows, cols, main) {
  m1 <- matrix(m[v[1],], ncol = cols, nrow=rows)
  for ( i in 2:length(v)) {
    m1 <- rbind(m1, m[v[i],])
  }
  rownames(m1) <- rownames(m)

  for ( i in 1:length(v)) {
    rownames(m1)[i] <- rownames(m)[v[i]]
  }

  par(mfrow=c(1,1), mar=c(4,4,3,3))
  mosaicplot(m1, shade = TRUE, main = main)
  
  rm(m1)
}

pintaMosaicoCRGs <- function(v, rows, cols, main) {
  #m1 <- matrix(m[v[1],], ncol = cols, nrow=rows)
  #for ( i in 2:length(v)) {
  #  m1 <- rbind(m1, m[v[i],])
  #}
  #rownames(m1) <- rownames(m)
  #
  #for ( i in 1:length(v)) {
  #  rownames(m1)[i] <- rownames(m)[v[i]]
  #}
  
  par(mfrow=c(1,1), mar=c(4,4,3,3))
  mosaicplot(m, shade = TRUE, main = main)
  
  #rm(m1)
}

getMatrizfromDF <- function(a) {
  colnames(a) <- c("x", "y", "total")
  #Generamos matrix
  m <- matrix(0, nrow=length(unique(a$x)), ncol=length(unique(a$y)))
  rownames(m) <- as.character(unique(a$x))
  colnames(m) <- as.character(unique(a$y))
  for (i in 1:nrow(a)) {
    m[as.character(a$x[i]), as.character(a$y[i])] <- a$total[i]
  }
  rm(i)
  
  return(m)
}

LeeFichero <- function(fichero) {
  mat <- readMat(file.path(origenMat, fichero))
  df <- as.data.frame(cbind(as.vector(mat$x), as.vector(mat$y), as.vector(mat$z)))
  colnames(df) <- c("old_x", "y", "z")
  df <- cbind(df, rep(as.character("[0, 11)"), length(df$y)))
  colnames(df)[4] <- "x"
  df$x <- as.character(df$x)
  df[df$old_x >= 11 & df$old_x < 25, "x"] <- "[11, 25)"
  df[df$old_x >= 25 & df$old_x <= 40, "x"] <- "[25, 40]"
  df$x <- factor(df$x)
  return (df)
}

EscribeTablaContingencia <- function(m) {
  cat("\n\nTabla de Contingencia:\n\n")
  cat("   D1 D2 D3\n\n")
  for(i in 1:nrow(m)) {
    a <- paste("[", rownames(m)[i], "] ", sep="")
    for(j in m[i,]) {
      a <- paste(a, j)
    }
    cat(paste(a, "\n\n"))
  }
  rm(i)
  rm(j)
  rm(a)
}

EscribeDimensionalidadTabla <- function(m) {
  numb.dim.cols<-ncol(m)-1
  numb.dim.rows<-nrow(m)-1
  a <- min(numb.dim.cols, numb.dim.rows) #dimensionality of the table
  cat(paste("\n\nDimensionalidad [", a, "]:", 
            " min(número de filas - 1, número columnas - 1) [",numb.dim.rows,",",numb.dim.cols, "]\n\n",sep=""))
  labs<-c(1:a) #set the numbers that will be used as x-axis' labels on the Malinvaud's test scatterplot
  
  return (labs)
}

EscribeTestInercia <- function(p) {
  eig <- get_eigenvalue(p)
  trace <- sum(eig$eigenvalue) 
  cor.coef <- sqrt(trace)
  cat("\n\n<b>Test de Inercia</b>\n\n")
  cat(paste("\n\nInercia: ", round(trace, 4), " - Coeficiente: ", round(cor.coef, 4), sep=""))
  cat("\n\n")
  rm(eig)
  rm(trace)
  rm(cor.coef)
}

EscribeTestChi <- function(m) {
  chisq <- chisq.test(m)
  cat("\n\n<b>Método de CHI^2</b>\n\n")
  cat(paste("Estadístico: ", round(chisq$statistic,4), 
            " ; df: ", chisq$parameter, 
            " ; p-Value: ", round(chisq$p.value, 4), sep=""))
  cat("\n\n")
  rm(chisq)
}

EscribeContribucionDimension <- function(m, dim) {
  #Pintamos las dimensiones según el Test de Molinvaud. 
  dims.to.be.plotted <- 2
  
  # AC del paquete estándar.
  res.ca <- ca(m, nd=dim)
  #res.ca <- ca(m)
  
  # AC almacenamos el resultado para pintarlo después.
  #cadataframe <- summary(ca(m, nd=dims.to.be.plotted))
  cadataframe <- summary(res.ca)
  
  cat("\n\n")
  cat("\n\n<b>-->Filas</b>\n\n")
  
  par(mfrow=c(1,2), mar=c(4,4,3,3))
  counter <- 0
  for(i in seq(7, ncol(cadataframe$rows), 3)) {
    counter <- counter +1
    max = max(cadataframe$rows[,i]/10) + 10
    max = min(max, 100)
    barplot(cadataframe$rows[,i]/10, ylim=c(0,100), col="steelblue",
            xlab="Categorías de Filas", 
            ylab=paste("% Contribución a Dim.", counter), 
            names.arg=rownames(m), cex.lab=0.8, las=1, cex.axis = 0.7, cex.names = 0.8)
    abline(h=round(((100/nrow(m))), digits=0), col="red", lty=2)
  }
  
  cat("\n\n<b>-->Columnas</b>\n\n")
  counter <- 0
  for(i in seq(7, ncol(cadataframe$columns), 3)){    
    counter <- counter +1
    max = max(cadataframe$columns[,i]/10) + 10
    max = min(max, 100)    
    barplot(cadataframe$columns[,i]/10, ylim=c(0,100), col="steelblue",
            xlab="Categorías de Columnas", 
            ylab=paste("% Contribución a Dim.", counter), 
            cex.lab=0.80, las=1, cex.axis = 0.7, cex.names = 0.8,
            names.arg=colnames(m))
    abline(h=round(((100/ncol(m))), digits=0), col="red", lty=2)
  }
  cat("\n\n")
  
  rm(dims.to.be.plotted)
  rm(res.ca)
  rm(cadataframe)
  rm(counter)
  rm(max)
}

EscribeGraficoSimetrico <- function(p, main) {
  cat("\n\n")
  par(mfrow=c(1,1), mar=c(4,4,3,3))
  xmin = min(p$row$coord[,1], p$col$coord[,1])[1] * 1.2
  xmax = max(p$row$coord[,1], p$col$coord[,1])[1] * 1.2
  ymin = min(p$row$coord[,2], p$col$coord[,2])[1] * 1.2
  ymax = max(p$row$coord[,2], p$col$coord[,2])[1] * 1.2
  plot(p$row$coord, type = "p", pch = 16, col="blue",
     xlim=c(xmin, xmax),
     ylim=c(ymin, ymax),
     xlab="Dimensión 1", ylab="Dimensión 2",
     main = main, cex.main=0.7
  )
  points(p$col$coord, type = "p", pch = 17, col="red")
  for (i in 1:nrow(m)) {
    text(p$row$coord[i,1] + 0.015, p$row$coord[i,2] + 0.03, rownames(m)[i], col="blue", cex=0.6)
  }
  for (i in 1:ncol(m)) {
    text(p$col$coord[i,1] + 0.015, p$col$coord[i,2] + 0.03, colnames(m)[i], col="red", cex=0.6)
  }
  abline(h=0,lty=2)
  abline(v=0,lty=2) 
  
  Sys.sleep(5)
  
  rm(xmin)
  rm(xmax)
  rm(ymin)
  rm(ymax)
  rm(i)
}

EscribeGraficosAsimetricos <- function(p, dims.to.be.plotted, map) {
  cat("\n\n")
  arr <- c(TRUE, FALSE)
  if (map == "colprincipal" | map == "colgreen") { 
    arr <- c(FALSE, TRUE)
  }
  
  #counter <- 1
  par(mfrow=c(1,1), mar=c(4,4,3,3))
  for(i in 2:dims.to.be.plotted){    
    cat(paste("\n\n --> Gráfico Asimétrico -", map, "\n\n"))
    p1 <- fviz_ca_biplot(p, map = map, arrow = arr)
    plot(p1)
    
#    cat("\n\n --> Gráfico Asimétrico - ColPrincipal\n\n")
#    p1 <- fviz_ca_biplot(p, map ="colprincipal", arrow = c(FALSE, TRUE))
#    plot(p1)
    
    rm(p1)
  }
#  rm(dims.to.be.plotted)
  rm(i)
  
  Sys.sleep(5)
}

getMCA <- function(df) {
  #Preparamos la matriz para el MCA
  dt <- df[,2:4]
  dt$x <- paste("x_", dt$x, sep="")
  dt$y <- paste("y_", dt$y, sep="")
  dt$z <- paste("z_", dt$z, sep="")
  dt <- as.matrix(dt)
  
  p <- MCA(dt, graph = FALSE)
  rm(dt)
  
  return(p)
}

getATCsMCA <- function(df) {
  dt <- df
  for (i in 1:14) {
    dt[,i] <- paste(colnames(dt)[i], "_", dt[,i], sep="")
  }
  rm(i)
  
  dt <- as.matrix(dt)
  p <- MCA(dt, graph = FALSE)
  rm(dt)
  
  return(p)
}

getATCsSexoMCA <- function(df) {
  dt <- df
  for (i in 1:14) {
    dt[,i] <- paste(colnames(dt)[i], "_", dt[,i], sep="")
  }
  dt[,16] <- paste("Sexo_", dt[,16], sep="")
  rm(i)
  
  dt <- as.matrix(dt)
  p <- MCA(dt, graph = FALSE)
  rm(dt)
  
  return(p)
}

getCRGsEdadSexoMCA <- function(df) {
  dt <- df
  dt[,3] <- paste("Sexo_", dt[,3], sep="")
  
  dt <- as.matrix(dt)
  p <- MCA(dt, graph = FALSE)
  rm(dt)
  
  return(p)
}

pintaNotaAnalisis <- function(ac, funcion, anyo, i, j) {
  cat("\n\n")
  #CRG - ATC
  if (ac == "A.1") pintaNotaAnalisis_A1(funcion, anyo, i, j)
  if (ac == "A.2") pintaNotaAnalisis_A2(funcion, anyo, i, j)
  
  #ATC - Sexo
  if (ac == "B.1") pintaNotaAnalisis_B1(funcion, anyo, i, j)  
  if (ac == "B.2") pintaNotaAnalisis_B2(funcion, anyo, i, j)  

  #ATC - Rango de Edad
  if (ac == "C.1") pintaNotaAnalisis_C1(funcion, anyo, i, j) 
  if (ac == "C.2") pintaNotaAnalisis_C2(funcion, anyo, i, j) 
  
  #CRGs (5424, 6144, 7070, 7071) y Sexo
  if (ac == "D") pintaNotaAnalisis_D(funcion, anyo, i, j) 
  
  #CRG(5424, 6144, 7071) y Edad
  if (ac == "E") pintaNotaAnalisis_E(funcion, anyo, i, j) 
  
  #ATCs (14 familias principales), CRGs (5424, 6144, 7071) y Rango de Edad
  if (ac == "F") pintaNotaAnalisis_F(funcion, anyo, i, j) 
  
  
  
  
}

pintaNotaAnalisis_A1 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que todas las frecuencias son superiores 
          a 5, por lo que es apropiado que utilicemos luego el método de Chi^2. Si no fuera el caso, 
          deberíamos utilizar el método de Fisher.")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que todas las frecuencias siguen siendo superiores 
          a 5.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("NOTA ANÁLISIS: Como primer vistazo se puede identificar:\n\n")
      cat("a) H, D, J son ATCs con un comportamiento similar en relación a los valores 5424 y 7071. Parece que la frecuencia de uso cuando es 
          diabetes e hipertensión (sin que estén agravados por una tercera enfermeadad) disminuye.\n\n")
      cat("b) A, M son ATCs con un comportamiento similar y parece que está más relacionado con el 
            CRG-base 5424.\n\n")
      cat("c) C, G, S son ATCs con un comportamiento similar y parece que está más relacionado con el CRG-base 6144.\n\n")
      cat("d) N, B, R son ATCs con un comportamiento similar y parece que está más relacionado con el CRG-base 7071.\n\n")
      cat("e) L, P, V son ATCs con pocos valores, en comparación al resto, y que no parece identificarse con ningún CRG-base de los tres en análisis.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Si comparamos el gráfico mosáico con el correspondiente al año 2011 se puede
          observar que en general sigue la misma estructura de patrones para los grupos ATCs, aunque si se 
          puede comprobar que hay un cambio significativo para los grupos ATCs S y B.\n\n
          a) En el caso de la familia S, se observa que la frecuencia para el CRG-base 6144 pasa a ser la esperada.\n\n
          b) En el caso de la familia B, se observa que para el CRG-base 6144 ha aumentado la frecuencia con respecto a 
          esperado y sin embargo, para el CRG-base 7071 ha bajado la frecuencia con respecto a lo esperado.\n\n
          TO-DO: Habría que hacer un análisis más detallado en esta parte para bajar al detalle del cambio.")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0952) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANALISIS: En este caso la inercial (0.0844) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 23.648,9 con 26 grados de libertad. Según las tablas de Chi^2 X^2[0.05](26) = 38,8851. Como 23.648 >> 38 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 21.398,12 con 26 grados de libertad. Según las tablas de Chi^2 X^2[0.05](26) = 38,8851. Como 21.398 >> 38 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa que la relación entre los grupos de ATCs que se podían visualizar en el gráfico de mosaico se ven representadas.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar visiblemente el cambio detallado en el gráfico de mosaico. 
          La familia ATC B ha cambiado de cuadrante en la gráfica y la familia S está más cerca del (0,0).\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("TEORÍA: Recordemos la teoría que se ha visto anteriormente. Para medir la distancia en un gráfico
          asimétrico hay que considerar la distancia perpendicular a la proyección de la recta desde el 
          punto (0,0) al elemento en cuestión. Si la perpendicular corta con la parte trasera de la proyección 
          (en el otro eje), representa que la frecuencia es muy inferior a lo esperado.\n\n")
      cat("\t\tNOTA ANÁLISIS: Se puede observar que se reproduce lo que se esperaba después de la visualización
          del gráfico de mosáico.\n\n")
      cat("\t\ta) Los ATCs que están más asociados al 6144 son C, G y S. El ATC B también.\n\n
          b) Los ATCs que están más asociados al 5424 son los A y M. Le siguen D y H.\n\n
          c) Los ATCs que está más asociados con el 7071 son R y B.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar que en este gráfico también se ven visibles los elementos 
          que se diferenciaban en el gráfico de mosaico. Además de los ya comentados de la familia S y B
          también se encuentran los otros cambios que se observaban:\n\n")
      cat("\t\ta) La familia H está más asociada con el CRG-base 5424 que en 2011.\n\n
          b) La familia G tiene una asociación menos fuerte con el CRG-base 6144 que en 2011.\n\n
          c) Los ATCs que está más asociados con el 7071 son R y B.\n\n")
      
    }
  }
}

pintaNotaAnalisis_A2 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar, a diferencia con la matriz de suma de dispensaciones, que ahora sí 
          que hay valores por debajo de 5 elementos en la tabla de contingencia. El valor de L para 5424 es de 2. Como 
          decíamos en el apartado anterior, esto hará que el método de Chi^2 pudiera generar error.\n\n
          TO-DO: Pendiente de revisar dos líneas de acción: Ejecutar Chi^2 sumando L+V y/o ejecutar método de Fisher.")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como ocurría en 2011, la tabla de contingencia tiene valores muy pequeños para la existencia 
          de medicamentos L y V. Vamos a tener el mismo problema puesto que cuando se ejecute el método de Chi^2 
          nos saldrá que puede arrojar valores pocos precisos.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Como primer vistazo se puede identificar que el gráfico es bastante distinto con respecto al de 2011 en suma:\n\n")
      cat("a) La familia H presenta menos asociación con el CRG-base 6144.\n\n")
      cat("b) Las familias D y J no se asocian ahora a el CRG-base 6144 y la asociación con el CRG-base 7071 es más débil.\n\n")
      cat("c) La familia A mantiene la asociación con el CRG-base 6144 y la asociación de la familia H es más débil a ese CRG-base.\n\n")
      cat("d) La familia C mantiene la asociación con el CRG-base 6144.\n\n")
      cat("e) La familia G no se asocia al CRG-base 6144 y pasa a asociarce al CRG-base 7071.\n\n")
      cat("f) La familia B con respecto a 2011 pasa a asociarse al CRG-base 6144.\n\n")
      cat("g) La familia R tiene una relación de asociación menor al CRG-base 7071.\n\n
          En definitiva, las relaciones de asociación presentan cambios con respecto a hacer una reducción de la matriz de 
          suma de disposiciones o de ocurrencia. Las relaciones más fuertes cuando se habla de ocurrencia son para la 
          familia H (con el CRG-base 7071), de la familia A (con el CRG-base 5424) y de la familia C (con el CRG-base 6144).\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Igual que ocurría cuando estábamos viendo la alternativa anterior, se puede observar 
          algunas pequeñas diferencias entre los años 2011 y 2012. Lo más llamativo es que aparece una asociación débil 
          de la familia N con el CRG-base 7071. La asociación de la familia M pierde intensidad tanto con el CRG-base 5424 
          como con el CRG-base 6144.\n\n
          Pero de carácter general las reglas de asociación no son muy distintas al año 2011.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0277) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0273) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 709.7 con 26 grados de libertad. Según las tablas de Chi^2 X^2[0.05](26) = 38,8851. Como 709 >> 38 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que como decíamos antes, este valor puede no ser correcto.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 704.9 con 26 grados de libertad. Según las tablas de Chi^2 X^2[0.05](26) = 38,8851. Como 704 >> 38 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que como decíamos antes, este valor puede no ser correcto.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa que el gráfico de ocurrencia cambia con respecto al gráfico de suma de dispensaciones, reflejando los movimientos 
          que se había identificado en el gráfico de mosáico.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se observa que la relación entre los grupos de ATCs que se podían visualizar en el gráfico de mosaico se ven representadas.\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En el gráfico asimétrico se puede observar las siguientes relaciones de asociación:\n\n
          a) CRG-base 5424: Familias M y A.\n\n 
          b) CRG-base 6144: Familias C.\n\n
          c) CRG-base 7071: Familias N, R, D.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como anticipábamos antes, en el gráfico asimétrico se puede observar las siguientes relaciones de asociación:\n\n
          a) CRG-base 5424: Familias M y A.\n\n 
          b) CRG-base 6144: Familias C.\n\n
          c) CRG-base 7071: Familias N, R, D.\n\n")
      
    }
  }
}

pintaNotaAnalisis_B1 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este gráfico se puede observar como hay una regla de asociación muy fuerte 
          entre las columnas y las filas de la tabla de contingencia.\n\n
          TO-DO: Revisar este resultado con el resultado del MAC y de la Edad con los ATCs para revisar si 
          esta 'asociación' tiene que ver con la inversión de Sexo a partir de los 70 años.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Al igual que ocurría en 2011, se puede observar como hay una regla de asociación muy fuerte 
          entre las columnas y las filas de la tabla de contingencia.\n\n
        TO-DO: Revisar este resultado con el resultado del MAC y de la Edad con los ATCs para revisar si 
          esta 'asociación' tiene que ver con la inversión de Sexo a partir de los 70 años.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.013) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0186) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 10.500,4 con 13 grados de libertad. Según las tablas de Chi^2 X^2[0.05](13) = 22,3620. Como 10.500 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 13.573,6 con 13 grados de libertad. Según las tablas de Chi^2 X^2[0.05](13) = 22,3620. Como 13.573 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
  }
  
  #Explicación de la Varianza
  if (funcion == "D.1") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Como se puede observar, la explicación de la varianza se hace solo con una dimensión. 
          Esto implica que a la hora de representar la información, no tenemos dos coordenadas por lo que en el gráfico 
          Simétrico hay que poner como segunda dimensión, a mano, el la ordenada 0, y no podemos representar un gráfico 
          asimétrico. Con esta librería hace falta que haya al menos 3 filas y/o tres columnas para representarlas.\n\n
          En la gráfica del test de Malivaund se representa el p-value para cada una de las dimensiones 'significativas'. La 
          línea roja representa el límite del 0.05 del p-value. Como se puede observar la dimensión está por debajo del 
          valor 0.05 por lo que no hay significancia estadística para rechazar la hipótesis de independencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Al igual que en 2011, que solo sale una única dimensión (es lo esperado porque el número de dimensiones es el mínimo 
          del número de filas y de columnas menos 1. Como Sexo son dos columnas, siempre va a dar 1.\n\n.")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("TEORÍA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. 
          Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
      cat("\t\tNOTA ANÁLISIS: Se puede observar que en el caso del Sexo, ambos valores contribuyen aproximadamente con los mismos valores y, además están en torno al nivel de lo esperado 
          si fueran valores uniformes. Es decir, el entendimiento de esta gráfica es que tiende a decir que no contribuye de forma dependiente entre el Sexo y los ATCs.
          TO-DO: Creo que aquí habría que profundizar algo más (no sé cómo ahora mismo).\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Al igual que en 2011, vemos que los valores de Sexo contribuyen 'de forma uniforme' a la dimensión en relación a lo esperado\n\n.")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
}

pintaNotaAnalisis_B2 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se pueden observar que en Ocurrencia ciertas frecuencias cambian de forma visible con 
          respecto al mosaico de suma de dispensaciones. Los cambios son los siguientes:\n\n
          a) Las familias N y B presentan una asociación menos fuerte aunque mantiene la proporción entre ambos sexos de más o menos frecuentes.\n\n
          b) Las familias M, V, L, R y G pasan a no presentar una asociación, es decir, mantienen frecuencias dentro de lo esperado en una distribución aleatoria.\n\n
          c) La familia J invierte la proporción, es decir, ahora presenta mayor frecuencia de lo esperado en el Sexo 2 (en el mosaico de suma lo hacía en el Sexo 1) 
          aunque la asociación es menos fuerte.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Solo se observa un cambio con respecto al mosaico de ocurrencia en 2012. La familia 
            N presenta una asociación más fuerte.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0086) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.01) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 418,4 con 13 grados de libertad. Según las tablas de Chi^2 X^2[0.05](13) = 22,3620. Como 418 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 530,9 con 13 grados de libertad. Según las tablas de Chi^2 X^2[0.05](13) = 22,3620. Como 530 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("TEORÍA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. 
          Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
      cat("\t\tNOTA ANÁLISIS: Se puede observar que en el caso del Sexo, ambos valores contribuyen aproximadamente con los mismos valores y, además están en torno al nivel de lo esperado 
          si fueran valores uniformes. Es decir, el entendimiento de esta gráfica es que tiende a decir que no contribuye de forma dependiente entre el Sexo y los ATCs.
          TO-DO: Creo que aquí habría que profundizar algo más (no sé cómo ahora mismo).\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Al igual que en 2011, vemos que los valores de Sexo contribuyen 'de forma uniforme' a la dimensión en relación a lo esperado\n\n.")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
}

pintaNotaAnalisis_C1 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Si visualizamos la información del gráfico, por rango de Edad se tiene la siguiente información:\n
          a) R1 [0, 49) -> Familias con asociación positiva: J, H, M, A y D.\n
          b) R2 [49, 55) -> Familias con asociación positiva: M,A,C,V.\n
          c) R3 [55, 59) -> Familias con asociación positiva: M,A,C,L.\n
          d) R4 [59, 63) -> Familias con asociación positiva: S,G,M,A,C,L.\n
          e) R5 [63, 68) -> Familias con asociación positiva: S,G,M,A,C. La familia A pierde intensidad en la asociación.\n
          f) R6 [68, 78) -> Familias con asociación positiva: S,G,B,R,D,V.\n
          h) R7 [78, 103] -> Familias con asociación positiva:J,H,B,R,N,D,V.\n\n
          A primera vista parece que la relación de las familias M y A pierden intensidad conforme avanza la edad,
          hasta desaparecer.\n
          Nótese que lo que indico es la parte 'azul', es decir, aquello donde hay más frecuencia de la esperada.\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Si comparamos con 2011 vemos que los patrones más o menos coinciden con algunas diferencias:\n
          a) R1 [0, 49) -> La asociación de la familia J es menos intensa.\n
          b) R2 [49, 55) -> Aparece una asociación con la familia H que en 2011 no había en positivo.\n
          c) R3 [55, 59) -> No se observan cambios.\n
          d) R4 [59, 63) -> En 2012 no aparecen relaciones positivas para las familias S y G.\n
          e) R5 [63, 68) -> En 2012 no aparecen relaciones positivas con la familia A.\n
          f) R6 [68, 78) -> En 2012 si aparece una relación positiva con la familia H.\n
          g) R7 [78, 103) -> En 2012 no aparece una relación positiva con la familia H. \n\n
          TO-DO: Habría que chequear si la situación de cambios de familia de medicamentos está relacionada con el 
          copago que se inició en 2012.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0215) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0243) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 17.362,6 con 78 grados de libertad. Según las tablas de Chi^2 X^2[0.05](70) = 90,53. Como 17.362 >> 90 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 17.689,6 con 78 grados de libertad. Según las tablas de Chi^2 X^2[0.05](70) = 90,53. Como 17.689 >> 90 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
#      cat("\t\tNOTA ANÁLISIS: Parece que se tiene con dos dimensiones una epxlicación total de la dimensionalidad.\n\n")
    }
    
    if (anyo == 2012) {
#      cat("\t\tNOTA ANÁLISIS: .\n\n")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Estos gráficos sirven para comparar los propios elementos de filas y columnas entre sí. 
          A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2, R3, R4 y R5 parece que no están muy lejos entre sí; en concreto R4 y R5 están situados muy cerca.\n 
          c) R6 \n
          d) R7 \n
          A nivel de filas (ATCs) se puede observar lo siguiente:
          a) Las familias B, N, D, V parece que están en un cluster.\n
          b) Las familias J, H parece que están en un cluster.\n
          c) Las familias S, G, R, L y P están separadas del resto.\n
          d) Las familias A, C, y M parece que tienen cierta relación entre sí.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Como se anticipaba en el gráfico de mosáico, se puede observar una diferencia en el comportamiento 
            de la toma de ATCs (número total de dispensaciones). A nivel de rango de edades parece que tenemos 
            tres clusters:\n
          a) R1 \n
          b) R2, R3, R4 y R5 parece que no están muy lejos entre sí; en concreto R4 y R5 están situados muy cerca.\n 
          c) R6 y R7 \n
          A nivel de filas (ATCs) se puede observar lo siguiente:
          a) Las familias B, N, S, G parece que están en un cluster. D y V se alejan de las familias B y N.\n
          b) Las familias R, D y J parece que están en un cluster.\n
          c) La familia V se queda más alejada del resto de clusters.\n
          d) Las familias C se queda más alejeda de la familia A y M. 
          e) Las familias A, M, L y P parece que forman un clusters.\n
          f) La familia H parece que se queda más alejada del resto de clusters.\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R7 parece estar muy relacionado con las familias B, V y N.\n
          b) R6 parece estar muy relacionado con la familia S.\n
          c) R5 parece estar muy relacionado con la familia C.\n
          d) R3, R4 y R5 parecen estar muy relacionados con la familia M.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R7 parece estar muy relacionado con las familias N, G, R y en algo de menor medida con B.\n
          b) R6 parece estar muy relacionado con la familia N, G, y en algo de menor medida B y R.\n
          c) R2, R3, R4 y R5 parece estar muy relacionado con la familia C.\n
          d) R2 parece estar muy relacionado también con A y M. Con algo menos de intensidad le pasa lo mismo a R3.\n
          Como se había anticipado en el gráfico de mosaico parece que en 2012 ha ocurrido un cambio en el número de dispensaciones 
          de las familias ATC.\n\n
          TO-DO: Chequear si en 2012 hubo algún medicamento nuevo sobre la diabetes.")
    }
  }
}

pintaNotaAnalisis_C2 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Si visualizamos la información del gráfico, por rango de Edad se tiene la siguiente información:\n
          a) R1 [0, 49) -> Familias con asociación positiva: H, A. Con respecto a suma de disposición no aparece asociación con J, M y D.\n
          b) R2 [49, 55) -> Familias con asociación positiva: M,A,C. Con respecto a suma de disposición no aparece asociación con V.\n
          c) R3 [55, 59) -> Familias con asociación positiva: A, C. Con respecto a suma de disposición no aparece asociación con M,L.\n
          d) R4 [59, 63) -> Familias con asociación positiva: C. Con respecto a suma de disposición no aparece asociación con S,G,M,A,C,L.\n
          e) R5 [63, 68) -> Familias con asociación positiva: Ninguna. Con respecto a suma de disposición no aparece asociación con S,G,M,A,C.\n
          f) R6 [68, 78) -> Familias con asociación positiva: G, S, B, D, V. Con respecto a suma de dispensación no aparece asociación con R.\n
          h) R7 [78, 103] -> Familias con asociación positiva:J, S, B, D, V. Con respecto a suma de dispensaciones no aparece asociación con H,R,N.\n\n
          Nótese que lo que indico es la parte 'azul', es decir, aquello donde hay más frecuencia de la esperada.\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Si comparamos con 2011 vemos que los patrones más o menos coinciden con algunas diferencias:\n
          a) R1 [0, 49) -> En 2012 no aparece asociación con la familia J.\n
          b) R2 [49, 55) -> Aparece una asociación con la familia H que en 2011 no había en positivo.\n
          c) R3 [55, 59) -> No se observan cambios.\n
          d) R4 [59, 63) -> En 2012 no aparecen relaciones positivas para las familias S y G.\n
          e) R5 [63, 68) -> En 2012 no aparecen relaciones positivas con la familia A.\n
          f) R6 [68, 78) -> En 2012 si aparece una relación positiva con la familia H.\n
          g) R7 [78, 103) -> En 2012 no aparece una relación positiva con la familia H. \n\n
          TO-DO: Habría que chequear si la situación de cambios de familia de medicamentos está relacionada con el 
          copago que se inició en 2012.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0168) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0193) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 815.9 con 78 grados de libertad. Según las tablas de Chi^2 X^2[0.05](70) = 90,53. Como 815 >> 90 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 1.024,5 con 78 grados de libertad. Según las tablas de Chi^2 X^2[0.05](70) = 90,53. Como 1.024 >> 90 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
 #     cat("\t\tNOTA ANÁLISIS: Parece que solo hay una dependencia.\n\n")
    }
    
    if (anyo == 2012) {
 #∫     cat("\t\tNOTA ANÁLISIS: .\n\n")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Estos gráficos sirven para comparar los propios elementos de filas y columnas entre sí. 
          A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2, R3 parece que no están muy lejos entre sí; R4 y R5 están situados muy cerca pero visualmente parece que son otro cluster.\n 
          c) R6 \n
          d) R7 \n
          A nivel de filas (ATCs) se puede observar lo siguiente:
          a) Las familias L y B aunque están cerca del cluster J, R, N visualmente parecen distintos.\n
          b) Las familias J, R y N parece que están en un cluster.\n
          c) Las familias S, G parece que están en un cluster.\n
          d) La familia V, H y P estás separada del resto.\n
          e) Las familias A, M y C parecen separadas del resto también.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Estos gráficos sirven para comparar los propios elementos de filas y columnas entre sí. 
          A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2, R3 parece que no están muy lejos entre sí; R4 y R5 están situados muy cerca pero visualmente parece que son otro cluster.\n 
          c) R6 y R7 parecen que forman un mismo cluster. \n
          A nivel de filas (ATCs) se puede observar lo siguiente:
          a) Las familias D, R, N y J parecen estar en el mismo cluster.\n
          b) Las familias L y C parecen que están en un cluster.\n
          c) Las familias M y A parecen que están en un cluster.\n
          d) La familia B y S parecen que están en un cluster.\n
          e) Las familias P y H parecen que están en un cluster.\n
          f) Las familias G y V parecen que están separadas del resto.\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R7 parece estar muy relacionado con las familias R y G. Con las familias N, S y D también parece tener relación.\n
          b) R6 parece estar muy relacionado con la familia N, R, S y G.\n
          c) R5 parece estar relacionado con la familia N.\n
          d) R2 parece estar muy relacionado con la familia M. En menor medida con A y C.\n
          e) R3 parece estar relacionado con las familias M y C.\n 
          f) R4 parece estar muy relacionados con la familia C y en menor medida con la M.\n
          g) R1 parece estar relacionado con la familia P y en menor medida con la A.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R7 parece estar muy relacionado con las familias R, N, S y D.\n
          b) R6 parece estar muy relacionado con la familia R, N, V y D y en algo de menor medida S.\n
          c) R5 parece estar muy relacionado con la familia N y L.\n
          d) R4 parece estar muy relacionado también con C. Con algo menos de intensidad le pasa lo mismo a R3.\n
          e) R3 y R2 parece estar relacionadas con C y M.\n
          f) R1 parece estar relacionado con P, H y en menor medida con A.\n\n
          TO-DO: Chequear si en 2012 hubo algún medicamento nuevo sobre la diabetes.")
    }
  }
}

pintaNotaAnalisis_D <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
           tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa una mayor dependencia del Sexo_1 con el grupo 5424 y como va invirtiéndose a través del CRG-base.
          En realidad lo que está reflejando es una relación indirecta, ya que como hay una relación entre el grupo y la edad, y hay otra entre
          la edad y el sexo (por la esperanza de la vida). \n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se refleja lo mismo, pero parece que hay una intensidad menor en la asociación entre el Sexo 1 y el grupo CRG-base 6144. \n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.022) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0167) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 119.8 con 3 grados de libertad. Según las tablas de Chi^2 X^2[0.05](3) = 7,81. Como 119.3 >> 7,81 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 99.1 con 3 grados de libertad. Según las tablas de Chi^2 X^2[0.05](3) = 7,81. Como 99 >> 7,81 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
}

pintaNotaAnalisis_E <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
          tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se puede observar en la tabla de contingencia, todos los elementos
          tienen más de 5 por lo que no debería haber problemas luego cuando se ejecute el método de Chi^2.\n\n")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar como conforme va aumentando el rango de edad la frecuencia con respecto a la 
          esperada se va desplazando también hacia abajo.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se observa el mismo comportamiento que en 2011.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.1986) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.2047) es mayor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa. Es decir, parece que hay una 
          correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 1.074,5 con 12 grados de libertad. Según las tablas de Chi^2 X^2[0.05](12) = 21,02. Como 1.074,5 >> 21,02 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 1.206,2 con 12 grados de libertad. Según las tablas de Chi^2 X^2[0.05](12) = 21,02. Como 1.206,2 >> 21,02 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Estos gráficos sirven para comparar los propios elementos de filas y columnas entre sí. 
          A nivel de filas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2, R3 parece que no están muy lejos entre sí.\n 
          c) R5, R6 parece que forman parte del mismo cluster.\n
          d) R4 parece que está a caballo entre R2, R3 y R5, R6.\n
          e) R7 está solo.\n
          A nivel de columnas (CRGs) se puede observar lo siguiente:
          a) Las tres variables son independientes.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: El gráfico es muy parecido al gráfico del 2011. La diferencia más sutil es 
          que R3 se aleja un poco del grupo 6144.\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: A simple vista se puede ver que:. 
          a) R1 tiene mucha asociación con el CRG-base 5424.\n
          b) R4 es el que tiene mayor asociación con 6144. R5 y R6 también aunque con menor intensidad.\n 
          c) R3 se encuentra más asociado al CRG-base 6144 que al CRG-base 5424.\n
          d) R2 se encuentra más asociado al CRG-base 5424, aunque también se asocia al 6144.\n
          e) R7 está más asociado al CRg-base 7071 aunque con una intensidad baja.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar un comportamiento muy parecido al del 2011.\n\n")
    }
  }
}

pintaNotaAnalisis_F <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Cuando comenzamos a hablar de MAC y ATCs no pintaré ni el gráfico de mosaico 
          ni la tabla de contingencia. Esto es debido a que en realidad serían muchas variables para una tabla.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Cuando comenzamos a hablar de MAC y ATCs no pintaré ni el gráfico de mosaico 
          ni la tabla de contingencia. Esto es debido a que en realidad serían muchas variables para una tabla.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (1.375) es mayor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa. Es decir, parece que hay una 
          correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Aparece el mismo valor en 2012 que en 2011: 1.375.\n\n")
    }
  }
  
  #Varianza
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Hace falta 16 dimensiones para explicar el 80% de la varianza.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En 2016 sigue haciendo falta 16 dimensiones para explicar el 80% de la varianza.\n\n")
    }
  }
  
  #Simétrico
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este gráfico se representan las variables. Como ocurría con el AC en teoría 
          solo sirve para poder comparar entre las distintas variables. Lo primero que se intuye es que parece 
          que el grupo 5424 no se relaciona con ningún elemento de ocurrencia de medicamento y sin embargo, se
          relaciona mucho con el rango de edad R1 y con algo de menor intensidad con R2.\n
          \n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Comparando el gráfico con el resultado de 2011 se pueden apreciar ciertas diferencias 
          que merece la pena analizar más adelante:
          a) V_1 parece que tiene un comportamiento diferente en 2012 con respecto a 2011.\n
          b) G_1 parece que tiene un comportamiento diferente en 2012 con respecto a 2011.\n
          c) A_0 tiene un comportamiento diferente pero porque en 2012 hay más elementos a 0 que en 2011.\n\n")
    }
  }
  
  #Gráfico Simétrico - No existencia de ATCs con Rango y CRG.
  if (funcion == "F")  {
    if (anyo == 2011)  {
      cat("\t\tNOTA ANÁLISIS: Se observa que los valores de no ocurrencia para las familias que más relacionadas están con 
          5424 son: A, C, B. N también, pero en mucha menor medida. No hay ninguna familia que la no ocurrencia esté especialmente 
          cerca del 6144 y del 7071.\n\n")                                                                                                                                                           
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No hay diferencias visuales en 2012 con respecto 2011.\n\n")
    }
  }
  
  #Gráfico Simétrico - Si existencia de ATCs con Rango y CRG.
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar que 6144 está muy cerca de C y de B. Con el 7071 teniendo en cuenta la 
          proporción de elementos tiene cierta relación con H, B, N, R.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tal y como se adelantaba en el gráfico simétrico, se observa diferencia en V_1 y G_1 
          También se ve alguna pequeña diferencia en P_1.\n\n")
    }
  }
  
  #Gráfico Simétrico - Si existencia de Rango y CRG.
  if (funcion == "H") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar que 5424 está muy cerca del R1. R2 y R3 ya está con menor intensidad.\n
            El grupo 6144 está más relacionado con los R3 al R6, aunque con ninguno mantiene una relación especialmente intensa. \n
            El grupo 7071 tiene una mayor relación con el R7.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar como el R7 tiene un cambio en el comportamiento, estando más cerca de 6144. Pero, 
          como 7071 está más bajo también, no me queda claro si se acerca más a 6144 o simplemente es consecuencia de la bajada 
          de 7071.\n\n")
    }
  }
  
  #Gráfico Simétrico - No existencia de ATC y Rango
  if (funcion == "I") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En general la no toma de medicamentos no está muy relacionada con la Edad.
          Cuanto mayor sea la edad menos probabilidad de no tomar medicamentos. \n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En esta gráfica también es visible el cambio en el R7. También se ve un pqueño cambo en R1 con respecto a C_0.\n\n")
    }
  }
  
  #Gráfico Simétrico - Si existencia de ATC y Rango
  if (funcion == "J") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Las familias B y C son las que más relación (presencia) tienen con los rango 
          de edad a partir del R3. Con el R1, al iugal que ocurría con el 5424, no hay ningún ATC que esté 
          especialmente relacionado.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Aquí se observan los cambios ya comentados de V_1, G_1 y de R7.\n\n")
    }
  }
  
}

pintaNotaAnalisis_G <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Cuando comenzamos a hablar de MAC y ATCs no pintaré ni el gráfico de mosaico 
          ni la tabla de contingencia. Esto es debido a que en realidad serían muchas variables para una tabla.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Cuando comenzamos a hablar de MAC y ATCs no pintaré ni el gráfico de mosaico 
          ni la tabla de contingencia. Esto es debido a que en realidad serían muchas variables para una tabla.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (1.0625) es mayor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa. Es decir, parece que hay una 
          correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Mantiene el mismo resultado del test de incercia que en 2011 (1.0625).\n\n")
    }
  }
  
  #Varianza
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se necesitan 12 dimensiones para explicar el 80% de la varianza.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se siguen necesitando 12 dimensiones para explicar el 80% de la varianza.\n\n")
    }
  }
  
  #Simétrico
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Parece que las variables de ATCs mantienen las distancias con respecto a lo visto 
          en el apartado anterior.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se observa que el V_1 ha bajado mucho más con respecto a 2011.\n\n")
    }
  }
  
  #Gráfico A-simétrico no existencia CRG-ATCs
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa la misma distribucion en Variables_0 y en CRGs.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Idem al punto anterior.\n\n")
    }
  }

  #Gráfico A-simétrico  existencia CRG-ATCs
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa la misma distribucion en Variables_1 y en CRGs que antes.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Idem al punto anterior.\n\n")
    }
  }
  
  #Gráfico Asimétrico CRGs-Edad
  if (funcion == "H") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se comprueba como el Sexo 2 está más relacionado con el grupo 7071 que el Sexo 1. 
          Esto es debido a que a mayor edad, se tiene más personas del Sexo 2 que del Sexo 1.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS:\n\n")
    }
  }
  
  #Gráfico Simétrico - No existencia de ATC y Sexo.
  if (funcion == "I") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar que en el género 1 está más relacionado con la no toma de medicamento y el género 2 está más relacionado 
          con la toma de medicamento en general.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar como el R7 tiene un cambio en el comportamiento, estando más cerca de 6144. Pero, 
          como 7071 está más bajo también, no me queda claro si se acerca más a 6144 o simplemente es consecuencia de la bajada 
          de 7071.\n\n")
    }
    }
  
  #Gráfico Simétrico - Si existencia de ATC y Rango
  if (funcion == "J") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Idem punto anterior. \n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En esta gráfica también es visible el cambio en el R7. También se ve un pqueño cambo en R1 con respecto a C_0.\n\n")
    }
    }
  
}

getRangoEdad <- function(csv, anyo) {
  cat("\t\t\n\n--> Rangos de Edad:\n\n")
  cat("\n\n-----> R1: [0, 49)\n\n")
  cat("\n\n-----> R2: [49, 55)\n\n")
  cat("\n\n-----> R3: [55, 59)\n\n")
  cat("\n\n-----> R4: [59, 63)\n\n")
  cat("\n\n-----> R5: [63, 68)\n\n")
  cat("\n\n-----> R6: [68, 78)\n\n")
  cat("\n\n-----> R7: [78, 103]\n\n")
  
  if (anyo == 2011) {
    #csv[csv$Edad >= 0 & csv$Edad < 49, "RangoEdad"] <- "[0, 49)"
    #csv[csv$Edad >= 49 & csv$Edad < 55, "RangoEdad"] <- "[49, 55)"
    #csv[csv$Edad >= 55 & csv$Edad < 59, "RangoEdad"] <- "[55, 59)"
    #csv[csv$Edad >= 59 & csv$Edad < 63, "RangoEdad"] <- "[59, 63)"
    #csv[csv$Edad >= 63 & csv$Edad < 68, "RangoEdad"] <- "[63, 68)"
    #csv[csv$Edad >= 68 & csv$Edad < 78, "RangoEdad"] <- "[68, 78)"
    #csv[csv$Edad >= 78 & csv$Edad <= 103, "RangoEdad"] <- "[78, 103]"
    csv[csv$Edad >= 0 & csv$Edad < 49 & csv$Anyo==2011, "RangoEdad"] <- "R1"
    csv[csv$Edad >= 49 & csv$Edad < 55 & csv$Anyo==2011, "RangoEdad"] <- "R2"
    csv[csv$Edad >= 55 & csv$Edad < 59 & csv$Anyo==2011, "RangoEdad"] <- "R3"
    csv[csv$Edad >= 59 & csv$Edad < 63 & csv$Anyo==2011, "RangoEdad"] <- "R4"
    csv[csv$Edad >= 63 & csv$Edad < 68 & csv$Anyo==2011, "RangoEdad"] <- "R5"
    csv[csv$Edad >= 68 & csv$Edad < 78 & csv$Anyo==2011, "RangoEdad"] <- "R6"
    csv[csv$Edad >= 78 & csv$Edad <= 103 & csv$Anyo==2011, "RangoEdad"] <- "R7"
  } 
  
  if (anyo == 2012) {
    csv[csv$Edad >= 0 & csv$Edad < 49 & csv$Anyo==2012, "RangoEdad"] <- "R1"
    csv[csv$Edad >= 49 & csv$Edad < 55 & csv$Anyo==2012, "RangoEdad"] <- "R2"
    csv[csv$Edad >= 55 & csv$Edad < 59 & csv$Anyo==2012, "RangoEdad"] <- "R3"
    csv[csv$Edad >= 59 & csv$Edad < 63 & csv$Anyo==2012, "RangoEdad"] <- "R4"
    csv[csv$Edad >= 63 & csv$Edad < 68 & csv$Anyo==2012, "RangoEdad"] <- "R5"
    csv[csv$Edad >= 68 & csv$Edad < 78 & csv$Anyo==2012, "RangoEdad"] <- "R6"
    csv[csv$Edad >= 78 & csv$Edad <= 103 & csv$Anyo==2012, "RangoEdad"] <- "R7"
  }
  
  return (csv)
}

getOrdenacion <- function(ac, anyo) {
  v <- c()
  if ( ac == "A.1" | ac == "A.2") {
    if (anyo == 2011) v <- c(6, 4, 7, 1, 9, 3, 5, 13, 10, 2, 12, 8, 11, 14)  
    #if (anyo == 2012) v <- c(6, 4, 7, 1, 9, 3, 2, 5, 10, 12, 13, 8, 11, 14)
    if (anyo == 2012) v <- c(6, 4, 7, 1, 9, 3, 5, 13, 10, 2, 12, 8, 11, 14) 
  }

  if ( ac == "B.1" | ac == "B.2") {
    if (anyo == 2011) v <- c(10, 6, 13, 9, 14, 8, 1, 3, 2, 12, 7, 5, 4, 11)  
    if (anyo == 2012) v <- c(10, 6, 13, 9, 14, 8, 1, 3, 2, 12, 7, 5, 4, 11)
  }  
  
  if ( ac == "C.1" | ac == "C.2") {
    if (anyo == 2011) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8)  
    if (anyo == 2012) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8) 
  }  

  if ( ac == "D") {
    if (anyo == 2011) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8)  
    if (anyo == 2012) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8) 
  }  
  
  
  return(v) 
}

EscribeTestMalinvaud <- function(p, a) {
  malinv.ca <- p
  malinv.test.rows <- a
  malinv.test.cols <- 6
  malinvt.output <-matrix(ncol= malinv.test.cols, nrow=malinv.test.rows)
  colnames(malinvt.output) <- c("K", "Dimension", "Eigen value", "Chi-square", "df", "p value")
  malinvt.output[,1] <- c(0:(a-1))
  malinvt.output[,2] <- c(1:a)
  
  for(i in 1:malinv.test.rows) {
    k <- -1+i
    malinvt.output[i,3] <- malinv.ca$eig[i,1]
    malinvt.output[i,5] <- (nrow(m)-k-1)*(ncol(m)-k-1)
  }
  
  malinvt.output[,4] <- rev(cumsum(rev(malinvt.output[,3])))*sum(m)
  malinvt.output[,6] <- round(pchisq(malinvt.output[,4], malinvt.output[,5], lower.tail=FALSE), digits=6)
  optimal.dimensionality <- length(which(malinvt.output[,6]<=0.05))
  
  cat(paste("\n\n --> Dimensionalidad Óptima según Malinvaud's Test:", optimal.dimensionality, "\n\n", sep=""))
  plot(malinvt.output[,6], xaxt="n", 
       xlim=c(1, a), xlab="Dimensiones", ylab="p value")
  axis(1, at=labs, labels=sprintf("%.0f",labs))
  title(main="Gráfica de Test de Mainvaud's", cex.sub=0.70,
        sub="línea punteada: límite alpha 0.05", cex.sub=0.50,
        col.sub="red")
  abline(h=0.05, lty=2, col="red")
  cat("\n\n")
  
  #liberamos las variables.
  rm(malinv.ca)
  rm(malinv.test.rows)
  rm(malinv.test.cols)
  rm(malinvt.output)
  rm(i)
  rm(k)
  rm(optimal.dimensionality)
}
