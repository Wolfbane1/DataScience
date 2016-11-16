
#Carga de librerías.
suppressPackageStartupMessages(library(ca))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(vcd))

#Importaciones de otros ficheros.
source("PCAs.R")

atcImportantes <- c("A02BC","A03FA","A06AD","A10BA","A10AE","B01AC","C03CA","C08CA","C09AA","C10AA","G04CA","J01MA","M02AA",
                    "N02BB","N02BE","N05AL", "N05BA", "R03AC","R03AK","R03BB","R05CB")

getCRGDiabHipertensionAtc <- function(csv, anyo, tipo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo)
  
  #Filtramos por los ATCs importantes y añadimos el CRG.
  #  Si es "Suma" no convertimos los ATCs a existencia.
  if (tipo == "SUMA_FAMILIAS" ) m_sumaFamilias <- dx[, c(atcImportantes, "CRG")]
  if (tipo == "EXISTENCIA") {
    m_sumaFamilias <- apply(dx[, atcImportantes], 2, convierteExistencia)
    m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG)
    colnames(m_sumaFamilias)[ncol(m_sumaFamilias)] <- c("CRG")
  }
   
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

  if (tipo == "SUMA_FAMILIAS" ) m_sumaFamilias <- dx[, c(atcImportantes, "Sexo")]
  if (tipo == "EXISTENCIA") {
    m_sumaFamilias <- apply(dx[, atcImportantes], 2, convierteExistencia)
    m_sumaFamilias <- cbind(m_sumaFamilias, dx$Sexo)
    colnames(m_sumaFamilias)[ncol(m_sumaFamilias)] <- c("Sexo")
  }
  
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
  
  m_sumaFamilias <- apply(dx[, atcImportantes], 2, convierteExistencia)
  m_sumaFamilias <- cbind(as.data.frame(m_sumaFamilias), dx$CRG, dx$RangoEdad)
  colnames(m_sumaFamilias)[(ncol(m_sumaFamilias)-1):ncol(m_sumaFamilias)] <- c("CRG", "RangoEdad")
  
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
  
  m_sumaFamilias <- apply(dx[, atcImportantes], 2, convierteExistencia)
  m_sumaFamilias <- cbind(as.data.frame(m_sumaFamilias), dx$CRG, dx$Sexo)
  colnames(m_sumaFamilias)[(ncol(m_sumaFamilias)-1):ncol(m_sumaFamilias)] <- c("CRG", "Sexo")

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
  
  m_sumaFamilias <- apply(dx2[, atcImportantes], 2, convierteExistencia)
  m_sumaFamilias <- cbind(as.data.frame(m_sumaFamilias), dx$CRG_12)
  colnames(m_sumaFamilias)[ncol(m_sumaFamilias)] <- c("CRG_12")

  EscribeDimensionalidadTabla(m_sumaFamilias)
  
  #Eliminamos variables
  rm(dx)
  rm(dx2)
  
  return(as.data.frame(m_sumaFamilias))
}



getAtcEdad <- function(csv, anyo, tipo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, csv$Anyo == anyo)

  if (tipo == "SUMA_FAMILIAS" ) m_sumaFamilias <- dx[, c(atcImportantes, "RangoEdad")]
  if (tipo == "EXISTENCIA") {
    m_sumaFamilias <- apply(dx[, atcImportantes], 2, convierteExistencia)
    m_sumaFamilias <- cbind(as.data.frame(m_sumaFamilias), dx$RangoEdad)
    colnames(m_sumaFamilias)[ncol(m_sumaFamilias)] <- c("RangoEdad")
  }
  
  #Le concatenamos el código de CRG para filtrar por los códigos que queremos. 
  
  m_R1 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R1", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R1) <- c("[0, 49)")
  colnames(m_R1) <- c("R1")
  m_R2 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R2", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R2) <- c("[49, 55)")
  colnames(m_R2) <- c("R2")
  m_R3 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R3", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R3) <- c("[55, 59)")
  colnames(m_R3) <- c("R3")
  m_R4 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R4", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R4) <- c("[59, 63)")
  colnames(m_R4) <- c("R4")
  m_R5 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R5", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R5) <- c("[63, 68)")
  colnames(m_R5) <- c("R5")
  m_R6 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R6", 1:length(atcImportantes)), 2, sum))
  #colnames(m_R6) <- c("[68, 78)")
  colnames(m_R6) <- c("R6")
  m_R7 <- as.data.frame(apply(subset(m_sumaFamilias, m_sumaFamilias[,"RangoEdad"] == "R7", 1:length(atcImportantes)), 2, sum))
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
  mosaicplot(m1, shade = TRUE, main = main, las=2)
  
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
  
  #ATCs (atcsImportantes), CRGs (5424, 6144, 7071) y Rango de Edad
  if (ac == "F") pintaNotaAnalisis_F(funcion, anyo, i, j) 
  
  
  
  
}

pintaNotaAnalisis_A1 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que para el medicamento N05AL hay valores a 0. 
          Esto nos va a afectar a la hora de invocar al método de Chi^2. TO-DO: ¿Utilizamos el método Fisher?.")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que para el medicamento N05AL hay valores a 0. 
          Esto nos va a afectar a la hora de invocar al método de Chi^2. TO-DO: ¿Utilizamos el método Fisher?.")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("NOTA ANÁLISIS: Como primer vistazo se puede identificar que al utilizar todos los ATCs se tiene más 
          detalle de relación de ciertos ATCs con respecto a la agrupación a la primera familia. Por ejemplo:\n\n")
      cat("a) A02BC tiene más frecuencia en el 7071 que en los otros dos CRG-bases. Esto quedaba 'oculto' en los resultados 
            anteriores que teníamos.\n\n")
      cat("b) Aunque C10AA está también relacionado con el CRG-base 5424 en realidad parece que el resto de ATCs 
            de la familia C se relacionan más con el CRG-base 6144.\n\n")
      cat("c) La familia Nxxxx parece que tiene más relación con el CRG-base 7071.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Si comparamos el gráfico mosáico con el correspondiente al año 2011 se puede
          observar que en general sigue la misma estructura de patrones para los ATCs, aunque se observan dos 
          pequeños cambios:\n\n
          a) A10BA: En 2012 se pone rojo en su relación con 6144.\n\n
          b) A02BC: En 2012 toma cierta relación con 6144.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.1218) es menor a la 'regla gorda' que indica que 
          si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANALISIS: En este caso la inercial (0.1184) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 16.556.8 con 30 grados de libertad. Según las tablas 
            de Chi^2 X^2[0.05](30) = 43,75. Como 16.556 >> 44 debería haber suficiente evidencia en contra de 
            que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables, aunque como hay una 
            fila que tiene menos de 5 elementos no puede afectar al resultado.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 16.675.2 con 30 grados de libertad. Según las tablas 
            de Chi^2 X^2[0.05](30) = 43,75. Como 16.556 >> 44 debería haber suficiente evidencia en contra de 
            que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables, aunque como hay una 
            fila que tiene menos de 5 elementos no puede afectar al resultado.\n\n")
    }
  }
  
  #Contribución de la Dimensión
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA TEORÍA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA TEORÍA: La línea roja marca la contribución media esperada. Si la contribución de filas/columnas fueran uniforme, el valor esperado sería (en %) 1/número de filas de la matriz. Para tres filas sería 1/3 = 33%. Para cualquier dimensión, una fila/columna con una contribución mayor que este límite podría ser considerada como importante en la contribución a esa dimensión.\n\n")
    }
  }
  
  #Gráfico Simétrico
  if (funcion == "F") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa que el ATC N05AL que no tiene valores en 5424 y en 6144 y muy poquitos en 
          7071 (14 para ser exactos) está muy lejos del resto de ATCs. Por otro lado, se puede observar que hay varios 
          códigos de ATC muy cercanos entre sí: Nxxxx.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: El principal cambio que se observa en el gráfico es que el J01CR ha cambiado de 
          cuadrante acercándose al M01AE. Los ATCs NXXXX están más próximos entre sí.\n\n")
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
      cat("\t\ta) Los ATCs que están más asociados al 6144 son B01AC, C09AA y C08AA.\n\n
          b) Los ATCs que están más asociados al 5424 son M01AE y A10AE..\n\n
          c) Los ATCs que está más asociados con el 7071 son NXxx, A02BC y M02AA.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: A parte del cambio del ATC J01CR, el otro cambio que se observa es una pequeña 
          variación en el ángulo del 5424.\n\n")
    }
  }
}

pintaNotaAnalisis_A2 <- function(funcion, anyo, i, j) {
  #Dimensionalidad
  if ( funcion == "A") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que para el medicamento N05AL hay valores a 0. 
          Esto nos va a afectar a la hora de invocar al método de Chi^2. TO-DO: ¿Utilizamos el método Fisher?.")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En la tabla de contingencia se observa que para el medicamento N05AL hay valores a 0. 
          Esto nos va a afectar a la hora de invocar al método de Chi^2. TO-DO: ¿Utilizamos el método Fisher?.")
    }
  }
  
  #Mosaico.
  if ( funcion == "B") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Como primer vistazo se puede identificar que el gráfico genera diferente asociación por frecuencias 
          entre los ATCs y los CRG-base. Predomina mucho menos el color rojo habiendo más celdas con valores 
          dentro de una frecuencia esperada. El ATC que más cambio representa es el C10AA que ahora solo 
          aparece asociado al 5424 y no muy intensamente.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: ATC A02BC se relaciona con algo más de intensidad con el CRG-base 6144 que en 2011. El 
          otro cambio es que el J01CR solo se relaciona con el 5424.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.1113) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.115) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 2.697,7 con 30 grados de libertad. Según las tablas 
          de Chi^2 X^2[0.05](30) = 43,73. Como 2.697 >> 43 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de \n\n
            que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables, aunque como hay una 
            fila que tiene menos de 5 elementos no puede afectar al resultado.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 2.766,3 con 30 grados de libertad. Según las tablas 
          de Chi^2 X^2[0.05](30) = 43,73. Como 2.766 >> 43 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de \n\n
            que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables, aunque como hay una 
            fila que tiene menos de 5 elementos no puede afectar al resultado.\n\n")
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
      cat("\t\tNOTA ANÁLISIS: Se observa que el ATC J01CR ha cambiado de cuadrante pasando a estar en el mismo 
          cuadrante que el 5424. En general se observan que los ATCs Nxxx y Rxxx están muy próximos por lo que se 
          visualiza más la relación entre ellos.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No se observa que haya diferencias con respecto a 2011.\n\n")
    }
  }
  
  #Gráfico Asimétrico
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se observa lo mismo que en la gráfica simétrica.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No se observa diferencias con respecto a 2011.\n\n")
      
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
      cat("\t\tNOTA ANÁLISIS: En este gráfico se observa que hay una mayor asociación en el Sexo 2 con aquellos 
          ATCs que están más asociados, según vimos antes, al grupo CRG-base 7071. En cambio el Sexo 1 está más 
          relacionado con el grupo CRG-base 5424 y 6144.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: El principal cambio entre 2011 y 2012 que se observa es para el ATC C08CA que en 
          2011 parece que tiene más frecuencia relativa a lo esperado con el Sexo 2 y en 2012 es con el Sexo 2.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0178) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0227) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 6.616,7 con 15 grados de libertad. Según las tablas de Chi^2 X^2[0.05](15) = 22,3071. Como 6.616 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 7.937,5,6 con 15 grados de libertad. Según las tablas de Chi^2 X^2[0.05](15) = 22,3071. Como 7.973 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
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
      cat("\t\tNOTA ANÁLISIS: Si comparamos a la suma de dispensaciones, se observan varios cambios:\n\n
          a) Los ATCs R05CB, J01CR, A02BC, M01AE y C08CA pierde la significancia en relación al sexo.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No se observa cambio con respecto a 2011.\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0167) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0176) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 766,8 con 15 grados de libertad. Según las tablas de Chi^2 X^2[0.05](15) = 22,3620. Como 766,8 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
          Nótese que el método indica que hay dependencia entre las variables, pero, como solo se muestra 
          una única dimensión ahí nos quedamos. Para este caso, el Test de Mainvaud's también indica que hay dependencia.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 875,9 con 15 grados de libertad. Según las tablas de Chi^2 X^2[0.05](15) = 22,3620. Como 875 >> 22 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n
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
      cat("\t\tNOTA ANÁLISIS: Se observa que conforme aumenta la edad, toman fuerza relaciones con ATCs más asociados al grupo 7071.n:\n
          Nótese que lo que indico es la parte 'azul', es decir, aquello donde hay más frecuencia de la esperada.\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Si comparamos con 2011 vemos que el cambio más significativo (que es poco) tenemos.\n\n") 
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0621) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0677) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 23.054,15 con 90 grados de libertad. Según las tablas de Chi^2 X^2[0.05](90) = 124,3421. Como 23.054 >> 124 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 23.705,6 con 90 grados de libertad. Según las tablas de Chi^2 X^2[0.05](90) = 124,3421 Como 23.705 >> 90 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
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
          A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2 y R3 parece que no están muy lejos entre sí.\n 
          c) R4 y R5 parece que no están muy lejos entre sí.\n
          c) R6 \n
          d) R7 \n
          A nivel de filas (ATCs) no hay grupos claros.
          a) N02BE y J01CR parece que están en un cluster.\n
          b) R05CB y M02AA parece que están en un cluster.\n
          c) C08CA y B01AC parece que están separadas del resto.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Comparando con 2011 se tienen los siguientes:\n
          A nivel de columnas (Rango de Edad) se puede observar lo siguiente:\n
          a) R1 \n
          b) R2 y R3 parece que no están muy lejos entre sí.\n 
          c) R4 y R5 parece que no están muy lejos entre sí. R4 no parece claro si cae cerca de R2 o de R3.\n
          c) R6 \n
          d) R7 \n
          A nivel de filas (ATCs) no hay grupos claros.
          a) N02BE y J01CR ya no están en el cluster.\n
          b) R05CB y M02AA parece que están en un cluster. Ahora están muy cercas de N02BE.\n
          c) C08CA y B01AC parece que están separadas del resto.\n\n")
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
      cat("\t\tNOTA ANÁLISIS: El cambio con respecto a la suma de disposiciones es visible. Tiene menos celdas rojas. Pero no hay grandes cambios en la parte de azul.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Con respecto a 2011 se observan pequeñas variaciones en relación a la intensidad.:\n\n")
    }
  }
  
  #Test de Inercia
  if (funcion == "C") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0441) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (0.0518) es menor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa.\n\n")
    }
  }
  
  #Test de CHI^2
  if (funcion == "D") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 2.029,0199 con 90 grados de libertad. Según las tablas de Chi^2 X^2[0.05](90) = 124,321. Como 2.029 >> 124 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Tenemos un estadístico de 2.572,6 con 90 grados de libertad. Según las tablas de Chi^2 X^2[0.05](90) = 124,321. Como 2.572 >> 124 (además se tiene un p-value de 0)  hay suficiente evidencia en contra de que la hipótesis nula sea cierta. Es decir, hay dependencia entre las variables.\n\n")
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
      cat("\t\tNOTA ANÁLISIS: Sí que se observan cambios con respecto a 2011:\n
          a) R1. En este cuadrante aparece J01CR.\n
          b) R2, R3 y R4 están juntos en el cuadrante.\n 
          c) R5 y R6 ahora están en cuadrantes distintos. R6 pasa al cuadrante del R7.\n
          d) R7 \n
          A nivel de filas (ATCs) también se observan algunos pequeños cambios con respecto al total de dispensaciones.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: Con respecto a 2011 se observan pequeñas variaciones, la más significativa es el ATC N05AL que cambia de cuadrante.\n\n") 
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
      cat("\t\tNOTA ANÁLISIS: En este caso la inercial (1.3333) es mayor a la 'regla gorda' que indica que si es > 0.2 hay mucha significancia estadística para indicar que hay una correlación significativa. Es decir, parece que hay una 
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
      cat("\t\tNOTA ANÁLISIS: En 2012 sigue haciendo falta 16 dimensiones para explicar el 80% de la varianza.\n\n")
    }
  }
  
  #Simétrico
  if (funcion == "E") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En este gráfico se representan las variables. Como ocurría con el AC en teoría 
          solo sirve para poder comparar entre las distintas variables. 
          Lo primero que se intuye es que parece 
          que el grupo 5424 no se relaciona con ningún elemento de ocurrencia de medicamento y sin embargo, se
          relaciona mucho con el rango de edad R1.\n
          \n\n")
    }
    
    if (anyo == 2012) {
    }
  }
  
  #Gráfico Simétrico - No existencia de ATCs con Rango y CRG.
  if (funcion == "F")  {
    if (anyo == 2011)  {
      cat("\t\tNOTA ANÁLISIS: Se observa que el CRG-base 5424 aparece más relacionado con la no toma de los distintos ATCs. 
        El grupo 6144 parece estar más relacionado con la no toma del ATC A10AE. El grupo 7071 está poco relacionado con la no toma de ATCs.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No hay diferencias visuales en 2012 con respecto 2011.\n\n")
    }
  }
  
  #Gráfico Simétrico - Si existencia de ATCs con Rango y CRG.
  if (funcion == "G") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: Se puede observar que 5424 está muy relacionado con la toma de A10AE. El grupo 6144 está 
        más relacionado con la toma de C08CA, C09CA, C10AA, B01AC y A10BA. El grupo 7071 con el resto.\n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No hay diferencias visuales en 2012 con respecto 2011.\n\n")
    }
  }
  
  #Gráfico Simétrico - No existencia de ATC y Rango
  if (funcion == "I") {
    if (anyo == 2011) {
      cat("\t\tNOTA ANÁLISIS: En general la no toma de medicamentos parece estar relacionada con la edad. \n\n")
    }
    
    if (anyo == 2012) {
      cat("\t\tNOTA ANÁLISIS: No hay diferencias en relación a 2011.\n\n")
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
      cat("\t\tNOTA ANÁLISIS: No hay diferencias en relación a 2011.\n\n")
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
    if (anyo == 2011) v <- c(2, 9, 3, 7, 6, 4, 5, 1, 10, 15, 13, 12, 11, 16, 8, 14, 17, 18, 19, 20, 21)  
    if (anyo == 2012) v <- c(2, 9, 3, 7, 6, 4, 5, 1, 10, 15, 13, 12, 11, 16, 8, 14, 17, 18, 19, 20, 21)  
  }

  if ( ac == "B.1" | ac == "B.2") {
    if (anyo == 2011) v <- c(6, 3, 7, 4, 16, 2, 8, 1, 13, 15, 11, 12, 9, 10, 5, 14, 17, 18, 19, 20, 21)
    if (anyo == 2012) v <- c(6, 3, 7, 4, 16, 2, 8, 1, 13, 15, 11, 12, 9, 10, 5, 14, 17, 18, 19, 20, 21)
  }  
  
  if ( ac == "C.1" | ac == "C.2") {
    if (anyo == 2011) v <- c(3, 9, 2, 6, 7, 4, 5, 1, 13, 10, 12, 10, 15, 16, 8, 14, 17, 18, 19, 20, 21)  
    if (anyo == 2012) v <- c(3, 9, 2, 6, 7, 4, 5, 1, 13, 10, 12, 10, 15, 16, 8, 14, 17, 18, 19, 20, 21)
  }  

  if ( ac == "D") {
    if (anyo == 2011) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8, 15, 16, 17, 18, 19, 20, 21)  
    if (anyo == 2012) v <- c(7, 6, 13, 5, 9, 1, 2, 3, 12, 10, 4, 11, 14, 8, 15, 16, 17, 18, 19, 20, 21) 
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
