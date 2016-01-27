
#Carga de librerías.
suppressPackageStartupMessages(library(ca))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(vcd))

#Importaciones de otros ficheros.
source("PCAs.R")

getCRGDiabHipertension <- function(anyo) {
  #Obtenemos los datos del año.
  dx <- subset(csv, Anyo == anyo)
  
  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "SUMA_FAMILIAS")
  m_sumaFamilias <- cbind(m_sumaFamilias, dx$CRG)
  colnames(m_sumaFamilias)[15] <- "CRG"
  
  #Le concatenamos el código de CRG para filtrar por los códigos que queremos. 
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
  
  #Eliminamos variables
  rm(dx)
  rm(m_sumaFamilias)
  rm(m_6144)
  rm(m_5424)
  rm(m_7071)
  rm(i)
  rm(a)
  rm(s)
  
  return(m)
}







#dx <- subset(csv, Anyo == 2011)
#
#msumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "SUMA_FAMILIAS")
#msumaFamilias <- as.data.frame(cbind(msumaFamilias, dx$CRG, dx$Sexo))
#colnames(msumaFamilias)[15:16] <- c("CRG","Sexo")
#msumaFamilias <- as.data.frame(subset(msumaFamilias, msumaFamilias[,"CRG"] %in% c(5424, 6144, 7071)))
#ms <- sqldf("select CRG, Sexo, sum(A) as A, sum(B) as B, sum(C) as C, sum(D) as D, sum(G) as G, sum(H) as H, 
#            sum(J) as J, sum(L) as L, sum(M) as M, sum(N) as N, sum(P) as P, sum(R) as R, sum(S) as S, sum(S) as V
#            from msumaFamilias group by CRG, Sexo")
#colnames(ms)
#rownames(ms) <- c("5424_1", "5424_2", "6144_1", "6144_2", "7071_1", "7071_2")
#ms <- ms[,!colnames(ms) %in% c("CRG", "Sexo")]
#
#mosaicplot(ms, shade = TRUE, 
#           main = "CRGs vs ATCs")
#
#
#CA(m, ncp = 5, graph = TRUE)
#
#p <- CA(ms, ncp = 5, graph = FALSE)
#p$col$coord
#p$row$coord
#xmin = range(p$row$coord[,1], p$col$coord[,1])[1] * 1.1
#plot(p$row$coord, type = "p", pch = 16, col="blue",
#     xlim=range(p$row$coord[,1], p$col$coord[,1]) * 1.1,
#     ylim=range(p$row$coord[,2], p$col$coord[,2]) * 1.1,
#     xlab="Dimensión 1", ylab="Dimensión 2",
#     main = "Análisis de Correspondencia de CRG-Base Diabético e Hipertensión y Familias de ATC", cex.main=0.7
#)
#points(p$col$coord, type = "p", pch = 17, col="red")
#for (i in 1:nrow(m)) {
#  text(p$row$coord[i,1] + 0.015, p$row$coord[i,2] + 0.03, rownames(m)[i], col="blue", cex=0.6)
#}
#for (i in 1:ncol(m)) {
#  text(p$col$coord[i,1] + 0.015, p$col$coord[i,2] + 0.03, colnames(m)[i], col="red", cex=0.6)
#}
#abline(h=0,lty=2)
#abline(v=0,lty=2) 
#

###################################
##### Funciones Datos Ficticios
###################################

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
            " min(número columnas - 1, número de filas - 1) [",numb.dim.cols,",",numb.dim.rows, "]\n\n",sep=""))
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

EscribeContribucionDimension <- function(m) {
  #Pintamos las dimensiones según el Test de Molinvaud. 
  dims.to.be.plotted <- 2
  
  # AC del paquete estándar.
  res.ca <- ca(m, nd=dims.to.be.plotted)
  
  # AC almacenamos el resultado para pintarlo después.
  cadataframe <- summary(ca(m, nd=dims.to.be.plotted))
  
  cat("\n\n")
  cat("####Distribución de la contribución de las categorías\n\n")
  cat("\n\n<b>Filas</b>\n\n")
  
  par(mfrow=c(1,2), mar=c(4,4,3,3))
  counter <- 0
  for(i in seq(7, ncol(cadataframe$rows), 3)) {
    counter <- counter +1
    max = max(cadataframe$rows[,i]/10) + 10
    max = min(max, 100)
    barplot(cadataframe$rows[,i]/10, ylim=c(0,max), col="steelblue",
            xlab="Categorías de Filas", 
            ylab=paste("% Contribución a Dim.", counter), 
            names.arg=rownames(m), cex.lab=0.8, las=1, cex.axis = 0.7, cex.names = 0.8)
    abline(h=round(((100/nrow(m))), digits=0), col="red", lty=2)
  }
  
  cat("\n\n<b>Columnas</b>\n\n")
  counter <- 0
  for(i in seq(7, ncol(cadataframe$columns), 3)){    
    counter <- counter +1
    max = max(cadataframe$columns[,i]/10) + 10
    max = min(max, 100)    
    barplot(cadataframe$columns[,i]/10, ylim=c(0,max), col="steelblue",
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

EscribeGraficoSimetrico <- function(p) {
  cat("\n\n")
  cat("####Salida del Análisis de Correspondencia\n\n")
  cat("\n\n<b>Gráfico Simétrico</b>\n\n")
  xmin = min(p$row$coord[,1], p$col$coord[,1])[1] * 1.2
  xmax = max(p$row$coord[,1], p$col$coord[,1])[1] * 1.2
  ymin = min(p$row$coord[,2], p$col$coord[,2])[1] * 1.2
  ymax = max(p$row$coord[,2], p$col$coord[,2])[1] * 1.2
  plot(p$row$coord, type = "p", pch = 16, col="blue",
     xlim=c(xmin, xmax),
     ylim=c(ymin, ymax),
     xlab="Dimensión 1", ylab="Dimensión 2",
     main = "X vs Z", cex.main=0.7
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

  rm(xmin)
  rm(xmax)
  rm(ymin)
  rm(ymax)
  rm(i)
}

EscribeGraficosAsimetricos <- function(p, dims.to.be.plotted, map) {
  cat("\n\n <b>Gráfico de relación bi-variable A-Simétrico</b> \n\n")
  
  arr <- c(TRUE, FALSE)
  if (map == "colprincipal" | map == "colgreen") { 
    arr <- c(FALSE, TRUE)
  }
  
  #counter <- 1
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
