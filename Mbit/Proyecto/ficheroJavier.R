#Librería que lee la estructura de un fichero de matlab.
library(R.matlab) 

setwd("/Users/zzddfge/Desktop/Compartida/Proyecto_Master")
origenData <- file.path(getwd(), "data")
origenMat <- file.path(getwd(), "data", "javier", "diag")

##########
###Ficheros CAs balanceados
##########

###Analisis 1
origenSanos <- file.path(origenMat, "diagn_age_analisis1.mat")
mat <- readMat(origenSanos)

as.matrix(mat$Matrix.CA.diag.age.11.A1.VF[1])

diag_age_11 <- matrix(unlist(mat$Matrix.CA.diag.age.11.A1.VF[1]), nrow= 9, ncol=14)
rownames(diag_age_11) <- unlist(mat$Matrix.CA.diag.age.11.A1.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.CA.diag.age.11.A1.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y EDAD para", 2011))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

diag_age_12 <- matrix(unlist(mat$Matrix.CA.diag.age.12.A1.VF[1]), nrow= 9, ncol=14)
rownames(diag_age_12) <- unlist(mat$Matrix.CA.diag.age.12.A1.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.CA.diag.age.12.A1.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y EDAD para", 2012))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

rm(diag_age_11)
rm(diag_age_12)

###Analisis 2
origenSanos <- file.path(origenMat, "Matrix_CA_diagn_age_ANALISIS2.mat")
mat <- readMat(origenSanos)

diag_age_11 <- matrix(unlist(mat$Matrix.CA.diag.age.11.A2.VF[1]), nrow= 10, ncol=18)
rownames(diag_age_11) <- unlist(mat$Matrix.CA.diag.age.11.A2.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.CA.diag.age.11.A2.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y EDAD para", 2011))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

diag_age_12 <- matrix(unlist(mat$Matrix.CA.diag.age.12.A2.VF[1]), nrow= 10, ncol=17)
rownames(diag_age_12) <- unlist(mat$Matrix.CA.diag.age.12.A2.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.CA.diag.age.12.A2.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y EDAD para", 2012))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

rm(diag_age_11)
rm(diag_age_12)

###Analisis 3
origenSanos <- file.path(origenMat, "Matrix_CA_diagn_CRG_ANALISIS1.mat")
mat <- readMat(origenSanos)

diag_age_11 <- matrix(unlist(mat$Matrix.CA.diag.CRG.11.A1.VF[1]), nrow= 15, ncol=4)
rownames(diag_age_11) <- unlist(mat$Matrix.CA.diag.CRG.11.A1.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.CA.diag.CRG.11.A1.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y CRG para", 2011))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

diag_age_12 <- matrix(unlist(mat$Matrix.CA.diag.CRG.12.A1.VF[1]), nrow= 15, ncol=4)
rownames(diag_age_12) <- unlist(mat$Matrix.CA.diag.CRG.12.A1.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.CA.diag.CRG.12.A1.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - DIAGs y CRG para", 2012))
EscribeGraficosAsimetricos(p, 2, "colprincipal")

rm(diag_age_11)
rm(diag_age_12)

###Analisis 4
origenSanos <- file.path(origenMat, "Matrix_CA_diagn_CRG_ANALISIS2.mat")
mat <- readMat(origenSanos)

diag_age_11 <- matrix(unlist(mat$Matrix.CA.diag.CRG.11.A2.VF[1]), nrow= 18, ncol=2)
rownames(diag_age_11) <- unlist(mat$Matrix.CA.diag.CRG.11.A2.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.CA.diag.CRG.11.A2.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2011, "Diags y CRG")


diag_age_12 <- matrix(unlist(mat$Matrix.CA.diag.CRG.12.A2.VF[1]), nrow= 17, ncol=2)
rownames(diag_age_12) <- unlist(mat$Matrix.CA.diag.CRG.12.A2.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.CA.diag.CRG.12.A2.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2012, "Diags y CRG")

rm(diag_age_11)
rm(diag_age_12)


###Analisis 5
origenSanos <- file.path(origenMat, "Matrix_CA_diagn_sex_ANALISIS1.mat")
mat <- readMat(origenSanos)

diag_age_11 <- matrix(unlist(mat$Matrix.11.CA.diag.sex.A1.VF[1]), nrow= 14, ncol=2)
rownames(diag_age_11) <- unlist(mat$Matrix.11.CA.diag.sex.A1.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.11.CA.diag.sex.A1.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2011, "Diags y Sexo")

diag_age_12 <- matrix(unlist(mat$Matrix.12.CA.diag.sex.A1.VF[1]), nrow= 14, ncol=2)
rownames(diag_age_12) <- unlist(mat$Matrix.12.CA.diag.sex.A1.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.12.CA.diag.sex.A1.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2012, "Diags y Sexo")

###Analisis 6 
origenSanos <- file.path(origenMat, "Matrix_CA_diagn_sex_ANALISIS2.mat")
mat <- readMat(origenSanos)

diag_age_11 <- matrix(unlist(mat$Matrix.11.CA.diag.sex.A2.VF[1]), nrow= 18, ncol=2)
rownames(diag_age_11) <- unlist(mat$Matrix.11.CA.diag.sex.A2.VF[2])
colnames(diag_age_11) <- unlist(mat$Matrix.11.CA.diag.sex.A2.VF[3])

m <- diag_age_11 
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2011, "Diags y Sexo")

diag_age_12 <- matrix(unlist(mat$Matrix.12.CA.diag.sex.A2.VF[1]), nrow= 17, ncol=2)
rownames(diag_age_12) <- unlist(mat$Matrix.12.CA.diag.sex.A2.VF[2])
colnames(diag_age_12) <- unlist(mat$Matrix.12.CA.diag.sex.A2.VF[3])

m <- diag_age_12
p <- CA(m, graph = FALSE)

pintaSimetrico2Dimensiones(m, 2012, "Diags y Sexo")

##########
###Fichero inicial CA.
##########


#####2011
crg0_11 <- matrix(mat$vect.CA.Healthy.11, nrow= 20, ncol=1)
crg5_11 <- matrix(mat$vect.CA.CRG5.11, nrow= 20, ncol=1)
crg6_11 <- matrix(mat$vect.CA.CRG6.11, nrow= 20, ncol=1)
crg7_11 <- matrix(mat$vect.CA.CRG7.11, nrow= 20, ncol=1)

h11 <- cbind(crg0_11, crg5_11, crg6_11, crg7_11)
colnames(h11) <- c("CRG0", "CRG5", "CRG6", "CRG7")
rownames(h11) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")

m <- h11
p <- CA(h11, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - CRGs-DIAGs para", 2011))
EscribeGraficosAsimetricos(p, 2, "colprincipal")


#####2012
crg0_12 <- matrix(mat$vect.CA.Healthy.12, nrow= 20, ncol=1)
crg5_12 <- matrix(mat$vect.CA.CRG5.12, nrow= 20, ncol=1)
crg6_12 <- matrix(mat$vect.CA.CRG6.12, nrow= 20, ncol=1)
crg7_12 <- matrix(mat$vect.CA.CRG7.12, nrow= 20, ncol=1)

h12 <- cbind(crg0_12, crg5_12, crg6_12, crg7_12)
colnames(h12) <- c("CRG0", "CRG5", "CRG6", "CRG7")
rownames(h12) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20")

m <- h12
p <- CA(h12, graph = FALSE)

EscribeGraficoSimetrico(p, paste("Gráfico Simétrico - CRGs-DIAGs para", 2012))
EscribeGraficosAsimetricos(p, 2, "colprincipal")






##########
##### Funciones Auxiliares
##########
pintaSimetrico2Dimensiones <- function(m, anyo, tipo) {
  par(mfrow=c(1,1), mar=c(4,4,2,2))
  counter <- 1
  numb.dim.cols<-ncol(m)-1
  numb.dim.rows<-nrow(m)-1
  a <- min(numb.dim.cols, numb.dim.rows)
  p$col$coord <- cbind(p$col$coord, rep(0, 2))
  xmin = min(p$row$coord, p$col$coord) * 1.1
  xmax = max(p$row$coord, p$col$coord) * 1.1
  plot(p$row$coord, rep(0, length(p$row$coord)), type = "p", pch = 16, col="blue",xlab="Dimensión 1",
       ylab="", main = paste("[", anyo, "] - ", tipo, sep=""), cex.main=0.7, xlim = c(xmin, xmax))
  points(p$col$coord, type = "p", pch = 17, col="red")
  for (i in 1:nrow(m)) {
    text(p$row$coord[i], 0.20, rownames(m)[i], col="blue", cex=0.6)
  }
  for (i in 1:ncol(m)) {
    text(p$col$coord[i], p$col$coord[i] + 0.15, colnames(m)[i], col="red", cex=0.6)
  }
  abline(h=0,lty=2)
  abline(v=0,lty=2) 
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


