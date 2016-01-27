
#####APLICACIÓN DEL EJEMPLO DE INTERNET A:
#- Filas: Relación entre CRG-5424 y CRG-6144.
#- Columnas: 14 valores de A. 

source("PCAs.R")

#1.- GENERACIÓN DE TABLA DE COMNTINGENCIA
  #Reducimos la matriz de ATCs para obtener sus variables asociadas y. 
  dx <- subset(csv, Anyo == 2011)
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
  
  #Eliminamos variables
  rm(dx)
  rm(m_sumaFamilias)
  rm(m_6144)
  rm(m_5424)
  rm(m_7071)
  
  mosaicplot(m, shade = TRUE,
             main = "Tabla de Contingencia CRG-base / Familias ATCs dispensados")
  
  #NOTAS: 
  # 1.- La superficio de un elemento indica la magnitud relativa de su valor.
  # 2.- Color azul indica que el valor observado es mayor que el valor esperado.
  # 3.- Color rojo indica que el valor observado es menor que el valor esperado.
  
  #none light medium heavy sum
  #SM 4 2 3 2  
  #JM 4 3 7 4  
  #SE 25 10 12 4  
  #JE 18 24 33 13  
  #SC 10 6 7 2 
 # m <- matrix(c(4,2,3,2,4,3,7,4,25,10,12,4,18,24,33,13,10,6,7,2), nrow= 5)
  m <- matrix(c(4,4,25,18,10,2,3,10,24,6,3,7,12,33,7,2,4,4,13,2), nrow= 5)
  colnames(m) <- c("none", "light", "medium", "heavy")
  rownames(m) <- c("SM", "JM", "SE", "JE", "SC")
  
#2.- CÁLCULO DE PORCENTAJES
  
  #Variables con los totales. 
  ncol <- ncol(m)
  nrow <- nrow(m)

  total         <- sum(m)
  total.fila    <- apply(m, 1, sum)
  total.columna <- apply(m, 2, sum)
  
  #datos Master
  xrtot <- cbind(total.fila, total.fila, total.fila)
  xctot <- rbind(total.columna, total.columna, total.columna, total.columna, total.columna, total.columna,
                 total.columna, total.columna, total.columna, total.columna, total.columna, total.columna,
                 total.columna, total.columna)

  #Datos fumadores
  xrtot <- cbind(total.fila, total.fila, total.fila, total.fila)
  xctot <- rbind(total.columna, total.columna, total.columna, total.columna, total.columna)
  
  xrtot  <- m              / xrtot
  xctot  <- m              / xctot
  rdot   <- total.fila     / total
  cdot   <- total.columna  / total

#3.- CÁLCULO DE MATRICES DE DISTANCIAS
  
  # Se calculan las matrices de distancias entre columnas
  dcols <- matrix(0,ncol,ncol)
  for(i in 1:ncol) {
    for(j in 1:ncol) {
      d<-0
      
      for(k in 1:nrow) {
        d <- d + (xctot[k,i] - xctot[k,j])^2 / rdot[k]
      }
      
      dcols[i,j] <- sqrt(d)
    }
  }
  colnames(dcols) <- colnames(m)
  rownames(dcols) <- colnames(m)
  
  # Se calculan las matrices de distancias entre filas
  drows <- matrix(0, nrow, nrow)
  for(i in 1:nrow) {
    for(j in 1:nrow) {
      d<-0
      
      for(k in 1:ncol) {
        d <- d + (xrtot[i,k] - xrtot[j,k])^2 / cdot[k]
      }
      
      drows[i,j] <- sqrt(d)
    }
  }
  colnames(drows) <- rownames(m)
  rownames(drows) <- rownames(m)
  
  #Eliminamos las variables intermedias.
  rm(i)
  rm(j)
  rm(k)
  rm(d)

# 4.- APLICACIÓN DEL MDS
  c <- ca(m / total)
  plot(c$rowcoord)
  
  c$rownames
  c$colnames
  
  r1 <- cmdscale(dcols, eig=TRUE)
  r1$points
  r1$eig
  
  c1 <- cmdscale(drows,eig=TRUE)
  c1$points
  c1$eig 
  
  #Gráfico 1 con todos los límites
  par(mfrow=c(1,1), mar=c(4,4,3,3))
  plot(r1$points,
       xlim=range(r1$points[,1], c1$points[,1]),
       ylim=range(r1$points[,1], c1$points[,1]),
       type="n",
       xlab="CRGs",ylab="Familia ATCs",lwd=2) 
  text(r1$points, labels=colnames(m), lwd = 1, cex = 0.7, col = "red") 
  text(c1$points, labels=rownames(m), lwd = 1, cex = 0.7)
  abline(h=0,lty=2)
  abline(v=0,lty=2)  
  
#5.- Liberamos los objetos
  rm(dcols)
  rm(drows)
  rm(xctot)
  rm(xrtot)
  rm(c1)
  rm(r1)
  rm(cdot)
  rm(ncol)
  rm(nrow)
  rm(rdot)
  rm(total)
  rm(total.columna)
  rm(total.fila)
  rm(c)
  rm(m)  
  rm(p)
  
#####EJEMPLO DE INTERNET

# Se introduce la tabla
sex<-matrix(c(21,21,14,13,8,8,9,6,8,2,2,3,4,10,10),ncol=5,byrow=TRUE)

# Se calculan los porcentajes
ncol<-5
nrow<-3
n<-sum(sex)

rtot<-apply(sex,1,sum)
ctot<-apply(sex,2,sum)

xrtot<-cbind(rtot,rtot,rtot,rtot,rtot)
xctot<-rbind(ctot,ctot,ctot)

xrtot<-sex/xrtot
xctot<-sex/xctot
rdot<-rtot/n
cdot<-ctot/n

# Se calculan las matrices de distancias entre columnas
dcols<-matrix(0,ncol,ncol)
for(i in 1:ncol){
  for(j in 1:ncol){d<-0
  for(k in 1:nrow) d<-d+(xctot[k,i]-xctot[k,j])^2/rdot[k]
  dcols[i,j]<-sqrt(d)}}

# Se calculan las matrices de distancias entre filas
drows<-matrix(0,nrow,nrow)
for(i in 1:nrow){
  for(j in 1:nrow){d<-0
  for(k in 1:ncol) d<-d+(xrtot[i,k]-xrtot[j,k])^2/cdot[k]
  drows[i,j]<-sqrt(d)}}

# Se aplica el MDS metrico
r1<-cmdscale(dcols,eig=TRUE)
r1$points
r1$eig

c1<-cmdscale(drows,eig=TRUE)
c1$points
c1$eig 

# Se dibujan las coordenadas en un dos dimensiones
par(pty="s") 
plot(r1$points,
     xlim=range(r1$points[,1], c1$points[,1]),
     ylim=range(r1$points[,1], c1$points[,1]),
     type="n",
     xlab="Coordenada 1",ylab="Coordenada 2",lwd=2) 
text(r1$points,labels=c("ED1","ED2","ED3","ED4","ED5"),lwd=2) 
text(c1$points,labels=c("Nopar","parnS","parS"),lwd=4)

abline(h=0,lty=2)
abline(v=0,lty=2)
