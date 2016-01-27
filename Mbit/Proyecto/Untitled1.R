library(swirl)
install_from_swirl("Exploratory Data Analysis")
swirl()

#email: zzddfge@gmail.com
#codigo principles of Analytic Graphs: b4XzX4KsSYL3gu6L
#codigo Exploratory Graphs: gyPI6OoFIsCuFoqX
#codigo Graphics Devices: qJ04zqcHqf7W4prO
#codigo Plotting Systems: 5F57zaLTjxgDchMo
#codigo Base Plotting: xAwOfD4GTw9P36MR

for (i in ls()) eval(parse(text=paste("rm(",i,")", sep="")))
rm(i)






setwd("/Users/zzddfge/Desktop/Compartida/Proyecto_Master")
origenMat <- file.path(getwd(), "data", "mat")
fichero <- "datos_orig.mat"
fichero_ruido <- "datos_ruidosos.mat"

#Librería que lee la estructura de un fichero de matlab.
library(R.matlab)
library(sqldf)

source("ac.R")

mat <- readMat(file.path(origenMat, fichero))
df <- as.data.frame(cbind(as.vector(mat$x), as.vector(mat$y), as.vector(mat$z)))
colnames(df) <- c("old_x", "y", "z")
df <- cbind(df, rep(as.character("[0, 11)"), length(df$y)))
colnames(df)[4] <- "x"
df$x <- as.character(df$x)
df[df$old_x >= 11 & df$old_x < 25, "x"] <- "[11, 25)"
df[df$old_x >= 25 & df$old_x <= 40, "x"] <- "[25, 40]"
df$x <- factor(df$x)

#Obtenemos cuentas.
a <- sqldf("select x, y, count(*) as total from df group by x, y") 

#Generamos matrix
m <- matrix(0, nrow=length(unique(a$x)), ncol=length(unique(a$y)))
rownames(m) <- as.character(unique(a$x))
colnames(m) <- as.character(unique(a$y))
for (i in 1:nrow(a)) {
  m[as.character(a$x[i]), as.character(a$y[i])] <- a$total[i]
}

par(mfrow=c(1,1), mar=c(4,4,3,3))
mosaicplot(m, shade = TRUE, main = "Mosaico X vs Y")


m1 <- as.matrix(cbind(as.data.frame(m),rep(1,3)))
ca1 <- ca(m1)
ca2 <- CA(m1)

art_max <- coindep_test(m, n = 5000)
art_max
mosaic(m, shade = TRUE, gp = shading_Friendly(lty = 1, eps = NULL), main="DD")
mosaic(m, shade=TRUE, gp = shading_hsv, 
       gp_args = list(interpolate = art_max$qdist(c(0.9,0.99)), 
                      p.value = art_max$p.value))

mosaic(m, gp = shading_max, gp_args = list(n = 5000))




#Pintamos las dimensiones según el Test de Molinvaud. 
dims.to.be.plotted <- 2

# AC del paquete estándar.
res.ca <- ca(m, nd=dims.to.be.plotted)

# AC almacenamos el resultado para pintarlo después.
cadataframe <- summary(ca(m, nd=dims.to.be.plotted))

# Distribución de la contribución de las categorías de filas a las dimensiones con línea de referencia.
# plot bar charts of contribution of row categories to the axes, and add a reference line
cat("\n\n")
cat("####Distribución de la contribución de las categorías\n\n")
cat("\n\nFilas\n\n")
counter <- 0
for(i in seq(7, ncol(cadataframe$rows), 3)){    
  counter <- counter +1
  barplot(cadataframe$rows[,i], ylim=c(0,1000), 
          xlab="Categorías de Filas", 
          ylab=paste("Contribución a Dim.", counter), 
          names.arg=rownames(m), cex.lab=0.80)
  abline(h=round(((100/nrow(m))*10), digits=0))
}

# plot bar charts of contribution of column categories to the axes, and add a reference line
cat("\n\nColumnas\n\n")
counter <- 0
for(i in seq(7, ncol(cadataframe$columns), 3)){    
  counter <- counter +1
  barplot(cadataframe$columns[,i], ylim=c(0,1000), 
          xlab="Categorías de Filas", 
          ylab=paste("Contribución a Dim.", counter), 
          cex.lab=0.80,
          names.arg=colnames(m))
  abline(h=round(((100/ncol(m))*10), digits=0))
}

cat("\n\n")
cat("####Correlación de categorías a Dimensiones\n\n")
cat("\n\n Filas \n\n")
counter <- 0
for(i in seq(6, ncol(cadataframe$rows), 3)){    
  counter <- counter +1
  correl.rows <- round(sqrt((cadataframe$rows[,i]/1000)), digits=3)
  barplot(correl.rows, ylim=c(0,1), 
          xlab="Categorías de Filas", 
          ylab=paste("Correlación con Dimensión ", counter), 
          names.arg=rownames(m), cex.lab=0.80)
}

cat("\n\n Columnas \n\n")
counter <- 0
for(i in seq(6, ncol(cadataframe$columns), 3)){    
  counter <- counter +1
  correl.cols <- round(sqrt((cadataframe$columns[,i]/1000)), digits=3)
  barplot(correl.cols, ylim=c(0,1),
          xlab="Categorías de Columnas", 
          ylab=paste("Correlación con Dimensión ", counter), 
          names.arg=colnames(m), cex.lab=0.80)
}

## CA graphical outputs ##:
# symmetric plots from FactoMineR package
cat("\n\n")
cat("####Salida del Análisis de Correspondencia\n\n")
cat("\n\n Gráfico Simétrico \n\n")
par(mfrow=c(1,1), mar=c(4,4,2,2))

numb.dim.cols<-ncol(m)-1
numb.dim.rows<-nrow(m)-1
a <- min(numb.dim.cols, numb.dim.rows)
p <- CA(m, ncp = 5, graph = FALSE)
#p$col$coord <- cbind(p$col$coord, rep(0, 2))
p$row$coord
r <- as.data.frame(p$row$coord, 1)
c <- as.data.frame(p$col$coord, 1)
xmin = range(p$row$coord[1], p$col$coord[1])[1] * 1.1
plot(r, c(0,0,0), type = "p", pch = 16, col="blue",
     xlim=c(-0.01, 0.01),
     ylim=c(-0.5,0.5),
     xlab="Dimensión 1", ylab="Dimensión 2",
     main = "Análisis de Correspondencia de CRG-Base Diabético e Hipertensión y Familias de ATC", cex.main=0.7
)
points(c, type = "p", pch = 17, col="red")
for (i in 1:nrow(m)) {
  text(p$row$coord[i], 0.20, rownames(m)[i], col="blue", cex=0.6)
}
for (i in 1:ncol(m)) {
  text(p$col$coord[i], p$col$coord[i] + 0.15, colnames(m)[i], col="red", cex=0.6)
}
abline(h=0,lty=2)
abline(v=0,lty=2) 

plot(m)

stripplot(res.ca$rowcoord)
r <- as.data.frame(res.ca$rowcoord, 1)
c <- as.data.frame(res.ca$colcoord, 1)
plot(r)
points(c)

plot(p$row$coord, type = "p", pch = 16, col="blue")

counter <- 1
for(i in 2:dims.to.be.plotted){    
  counter <- counter +1
  plot(malinv.ca, axis=c(1,i),
       shadow=TRUE, cex=0.80, invisible="none", 
       title = paste("AC Simétrico: Dim. 1 +", counter), cex.main=0.8)
  #plot(malinv.ca, axes=c(1,i), shadow=TRUE, cex=0.80, invisible="col", title = paste("Correspondence #Analysis-symmetric rows map: Dim. 1 +", counter), cex.main=0.8)
  #  plot(malinv.ca, axes=c(1,i), shadow=TRUE, cex=0.80, invisible="row", title = paste("Correspondence #Analysis-symmetric cols map: Dim. 1 +", counter), cex.main=0.8)
}

# asymmetric biplots (Standard Biplots) from Greenacre's package: rows in principal coordinates and columns in standard coordinates times square root of the mass (Greenacre 2007, pp. 102, 234, 268, 270). NOTE: The lenght of each arrow joining the column points to the origin is proportional to the contribution that each column category makes to the principal axes; colour intensity proportional to the absolute contribution to the total inertia
cat("\n\n Gráfico de relación bi-variable A-Simétrico \n\n")
#cat("\n\n Asymmetric biplots from Greenacre's package: rows in principal coordinates and columns in standard #coordinates \n\n")
counter <- 1
for(i in 2:dims.to.be.plotted){    
  counter <- counter +1
  plot(res.ca, mass = FALSE, 
       dim=c(1,i), contrib = "none", col=c("black", "red"), map ="rowgreen", arrows = c(FALSE, TRUE), 
       main = paste("(F-T) Biplot Estándar AC: Dim. 1 +", counter), cex.main=0.8)
  
  plot(res.ca, mass = FALSE, 
       dim=c(1,i), contrib = "absolute", col=c("black", "red"), map ="rowgreen", arrows = c(FALSE, TRUE), 
       main = paste("(F-T) Biplot Estándar AC: Dim. 1 +", counter), cex.main=0.8,
       sub="Intensidad del color proporcional a la contribución absoluta a la inercia", cex.sub=0.60)
  
  plot(res.ca, mass = FALSE, 
       dim=c(1,i), contrib = "none", col=c("black", "red"), map ="colgreen", arrows = c(TRUE, FALSE), 
       main = paste("(T-F) Biplot Estándar AC: Dim. 1 +", counter))
  
  plot(res.ca, mass = FALSE, 
       dim=c(1,i), contrib = "absolute", col=c("black", "red"), map ="colgreen", arrows = c(TRUE, FALSE), 
       main = paste("(T-F) Biplot Estándar AC: Dim. 1 +", counter), cex.main=0.8,
       sub="Intensidad del color proporcional a la contribución absoluta a la inercia", cex.sub=0.60)
}

## clustering after FactoMiner package:
#ca.factom <- CA(mydata, ncp=dims.to.be.plotted, graph= FALSE)
#resclust.rows<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, #graph.scale="inertia", graph=FALSE, cluster.CA="rows")
#resclust.cols<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, #graph.scale="inertia", graph=FALSE, cluster.CA="columns")


#rm(ncols)
#rm(nrows)
rm(numb.dim.rows)
rm(numb.dim.cols)
rm(optimal.dimensionality)
rm(perf.corr)
#rm(quality.cols)
#rm(quality.rows)
#rm(r.dim)
#rm(res.ca)
#rm(resclust.cols)
#rm(user.dimensionality)
rm(thresh.sig.dim)
rm(sqr.trace)
#rm(resclust.rows)
rm(n.dim.average.rule)
rm(malinvt.output)
rm(malinv.test.cols)
rm(malinv.test.rows)
rm(malinv.ca)
rm(labs)
rm(k)
rm(i)
#rm(grandtotal)
rm(dims.to.be.plotted)
rm(dataframe.after.ca)
rm(counter)
rm(correl.cols)
rm(correl.rows)
rm(cadataframe)
#rm(ca.factom)
rm(a)
rm(c.dim)
#rm(mydata)
rm(mydataasmatrix)
#rm(data.w.colsum)
#rm(data.w.rowsum)


suppressMessages(a <- sqldf("select x, z, count(*) as total from df group by x, z"))
m <- getMatrizfromDF(a)

numb.dim.cols<-ncol(m)-1
numb.dim.rows<-nrow(m)-1
a <- min(numb.dim.cols, numb.dim.rows) #dimensionality of the table
labs<-c(1:a) #set the numbers that will be used as x-axis' label
mosaicplot(m, shade = TRUE, main = "Mosaico X vs Z")

m
ca.ca <- ca(m)
fm.CA <- CA(m, graph=FALSE)
fm.PCA <- PCA(m, graph=FALSE)

eig <- get_eigenvalue(p)
rc <- sqrt(eig$eigenvalue) 
rc

#1.- Single Values
ca.ca$sv
fm.CA$svd$vs
#devuelve los mismos valores.

#2.- Inercia de las filas.
ca.ca$rowinertia
fm.CA$row$inertia
#devuelve los mismos valores.

#3.- Inercia de las columnas.
ca.ca$colinertia
fm.CA$col$inertia

#4.- 
ca.ca$rowcoord
fm.CA$row$coord
fm.CA$row$coord/rc

summary(ca(m, nd=dims.to.be.plotted))
fm.CA$row$contrib

#Pintar ... 
plot.CA(fm.CA, axes = c(1,2), col.row = "blue", col.col = "red")
par(mfrow=c(1,1), mar=c(4,4,3,3))
plot(fm.CA$row$coord,
     xlim=c(min(fm.CA$row$coord[,1], fm.CA$col$coord[,1])*1.2, max(fm.CA$row$coord[,1], fm.CA$col$coord[,1])*1.2),
     ylim=c(min(fm.CA$row$coord[,2], fm.CA$col$coord[,2])*1.2, max(fm.CA$row$coord[,2], fm.CA$col$coord[,2])*1.2),
     pch=19,
     col="blue")
points(fm.CA$col$coord, pch=17, col="red")
for (i in 1:nrow(m)) {
  text(fm.CA$row$coord[i,1] + 0.015, fm.CA$row$coord[i,2] + 0.03, rownames(m)[i], col="blue", cex=0.6)
}
for (i in 1:ncol(m)) {
  text(fm.CA$col$coord[i,1] + 0.015, fm.CA$col$coord[i,2] + 0.03, colnames(m)[i], col="red", cex=0.6)
}
abline(h=0,lty=2)
abline(v=0,lty=2) 

cat("\n\nAhora imprimimos con la función del paquete factoextra\n\n")
p1 <- fviz_ca_biplot(fm.CA, map ="rowprincipal", arrow = c(FALSE, TRUE))
plot(p1)
rm(p1)

plot(fm.CA, axes = c(1,2), col.row = "blue", col.col = "red", arrow=c(FALSE, TRUE),
        title = "(F-T) Biplot Estándar AC: Dim. 1 + 2", cex.main=0.8)

plot(fm.CA, mass = FALSE, 
     dim=c(1,i), contrib = "none", col=c("blue", "red"), map ="rowgreen", arrows = c(FALSE, TRUE), 
     main = paste("(F-T) Biplot Estándar AC: Dim. 1 +", counter), cex.main=0.8)



fviz_ca_biplot(p)

fviz_ca_biplot(p, map ="rowgreen",
               arrow = c(TRUE, FALSE))

# Contributions of rows on Dim.1
fviz_contrib(fm.CA, choice = "row", axes = 1)


fviz_pca_var(fm.PCA)
fviz_pca_var(fm.PCA, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

plot3d.ca()


#########################################################################
#########################################################################
#########################################################################
#########################################################################

origenData <- file.path(getwd(), "data")
origenMat <- file.path(getwd(), "data", "mat")
destinoCSV <- file.path(getwd(), "data", "csv")
ficheroDestino <- file.path(destinoCSV, "pacientesDiabetes.csv")
ficheroTomanA10Destino <- file.path(destinoCSV, "tomanA10.mat")
ficheroNoTomanA10Destino <- file.path(destinoCSV, "noTomanA10.mat")
colorAnyo <- c("springgreen2", "turquoise")
colorGenero <- c("slateblue3", "snow4")
colorCRG <- c("thistle", 
              "darkblue", "chocolate", "green", "darkgrey", "firebrick4", "floralwhite", "firebrick", 
              "lemonchiffon", "red",       
              "slateblue", "saddlebrown", "aquamarine", "purple", "wheat", "orchid", "violetred", "saddlebrown", 
              "pink", "yellow", "olivedrab", "navajowhite")

source("analisisbasico.R")
source("ac.R")

#procesamos el CSV final.
csv <- procesaCSV(ficheroDestino)

cat("\n\n")
cat("##Año 2011")
anyo <- 2011
m <- getCRGDiabHipertension(anyo)
labs <- EscribeDimensionalidadTabla(m)


EscribeContribucionDimension(m)

p <- CA(m)
p1 <- ca(m)

max = max(p$row$contrib[,1]/10) + 10
max = min(max, 100)
barplot(p$row$contrib[,1], ylim=c(0,max), col="steelblue",
        xlab="Categorías de Filas", 
        ylab=paste("% Contribución a Dim.", 1), 
        names.arg=rownames(m), cex.lab=0.8, las=1, cex.axis = 0.7, cex.names = 0.8)
abline(h=round((1/nrow(m)), digits=0), col="red", lty=2)

m

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

cadataframe <- summary(ca(m, nd=2))
cadataframe$rows[,1]
table(cadataframe$rows[,1])

cat(paste("\n\nGráfico AC.1.", anyo, " - Mosaico de ATCs - CRGs[5424, 6144, 7071]", sep=""))

mosaicplot(m, shade = TRUE, main = "Mosaico de ATCs - CRGs[5424, 6144, 7071]")





m1 <- matrix(m[6,], ncol = 3, nrow=1)
m1 <- rbind(m1, m[4,])
m1 <- rbind(m1, m[7,])
m1 <- rbind(m1, m[1,])
m1 <- rbind(m1, m[9,])
m1 <- rbind(m1, m[3,])
m1 <- rbind(m1, m[5,])
m1 <- rbind(m1, m[13,])
m1 <- rbind(m1, m[10,])
m1 <- rbind(m1, m[2,])
m1 <- rbind(m1, m[12,])
m1 <- rbind(m1, m[8,])
m1 <- rbind(m1, m[11,])
m1 <- rbind(m1, m[14,])
rownames(m1) <- rownames(m)
rownames(m1)[1] <- rownames(m)[6]
rownames(m1)[2] <- rownames(m)[4]
rownames(m1)[3] <- rownames(m)[7]
rownames(m1)[4] <- rownames(m)[1]
rownames(m1)[5] <- rownames(m)[9]
rownames(m1)[6] <- rownames(m)[3]
rownames(m1)[7] <- rownames(m)[5]
rownames(m1)[8] <- rownames(m)[13]
rownames(m1)[9] <- rownames(m)[10]
rownames(m1)[10] <- rownames(m)[2]
rownames(m1)[11] <- rownames(m)[12]
rownames(m1)[12] <- rownames(m)[8]
rownames(m1)[13] <- rownames(m)[11]
rownames(m1)[14] <- rownames(m)[14]
  
mosaicplot(m, shade = TRUE, main = "Mosaico de ATCs - CRGs[5424, 6144, 7071]")
mosaicplot(m1, shade = TRUE, main = "Mosaico de ATCs - CRGs[5424, 6144, 7071]")







m[1,]



###################################################
### Conversión de las matrices relativas al grupo.
###################################################

source("PCAs.R")

for (anyo in unique(csv$Anyo)) {
  #Generamos título Nivel 2
  cat(paste("##Año ", anyo, "\n\n"))
  
  #Generamos el conjunto de datos.
  dx <- eliminaATCsVacios(csv, anyo, 7:(ncol(csv)-N))
  
  #Generamos las familias
  m_cuentaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "CUENTA_FAMILIAS")
  m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "SUMA_FAMILIAS")
  m_existeFamilia <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "EXISTENCIA")
  
#  b <- c(20, 40, 60, 70, 80, 90, 100)
  b <- c(10, 15, 20, 25, 30, 40, 100)
  
  #Generamos el gráfico de Existencia.
  cat("###Análisis de Existencia \n\n")
  m <- calculaBurbujaATC(dx, m_existeFamilia)
  
  #Pintamos el gráfico. 
  cat(paste("\n\nGráfica ATC.1.", anyo, " - Distribución de Existencia por CRG en la toma de ATC\n\n", sep=""))
  g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
    scale_fill_manual("CRG-base", values=colorCRG) +
#    scale_size("Número", breaks=b) + 
    scale_size("Número") + 
    theme(axis.text.x = element_text(size = 8, colour = "black")) + 
    theme(title = element_text(size = 8, colour = "black")) + 
    theme(legend.text = element_text(size = 6)) + 
    theme(legend.key.height = unit (0.4, "cm")) + 
    labs(x="ATC", y="CRG-base", 
         title="Existencia de toma de grupo de Familia ATC por CRG")
  print(g)
  cat("\n\n")
  
  #Generamos el gráfico de Cuentas.
  cat("###Análisis de Cuentas\n\n")
  
  #Generamos el dato definitivo
  m <- calculaBurbujaATC(dx, m_cuentaFamilias, "SUMA")
  
  #Pintamos el gráfico. 
  cat(paste("\n\nGráfica ATC.2.", anyo, " - Distribución de Existencia por CRG en la toma de ATC\n\n", sep=""))
  g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
    scale_fill_manual("CRG-base", values=colorCRG) +
#    scale_size("Número", breaks=b) + 
    scale_size("Número") + 
    theme(axis.text.x = element_text(size = 8, colour = "black")) + 
    theme(title = element_text(size = 8, colour = "black")) + 
    theme(legend.text = element_text(size = 6)) + 
    theme(legend.key.height = unit (0.4, "cm")) + 
    labs(x="ATC", y="CRG-base", 
         title="Cuenta de medicamentos distintos de grupo de Familia ATC por CRG")
  print(g)
  cat("\n\n")
  
  #Generamos el gráfico de Suma
  cat("###Análisis de Sumas\n\n")
  
  #Generamos el dato definitivo
  m <- calculaBurbujaATC(dx, m_sumaFamilias, "SUMA")
  
  #Pintamos el gráfico. 
  cat(paste("\n\nGráfica ATC.3.", anyo, " - Distribución de Existencia por CRG en la toma de ATC\n\n", sep=""))
  g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
    scale_fill_manual("CRG-base", values=colorCRG) +
    scale_size("Número") + 
    theme(axis.text.x = element_text(size = 8, colour = "black")) + 
    theme(title = element_text(size = 8, colour = "black")) + 
    theme(legend.text = element_text(size = 6)) + 
    theme(legend.key.height = unit (0.4, "cm")) + 
    labs(x="ATC", y="CRG-base", 
         title="Suma total de medicamentos por grupo de Familia ATC y por CRG")
  print(g)
  
  cat("\n\n\n")
  
  #Generamos el gráfico para cada CRG.
  cat("###Generacíon de gráficas individuales por CRG-base\n\n")
  
  #Preparamos los datos para el año en curso.
  dx <- subset(csv, csv$Anyo == anyo)
  pacientes <- as.data.frame(cbind(dx$Id, dx$CRG, 
                                   dx$totalATCTodos, dx$totalATCDiabeticos, dx$totalATCOtros,
                                   rep(1.01, length(dx$Id)), rep(1.01, length(dx$Id)), 
                                   rep(1.01, length(dx$Id))))
  colnames(pacientes) <- c("Id", "CRG", "TotalATCs", "TotalATCDiab", "TotalATCOtros", 
                           "%TotalATc", "%TotalATCDiab", "%TotalATCOtros")
  
  #Calculamos los totales por edad y por edad y crg para que poder calcular el %.
  df <- as.data.frame(sqldf("select CRG, sum(totalATCTodos) as TotalATCs, 
                            sum(totalATCDiabeticos) as TotalATCDiab, 
                            sum(totalATCOtros) as TotalATCOtros
                            from dx group by CRG"))
  
  #Calculamos el %
  for (i in 1:nrow(pacientes)) {
    pacientes[i, "%TotalATc"] <- as.numeric(round(pacientes[i, "TotalATCs"] / df[df$CRG == pacientes$CRG[i], "TotalATCs"], 4))
    pacientes[i, "%TotalATCDiab"] <- as.numeric(round(pacientes[i, "TotalATCDiab"] / df[df$CRG == pacientes$CRG[i], "TotalATCDiab"], 4))
    pacientes[i, "%TotalATCOtros"] <- as.numeric(round(pacientes[i, "TotalATCOtros"] / df[df$CRG == pacientes$CRG[i], "TotalATCOtros"], 4))
  }
  pacientes$CRG <- factor(pacientes$CRG)
  i <- 1 #variable para pintar el CRG del color apropiado. 
  par(mfrow=c(1,2))
  
  for (crg in unique(dx$CRG)) {
    cat(paste("\n\nGráfica ATC.CRG.", i, ".", anyo, ": ", crg, 
              " - Distribución de Pacientes por toma de ATCs",
              ", Número de pacientes: ", length(pacientes[pacientes$CRG==crg, "CRG"]), "\n\n", sep=""))
    
    #Pintamos el gráfico con ambas líneas
    #Pintamos el gráfico de total.
    par(mfrow=c(1,1))
    plot(pacientes[pacientes$CRG==crg, "%TotalATc"], type = "l", cex.axis = 0.7,  
         col="red",
         xlab = "Número de pacientes", ylab="% de ATCs (total)")
    lines(pacientes[pacientes$CRG==crg, "%TotalATCDiab"], col="blue")
    title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
          cex.main=0.7)
    
    par(mfrow=c(1,2))
    #Pintamos el gráfico de total.
    plot(pacientes[pacientes$CRG==crg, "%TotalATc"], type = "l", cex.axis = 0.7,  
         col=colorCRG[i],
         xlab = "Número de pacientes", ylab="% de ATCs (total)")
    title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
          cex.main=0.65)
    
    #Pintamos el gráfico de total de ATCs
    plot(pacientes[pacientes$CRG==crg, "%TotalATCDiab"], type = "l", cex.axis = 0.7,  
         col=colorCRG[i],
         xlab = "Número de pacientes", ylab="% de ATCs (Diabéticos)")
    title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
          cex.main=0.65)
    
    i <- i + 1
  }#fin bucle CRG.
}#fin de bucle de año.
