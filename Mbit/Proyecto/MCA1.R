
#referencia: http://www.sthda.com/english/wiki/multiple-correspondence-analysis-essentials-interpretation-and-application-to-investigate-the-associations-between-categories-of-multiple-qualitative-variables-r-software-and-data-mining
library("FactoMineR")
library("factoextra")


dt <- df[,2:4]
dt$x <- paste("x_", dt$x, sep="")
dt$y <- paste("y_", dt$y, sep="")
dt$z <- paste("z_", dt$z, sep="")
dt <- as.matrix(dt)

p <- MCA(dt, ncp = 5, graph = FALSE)

summary(p, nb.dec = 3, nbelements = 10, ncp = TRUE, file ="")

# Select the top 20 contributing individuals
fviz_mca_ind(p, select.ind = list(contrib = 20))

# Select the top 20 contributing variables
fviz_mca_ind(p, select.var = list(contrib = 20))

fviz_mca_var(p, select.var = list(cos2 = 0.4))

fviz_mca_biplot(p, label="var", axes=c(1,2))

fviz_mca_biplot(p, label="var", axes=c(1,2), mode="rowprincipal", arrows=c(TRUE, FALSE))
fviz_mca_biplot(p, label="var", axes=c(1,2), mode="colprincipal", arrows=c(FALSE, TRUE))

fviz_mca_biplot(p, label="var", axes=c(1,2), mode="rowgreen", arrows=c(TRUE, FALSE))
fviz_mca_biplot(p, label="var", axes=c(1,2), mode="colgreen", arrows=c(FALSE, TRUE))

library("scatterplot3d")
x <- p$var$coord[,1]
y <- p$var$coord[,2]
z <- p$var$coord[,3]
graf <- scatterplot3d(x,y,z)
for (i in 1:length(p$var$coord[,1])) {
  text(graf$xyz.convert(p$var$coord[,1][i],p$var$coord[,2][i],p$var$coord[,3][i]), 
       labels=names(p$var$coord[,1])[i], pos=1) 
}


names(p$var$coord[,1])

m3d <- as.matrix(cbind(x, y, z))
colnames(m3d) <- c("x", "y", "z")

s3d.dat <- data.frame(cols=as.vector(col(m3d)),
                      rows=as.vector(row(m3d)),
                      value=as.vector(m3d))

scatterplot3d(s3d.dat, type="h", lwd=5, pch=" ",
              x.ticklabs=colnames(m3d), y.ticklabs=rownames(m3d),
              color="red", main="scatterplot3d - 4")

mca.ca <- mjca(dt)

plot3d.mjca(mca.ca)

fviz_mca_var(p)

var <- get_mca_var(p)
var


grp <- as.factor(dt[, "y"])
fviz_mca_ind(p, label="none", habillage=grp)

View(var$coord)



summary(df$y)
sd(df$y)

######################
######################

data(poison)
head(poison[, 1:7])

poison.active <- poison[1:55, 5:15]
poison.active

p.MCA <- MCA(poison.active, ncp = 5, graph = TRUE)
p.MCA

#Resumen del resultado.
summary(p.MCA, nb.dec = 3, nbelements = Inf, ncp = TRUE, file ="")

###Interpretación de los resultados.
eigenvalues <- get_eigenvalue(p.MCA)
round(eigenvalues, 2)
trace = sum(eigenvalues$eigenvalue)
cor.coef <- sqrt(trace)
cat(paste("\n\nCoeficiente de correlación entre filas y columnas:" ,cor.coef, "\n\n", sep=""))

#Descripción de la explicación de las varianza 
fviz_screeplot(p.MCA, addlabels=TRUE) 

    + geom_hline(yintercept=7.11)

#Pintamos un nuevo gráfico
par(mfrow=c(1,1), mar=c(4,4,3,3))
plot(p.MCA, axes = c(1,2), choix=c("var"))

grp <- as.factor(poison.active[, "Vomiting"])
fviz_mca_ind(p.MCA, label="none", habillage=grp)

#O con esta función
fviz_mca_biplot(p.MCA)

#The graph above shows a global pattern within the data. Rows (individuals) are represented by blue points and columns (variable categories) by red triangles.
#
#The distance between any row points or column points gives a measure of their similarity (or dissimilarity).
#
#Row points with similar profile are closed on the factor map. The same holds true for column points.

var <- get_mca_var(p.MCA)
var


a <- xtabs(~ poison.active$Nausea + poison.active$Vomiting + poison.active$Abdominals)
mosaicplot(a, shade = TRUE)





#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################
#############################################################################################################

distancia <- dist(m)
heatmap(m)

d11 <- subset(csv, csv$Anyo == 2011)
#m11 <- d11[, !colnames(d11) %in% c("Id", "RangoEdad", "nivel", "Genero")]
m11 <- d11[,colnames(d11) %in% c("Edad", "Sexo", "CRG", 
                                 "totalATCDiabeticos", "totalATCOtros", "numATCDiabeticos", "numATCOtros")]
m11 <- as.matrix(m11)
m12 <- scale(m11, center=TRUE, scale=TRUE)

#euclidean
distancia=dist(m12, method = "euclidean")
hc <- hclust(distancia)
plot(hc)
head(hc)

table(hc$labels)
table(hc$height)
summary(hc$height)

hc$height

#manhattan
distancia1=dist(m12, method = "manhattan")
hc1 <- hclust(distancia1)
plot(hc1)


heatmap(m12)

#################################################
#################################################

#Librería que lee la estructura de un fichero de matlab.
suppressPackageStartupMessages(library(R.matlab))
suppressPackageStartupMessages(library(sqldf))
suppressWarnings(suppressPackageStartupMessages(library("FactoMineR")))
suppressWarnings(suppressPackageStartupMessages(library("factoextra")))
suppressPackageStartupMessages(library("vcd"))
suppressPackageStartupMessages(library("corrplot"))

setwd("/Users/zzddfge/Desktop/Compartida/Proyecto_Master")
origenMat <- file.path(getwd(), "data", "mat")
fichero <- "datos_orig2.mat"
#fichero <- "datos_ruidosos2.mat"
#fichero <- "datos_orig.mat"
#fichero <- "datos_ruidosos.mat"

suppressWarnings(source("ac.R"))

mat <- readMat(file.path(origenMat, fichero))
df <- as.data.frame(cbind(as.vector(mat$x), as.vector(mat$y), as.vector(mat$z)))
colnames(df) <- c("old_x", "y", "z")
df <- cbind(df, rep(as.character("[0, 11)"), length(df$y)))
colnames(df)[4] <- "x"
df$x <- as.character(df$x)
df[df$old_x >= 11 & df$old_x < 25, "x"] <- "[11, 25)"
df[df$old_x >= 25 & df$old_x <= 40, "x"] <- "[25, 40]"
df$x <- factor(df$x)

m <- xtabs(~ df$x + df$z)
m

#Preparamos la matriz para el MCA
dt <- df[,2:4]
dt$x <- paste("x_", dt$x, sep="")
dt$y <- paste("y_", dt$y, sep="")
dt$z <- paste("z_", dt$z, sep="")
dt <- as.matrix(dt)
head(dt)

p <- MCA(dt, ncp = 10, graph = FALSE)
p1 <- MCA(dt, ncp = 10, graph = FALSE)
p2 <- MCA(dt, graph=FALSE, method = "Burt")
rm(dt)

plot(p2)
plot(p1)

summary(p1, nbelements = 10, ncp = TRUE, file ="")

summary(p2, nbelements = 10, ncp = TRUE, file ="")

dimdesc(p2)


head(dt)
data(tea)
head(tea)

chisq <- chisq.test(m)
cat("\n\n<b>Método de CHI^2</b>\n\n")
cat(print(chisq))
cat("\n\n")
rm(chisq)
