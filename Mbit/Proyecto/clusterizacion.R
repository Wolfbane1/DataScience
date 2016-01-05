library("dbscan")
library("apcluster")

#Preparamos la matriz para 2011.
columnasCopiar <- c("Sexo", "Edad", "CRG", "totalATCDiabeticos", "totalATCOtros", "numATCDiabeticos", "numATCOtros")
#columnasCopiar <- c("Sexo", "Edad", "CRG", "totalATCTodos", "numATCTodos")

d11.m <- as.matrix(d11[,colnames(d11) %in% columnasCopiar])
d11.m.scaled <- scale(d11.m, center=TRUE, scale=TRUE)
d12.m <- as.matrix(d12[,colnames(d12) %in% columnasCopiar])
d12.m.scaled <- scale(d12.m, center=TRUE, scale=TRUE)
rm(columnasCopiar)

d.m <- as.data.frame(d11.m)
d.m.scaled <- as.data.frame(d11.m.scaled)

rm(d11.m)
rm(d12.m)
rm(d11.m.scaled)
rm(d12.m.scaled)


#Vamos a copiar primero ciertas 
#noCopiar <- c("Id", "RangoEdad", "nivel", "Genero", "totalATCDiabeticos", "totalATCOtros")
#d11.m <- as.matrix(d11[,!colnames(d11) %in% noCopiar])

#K-MEANS
cluster.kmean <- kmeans(d.m.scaled, 78, iter.max = 1000, nstart = 1)

d.c <- as.data.frame(cbind(d.m, cluster.kmean$cluster))
colnames(d.c)[length(colnames(d.c))] <- "KMEAN"
d.c[,"KMEAN"] <- as.factor(d.c[,"KMEAN"])

#Vamos a ver la distribución número pacientes por cluster
qplot(d.c$KMEAN, fill=d.c$KMEAN, geom="histogram")

qplot(d.c$KMEAN, d.c$Edad, fill=d.c$KMEAN, geom="boxplot")

#APCLUSTER
s <- negDistMat(r=2)
cluster.ap <- apcluster(s, d.m.scaled)
rm(s)

#DBSCAN
a <- kNNdist(d.m.scaled, 5)
kNNdistplot(d.m.scaled, 5)
abline(h=0.5,lty=4)

mean(a)
rm(a)

cluster.dbscan <- dbscan(d.m.scaled, 1, 20)
table(cluster.dbscan$cluster)

d.c[,"DBSCAN"] <- as.factor(cluster.dbscan$cluster)

d.c <- as.data.frame(cbind(d.c, cluster.dbscan$cluster))
d.m <- as.data.frame(cbind(d.m, cluster.dbscan$cluster))
colnames(d.c)[length(colnames(d.c))] <- "DBSCAN"
colnames(d.m)[length(colnames(d.m))] <- "DBSCAN"
d.c[,"DBSCAN"] <- as.factor(d.c[,"DBSCAN"])

#Vamos a ver la distribución número pacientes por cluster
qplot(d.c$DBSCAN, fill=d.c$DBSCAN, geom=c("histogram"))  
#  theme(axis.text.x = element_text(size = 8, colour = "red", angle = 45)))

c <- table(d11.c$DBSCAN)
cat <- sort(c)
qplot(rownames(cat), cat, xlab="Cluster", ylab= "Número Pacientes", stat="identity",geom="histogram", fill=rownames(cat))
plot(cat, type="s")
rm(c)
rm(cat)

#Distribución por edad de los medicamentos.
qplot(d.c$DBSCAN, d.c$Edad, fill=d.c$Sexo, geom="boxplot",
      xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y Sexo")

table(d.c$DBSCAN, d.c$Sexo)

#Distribución por edad y CRG.
qplot(d.c$DBSCAN, d.c$Edad, fill=d.c$nivel, geom="boxplot",
      xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y CRG")

table(d.c$DBSCAN, d.c$nivel)

#Distribución por edad y número de medicamentos.
qplot(d.c$DBSCAN, d.c$numATCDiabeticos+d.c$numATCOtros, fill=d.c$nivel, geom="boxplot",
      xlab="Cluster", ylab="Num.Medicamentos total", 
      main="Número de pacientes por Cluster, Edad y Número de medicamentos distintos que toma")

table(d.c$DBSCAN, d.c$nivel)

#Distribución por edad y número total de dispensaciones.
qplot(d.c$DBSCAN, d.c$totalATCTodos, fill=d.c$nivel, geom="boxplot",
      xlab="Cluster", ylab="Num. Dispensaciones total", 
      main="Número de pacientes por Cluster, Edad y Número total de dispensaciones")

d <- d11.m
d.m <- as.data.frame(d.m)

#Cluster 0

resultado <- matrix(nrow=0, ncol=)

cluster

n <- 5
  
#Cluster 
table(d.m[d.m$DBSCAN==n, "DBSCAN"])
summary(d.m[d.m$DBSCAN==n, "Edad"])
table(d.m[d.m$DBSCAN==n, "Sexo"])/nrow(d.m[d.m$DBSCAN==n,])
table(d.m[d.m$DBSCAN==n, "CRG"])
#summary(d.m[d.m$DBSCAN==n, "totalATCTodos"])
#summary(d.m[d.m$DBSCAN==n, "numATCTodos"])
summary(d.m[d.m$DBSCAN==n, "totalATCDiabeticos"] + d.m[d.m$DBSCAN==n, "totalATCOtros"])
qplot(d.m[d.m$DBSCAN==n, "totalATCOtros"], d.m[d.m$DBSCAN==n, "totalATCDiabeticos"], geom="boxplot")
summary(d.m[d.m$DBSCAN==n, "numATCDiabeticos"] + d.m[d.m$DBSCAN==n, "numATCOtros"])

