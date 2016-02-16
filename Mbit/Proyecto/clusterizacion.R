
library("dbscan")

iniciaAnyo <- function(csv, anyo) {
  cat(paste("\n\n####Año: ", anyo, "\n\n", sep=""))
  #Sacamos cada año para ver si la clusterización se consigue por separado. 
  d <- subset(csv, csv$Anyo == anyo)
  
  #Convertimos el género en variable 0 y 1. Haremos que todos los que sean 2, pasen a ser 0.
  d[d$Sexo == 2, "Sexo"] <- 0
  
  return (d)
}

iniciaMatriz <- function(d, columnasCopiar) {
  #Obtenemos la matriz con las variables a usar y la normalizamos. 
  d.m <- as.matrix(d[,colnames(d) %in% columnasCopiar])
  d.m.scaled <- as.data.frame(scale(d.m, center=TRUE, scale=TRUE))
  
  #Vamos a visualizar la distancia de los K-Vecinos más cercanos.
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.1 - K-Vecinos[min=", n, "] cercarnos para ", anyo, 
            "</b>\n\n", sep=""))
  cat("En las gráficas vamos a marcar varias líneas: Valor 0.3 (gris), Valor 0.5 (azul), 
      Valor 1 (roja) y Valor 1.5 (verde). Eso solo es para tener una referencia de cara a 
      identificar mejor cuál es el mejor punto de cara a la inflexión.")
  par(mfrow=c(1,2), mar=c(3,3,2,2))
  a <- kNNdist(d.m.scaled, n)
  plot(a)
  abline(h=0.25, col="grey")
  abline(h=0.5, col="blue")
  abline(h=1, col="red")
  abline(h=1.5, col="green")
  
  kNNdistplot(d.m.scaled, n) 
  abline(h=0.3, col="grey")
  abline(h=0.5, col="blue")
  abline(h=1, col="red")
  abline(h=1.5, col="green")
  
  cat("\n\n")
  cat("Se puede observar como existen elementos que probablemente sean outliers. Estos elementos
      afectarán al algoritmo de DBSCAN, ya que los outliers no le sientan bien. TODO: Buscar 
      referencias de Outliers para este algoritmo.")
  cat("\n\n")
  
  cat(paste("-->Media de la distancia: ", round(mean(a), 2), sep=""))
  
  #Ejecuramos la clusterización
  cat("\n\n<b>Distancia: 1</b>\n\n")
  cluster.dbscan.5 <- dbscan(d.m.scaled, 1, 5)
  cluster.dbscan.20 <- dbscan(d.m.scaled, 1, 20)
  
  cat("\n\n-->Número de elementos mínimos: 5\n\n")
  #cat("\n\n-->Datos del cluster(5): ")
  #cat(table(cluster.dbscan.5$cluster))
  #cat("\n\n")
  
  cat("\n\n-->Número de elementos mínimos: 20\n\n")
  #cat("\n\n-->Datos del cluster(20): ")
  #cat(table(cluster.dbscan.20$cluster))
  #cat("\n\n")
  
  cat("\n\n")
  cat("NOTA ANÁLISIS: Se puede observar como el número de clusters se ve afectado por el número mínimo 
      de elementos que se permiten en un cluster. Cuando n=5 aparecen 17 clusters (aunque del
      número 11 al 17 cada cluster tiene pocos elementos), cuando n=20 se quedan en 10 clusters.
      Vamos a probar con varios valores distintos para que podamos evaluar / interpretar distintos
      resultados.")
  cat("\n\n")
  
  rm(cluster.dbscan.5)
  rm(cluster.dbscan.20)
  rm(a) 
  
  return (d.m)
}

generaMatrizResultado <- function() {
  res <- as.data.frame(matrix(nrow=0, ncol=9))
  colnames(res) <- c("Método", "Cluster", "NumReg", "Genero1", "Genero2",
                      "EdadMin", "EdadMax", "Distancia", "NumElem")
  
  
  return (res)
}

iteraPorDistanciaElementos <- function(d, d.m, d.m.scaled, distancia, elementos, pintar) {
  
  #Preparamos la estructura donde vamos a almacenar el resultado.
  res <- generaMatrizResultado()
  
  for(i in distancia) {
    cat(paste("\n\n####Distancia ", i, "\n\n", sep=""))
    
    for(n in elementos) {
      cat(paste("\n\n#####Elementos ", n, "\n\n", sep=""))  
      
      cluster.dbscan <- dbscan(d.m.scaled, i, n)
      
      #Juntamos la información en los datos principales.
      db <- as.data.frame(cbind(d.m, cluster.dbscan$cluster))
      colnames(db)[length(colnames(db))] <- "DBSCAN"
      db <- as.data.frame(cbind(db, d$CRG))
      colnames(db)[length(colnames(db))] <- "CRG"
      db[,"DBSCAN"] <- as.factor(db[,"DBSCAN"])
      db[,"CRG"] <- as.factor(db[,"CRG"])
      db[db$Sexo == 0, "Sexo"] <- 2
      db[,"Sexo"] <- as.factor(db[,"Sexo"])
      
      if (pintar == TRUE) {
        pintarDBSCAN(anyo, i, n, db)
      }        

      #Guardamos el resultado de esta iteración en la matriz de resultados.
      res <- actualizaMatrizResultado(res, db, i, n)
    }
  }
  
  rm(cluster.dbscan)
  rm(i)
  rm(n)
  rm(db)
  
  return (res)
}

actualizaMatrizResultado <- function(res, db, i, n) {
  #Calculamos para cada Sexo.
  df_1 <- sqldf("select 'DBSCAN' as Método, DBSCAN as Cluster, Sexo, 
              count(*) as NumReg_1, 0 as NumReg_2, min(Edad) as EdadMin, max(Edad) as EdadMax 
              from db Where Sexo = 1 group by DBSCAN, Sexo")
  df_2 <- sqldf("select 'DBSCAN' as Método, DBSCAN as Cluster, Sexo, 
              0 as NumReg_1, count(*) as NumReg_2, min(Edad) as EdadMin, max(Edad) as EdadMax 
                from db Where Sexo = 2 group by DBSCAN, Sexo")
  
  #Unimos y calculamos el % de Género 1.
  df <- rbind(df_1, df_2)
  r <- sqldf("select Método, Cluster, sum(NumReg_1) as NumReg_1, sum(NumReg_2) as NumReg_2,
              min(EdadMin) as EdadMin, max(EdadMax) as EdadMax from df group by Método, Cluster")
  
  #Pasamos el resultado a res
  res_aux <- as.data.frame(matrix(nrow=nrow(r), ncol=9))
  colnames(res_aux) <- colnames(res)
  res_aux[, 1] <- r$Método
  res_aux[, 2] <- paste("C", r$Cluster, sep="")
  res_aux[, 3] <- r$NumReg_1+r$NumReg_2
  res_aux[, 4] <- ifelse( r$NumReg_2 == 0, 100, round(r$NumReg_1/(r$NumReg_1+r$NumReg_2), 2))
  res_aux[, 5] <- 100.00 - res_aux[, 4]
  res_aux[, 6] <- r$EdadMin
  res_aux[, 7] <- r$EdadMax
  res_aux[, 8] <- i
  res_aux[, 9] <- n
  
  res <- rbind(res, res_aux)
  
  rm(df)
  rm(df_1)
  rm(df_2)
  rm(r)
  rm(res_aux)
  
  return (res)
}

pintarDBSCAN <- function(anyo, i, n, db) {
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.2 - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución de pacientes por cluster</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, fill=db$DBSCAN)
  print(q)
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.3 - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución de pacientes por cluster y por Edad y Sexo</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$Edad, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y Sexo")
  print(q)
  
  #cat("\n\n")
  #cat(table(db$DBSCAN, db$Sexo))
  
  #cat("\n\n")
  #cat("NOTA ANÁLISIS: En 2011, salvo en el cluster 0, se puede observar que la custerización 
  #    discrimina bien por Sexo")
  #cat("\n\n")
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.4 - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución de pacientes por cluster y por Edad y CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$Edad, fill=db$CRG, geom="boxplot",
             xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y CRG-base")
  print(q)
  
  #cat("\n\n")
  #cat(table(db$DBSCAN, db$CRG))
  
  #cat("\n\n")
  #cat("NOTA ANÁLISIS: El CRG no ha sido una variable que se ha introducido. Se puede observar 
  #  que el número de tomas de medicamentos o el número de familias distintas de medicamentos que 
  #  se toma no sirve para discriminar el grado de cronicidad donde se encuentran")
  #cat("\n\n") 
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.5.Todos - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del número de medicamentos distintos por cluster y por Edad 
            , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$numATCDiabeticos+db$numATCOtros, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num.Medicamentos total", 
             main="Distribución de pacientes por Cluster y Sexo según el número de medicamentos 
             distintos que toma")
  print(q)
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.5.Diab - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del número de medicamentos distintos por cluster y por Edad 
            , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$numATCDiabeticos, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num.Medicamentos total", 
             main="Distribución de pacientes por Cluster y Sexo según el número de medicamentos 
             distintos que toma")
  print(q)
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.5.Otros - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del número de medicamentos distintos por cluster y por Edad 
            , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$numATCDiabeticos, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num.Medicamentos total", 
             main="Distribución de pacientes por Cluster y Sexo según el número de medicamentos 
             distintos que toma")
  print(q)
  
  #cat("\n\n")
  #cat(table(db$DBSCAN, db$CRG))
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.6.Todos - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del total de medicamentos distintos por cluster y por Edad 
            , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$totalATCDiabeticos+db$totalATCOtros, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num. Dispensaciones total", 
             main="Distribución de pacientes por Cluster y Sexo según el total de dispensaciones 
             que toma")
  print(q)
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.6.Diab - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del total de medicamentos distintos por cluster y por Edad 
                  , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$totalATCDiabeticos, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num. Dispensaciones total", 
             main="Distribución de pacientes por Cluster y Sexo según el total de dispensaciones 
                   que toma")
  print(q)
  
  cat(paste("\n\n<b>Gráfica CLUST.DBSCAN.6.Otros - Año:", anyo, " - Distancia: ", i, " - Elemento:", n,
            ". Distribución del total de medicamentos distintos por cluster y por Edad 
                  , CRG</b>\n\n", sep=""))
  q <- qplot(db$DBSCAN, db$totalATCOtros, fill=db$Sexo, geom="boxplot",
             xlab="Cluster", ylab="Num. Dispensaciones total", 
             main="Distribución de pacientes por Cluster y Sexo según el total de dispensaciones 
                   que toma")
  print(q)
}

pintarResultadoDBSCAN <- function(res) {
  #Pintamos el gráfico de bolas.
  df <- aggregate(Cluster ~ NumElem + Distancia, data = res, FUN = NROW)
  
  g <- ggplot(df, aes(x=Distancia, y=NumElem, size=Cluster)) + 
    geom_point(shape=21, fill="steelblue2") + 
    scale_x_continuous(name="Distancia", breaks=unique(df$Distancia)) +
    scale_y_continuous(name="Número de elementos mínimo en cluster", breaks=unique(df$NumElem)) +
    scale_size("Número Cluster", breaks=c(3, 8, 10, 20, 40, 60)) + 
    theme(axis.text.x = element_text(size = 10, colour = "black")) + 
    theme(title = element_text(size = 10, colour = "black")) + 
    theme(legend.text = element_text(size = 10)) + 
    theme(legend.key.height = unit (0.4, "cm")) + 
    labs(title="Relación del número de Cluster en función de los parámetros del método DBSCAN ")
  print(g)
  
  #Pintamos la evolución de los clusters en tamaño.
  colores = c("red", "blue", "green", "yellow",
              "red1", "blue1", "green1", "yellow1",
              "red2", "blue2", "green2", "yellow2")
  
  i <- 1
  par(mfrow=c(3,1), mar=c(4,4,3,2))
  for(cluster in unique(res$Cluster)) {
    c <- subset(res, res$Cluster == cluster)
    j=1
    
    xlab="Distancia"
    ylab="Número de Registros"
    ylim= range(c$NumReg)
    
    par(new = FALSE)
    for(elem in unique(c$Distancia)) {
      c2 <- subset(c, c$Distancia== elem)
      
      plot(c2$NumElem, c2$NumReg, type="l", col=colores[j], xaxt="n", main=paste("Cluster", cluster),
           xlab="Número de Elementos", ylab="Número de registros", ylim = ylim)
      if ( j == 1) {
        legend("topleft", ncol=2, title="Nº Elementos", 
               legend=unique(c$Distancia), pch=19, col=colores[((i-1)*4+1):((i-1)*4+4)])
        axis(1, at=unique(c2$NumElem), labels=unique(c2$NumElem))
      }
      par(new = TRUE)
      j <- j + 1
      xlab=""
      ylab=""
    }
    
    i<- i+1
  }
  
}
  


#K-MEANS
#cluster.kmean <- kmeans(d.m.scaled, 78, iter.max = 1000, nstart = 1)
#
#d.c <- as.data.frame(cbind(d.m, cluster.kmean$cluster))
#colnames(d.c)[length(colnames(d.c))] <- "KMEAN"
#d.c[,"KMEAN"] <- as.factor(d.c[,"KMEAN"])

#Vamos a ver la distribución número pacientes por cluster
#qplot(d.c$KMEAN, fill=d.c$KMEAN, geom="histogram")

#qplot(d.c$KMEAN, d.c$Edad, fill=d.c$KMEAN, geom="boxplot")

#APCLUSTER
#s <- negDistMat(r=2)
#cluster.ap <- apcluster(s, d.m.scaled)
#rm(s)



#Vamos a ver la distribución número pacientes por cluster
#qplot(d.c$DBSCAN, fill=d.c$DBSCAN, geom=c("histogram"))  
#  theme(axis.text.x = element_text(size = 8, colour = "red", angle = 45)))

#c <- table(d11.c$DBSCAN)
#cat <- sort(c)
#qplot(rownames(cat), cat, xlab="Cluster", ylab= "Número Pacientes", stat="identity",geom="histogram", fill=rownames(cat))
#plot(cat, type="s")
#rm(c)
#rm(cat)

#Distribución por edad de los medicamentos.
#qplot(d.c$DBSCAN, d.c$Edad, fill=d.c$Sexo, geom="boxplot",
#      xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y Sexo")

#table(d.c$DBSCAN, d.c$Sexo)

#Distribución por edad y CRG.
#      xlab="Cluster", ylab="Edad", main="Número de pacientes por Cluster, Edad y CRG")
#qplot(d.c$DBSCAN, d.c$Edad, fill=d.c$nivel, geom="boxplot",

#table(d.c$DBSCAN, d.c$nivel)

#Distribución por edad y número de medicamentos.
#qplot(d.c$DBSCAN, d.c$numATCDiabeticos+d.c$numATCOtros, fill=d.c$nivel, geom="boxplot",
#      xlab="Cluster", ylab="Num.Medicamentos total", 
#      main="Número de pacientes por Cluster, Edad y Número de medicamentos distintos que toma")

#table(d.c$DBSCAN, d.c$nivel)

#Distribución por edad y número total de dispensaciones.
#qplot(d.c$DBSCAN, d.c$totalATCTodos, fill=d.c$nivel, geom="boxplot",
#      xlab="Cluster", ylab="Num. Dispensaciones total", 
#      main="Número de pacientes por Cluster, Edad y Número total de dispensaciones")

#d <- d11.m
#d.m <- as.data.frame(d.m)

#Cluster 0

#resultado <- matrix(nrow=0, ncol=)


#cluster
#n <- 5
  
#Cluster 
#summary(d.m[d.m$DBSCAN==n, "Edad"])
#table(d.m[d.m$DBSCAN==n, "DBSCAN"])
#table(d.m[d.m$DBSCAN==n, "Sexo"])/nrow(d.m[d.m$DBSCAN==n,])
#table(d.m[d.m$DBSCAN==n, "CRG"])
#summary(d.m[d.m$DBSCAN==n, "totalATCTodos"])
#summary(d.m[d.m$DBSCAN==n, "numATCTodos"])
#summary(d.m[d.m$DBSCAN==n, "numATCDiabeticos"] + d.m[d.m$DBSCAN==n, "numATCOtros"])
#summary(d.m[d.m$DBSCAN==n, "totalATCDiabeticos"] + d.m[d.m$DBSCAN==n, "totalATCOtros"])
#qplot(d.m[d.m$DBSCAN==n, "totalATCOtros"], d.m[d.m$DBSCAN==n, "totalATCDiabeticos"], geom="boxplot")


i <- 1
cat(paste("Pacientes en N5424 en 2011: ", nrow(si5424), sep=""))
cat(paste("Pacientes que han seguido en N5424 en 2012: ", p[1,2], sep=""))
for( i in 2:nrow(p)) {
  cat(paste("Pacientes que han pasado de N5424 -> ", p[i,1], ": ", p[i,2], "\n", sep=""))
}
rm(i)
