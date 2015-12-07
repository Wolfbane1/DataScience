
library("ggplot2")  #Librería de gráficos "mínimos".
library("lattice")  #Librería de gráficos para la comparativa de variables.
library("sqldf")    #Librería para tratar con SQL los objetos DataFrame.
library("R.matlab") #Librería para escribir los .mat

####################################
## Función: revisaDispensacionATCsDiabeticos 
#       --> Revisa el grado de ATCs que toman los pacientes para ver si realmente están siendo 
#           medicados con los medicamentos de la familia A10.
#
revisaDispensacionATCsDiabeticos <- function(csv, ficheroTomanA10, ficheroNoTomanA10) {
  #Obtenemos una tablita con los pacientes asociados a cada CRG
  CRGs <- sqldf("select nivel, count(*) as Num_CRG, 'Total ATCs' as conDispensacion from csv group by nivel, null")
  
  #Obtenemos una tablita con los pacientes, por CRG, que han tomado medicamento de Diabéticos
  diabeticos <- as.data.frame(ifelse(csv[csv$totalATCDiabeticos > 0, "totalATCDiabeticos"], csv$CRG, NA))
  colnames(diabeticos) <- "nivel"
  
  CRGs_diabeticos <- sqldf("select nivel, count(*) as Num_CRG, 'ATCs Diabeticos' as conDispensacion from diabeticos group by nivel")
  
  #Añadimos los que si han tomado. 
  CRGs <- rbind(CRGs, CRGs_diabeticos)
  
  rm(CRGs_diabeticos)
  rm(diabeticos)
  
  barchart(as.character(CRGs$nivel) ~ CRGs$Num_CRG  | CRGs$conDispensacion, data = CRGs, layout = c(2, 1), horizontal=TRUE,
           ylab="Niveles CRG", xlab="Número de pacientes con dispensaciones",
           groups= CRGs$conDispensacion)
  
  rm(CRGs)
  
  #Una vez que hemos visto las gráficas, vamos a escribir los ficheros
  tomanA10 <- subset(csv, csv$totalATCDiabeticos > 0, select = c("Id", "totalATCDiabeticos", "totalATCOtros"))
  noTomanA10 <- subset(csv, csv$totalATCDiabeticos == 0, select = c("Id", "totalATCDiabeticos", "totalATCOtros")) 
  
  writeMat(ficheroTomanA10, x=tomanA10)
  writeMat(ficheroNoTomanA10, x=noTomanA10)
}

####################################
## Función: ejecutaComparativa --> Dado el conjunto de datos, ejecuta la generación de todas las.
#                                  gráficas para comprender visualmente la información.
#
#Variables
# -> Género (Sexo)       --> Categórica.
# -> Edad                --> Contínua.
# -> Rango de Edad       --> Categórica. 
# -> Nivel               --> Categórica. ¿Ordenada?
# -> Número de pacientes --> Contínua.
ejecutaComparativa <- function(csv2) {
  ######################
  ## Edad vs Género
  ######################
  
  ##
  # Histograma
  ##
  
  #Distribución por Edad y Género
  qplot(csv2$Edad, geom="histogram", fill = csv2$Genero,
        xlab="Edad", ylab = "Número Pacientes", main = "Distribución de Pacientes por Edad y Género")
  
  ##
  # Boxplot
  ##
  
  #Distribución por Edad y Género
  qplot(csv2$Genero, csv2$Edad, geom="boxplot", fill = csv2$Genero,
        xlab="Género", ylab = "Número Pacientes", main = "Distribución de Pacientes por Edad y Género")
  
  ######################
  ## Género vs Nivel
  ######################
  
  ##
  # Histograma
  ##
  
  qplot(csv2$nivel, geom="histogram", fill = csv2$Genero,
        xlab="Nivel", ylab = "Número Pacientes", main = "Distribución de Pacientes por Nivel y Género")
  
  ##
  # Boxplot
  ##
  
  df <- sqldf("select Genero, nivel, count(*) as num from csv2 group by Genero, nivel")
  
  qplot(df$nivel, df$Genero, geom="point", colour = df$Genero, size=df$num,
        xlab="Nivel", ylab = "Número Pacientes", main = "Distribución de Pacientes por Nivel y Género")
  
  rm(df)
  
  ######################
  ## Edad vs Nivel
  ######################
  
  ##
  # Histograma
  ##
  qplot(csv2$Edad, geom="histogram", fill = csv2$nivel,
        xlab="Edad", ylab = "Número Pacientes", main = "Distribución de Pacientes por Edad y Nivel CRG")
  
  ##
  # Boxplot
  ##
  qplot(csv2$nivel, csv2$Edad, geom="boxplot", fill = csv2$Genero,
        xlab="Nivel CRG", ylab = "Edad", main = "Distribución de Pacientes por Nivel, Edad y Género")
  
  df <- as.data.frame(cbind(csv2$Genero, csv2$RangoEdad, csv2$CRG))
  colnames(df) <- c("Genero", "RangoEdad", "nivel")
  df[,"nivel"] <- as.character(df[,"nivel"])
  rownames(df) <- rownames(csv2)
  
  ##
  # Jitter
  ##
  qplot(csv2$Edad, csv2$nivel, geom=c("jitter"), colour = csv2$Genero,
        xlab="Edad", ylab = "Nivel de CRG",
        main = "Distribución de Pacientes por Edad y Nivel CRG")
  
  ##
  # Puntos
  ##
  df <- sqldf("select Genero, Edad, nivel, count(*) as Num from csv2 group by Genero, Edad, nivel")
  
  qplot(df$Edad, df$nivel, geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes por Edad y Nivel CRG",
        colour = df$Genero, size=df$Num
  )
  
  #Aquí no se ve bien pintanto a la vez hombres y mujeres. Separamos entre hombres y mujeres.
  #Género 1
  qplot(df[df$Genero ==1 , "Edad"], df[df$Genero == 1, "nivel"], geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Género 1 por Edad y Nivel CRG",
        colour = I("red"), size=df[df$Genero == 1, "Num"]
  )
  
  #Género 2
  qplot(df[df$Genero ==2 , "Edad"], df[df$Genero == 2, "nivel"], geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Género 2 por Edad y Nivel CRG",
        colour = I("blue") , size=df[df$Genero == 2, "Num"]
  )
  
  NivelX1 <- df[df$Genero == 1 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "nivel"]
  NivelX2 <- df[df$Genero == 2 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "nivel"]
  
  EdadX1 <- df[df$Genero == 1 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Edad"]
  EdadX2 <- df[df$Genero == 2 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Edad"]
  
  NumX1 <- df[df$Genero == 1 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Num"]
  NumX2 <- df[df$Genero == 2 & df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Num"]
  
  Nivel <- df[df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "nivel"]
  Edad <- df[df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Edad"]
  Num <- df[df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Num"]
  Gen <- df[df$nivel %in% c(6120, 7070, 6130, 7011, 7010, 7012, 7012), "Genero"]
  
  #Género 1
  qplot(EdadX1, NivelX1, geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes de Género 1 por Edad y Nivel CRG",
        colour = I("red"), size=NumX1
  )
  
  #Género 2
  qplot(EdadX2, NivelX2, geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes de Género 2 por Edad y Nivel CRG",
        colour = I("blue"), size=NumX2
  )
  
  #Juntos
  qplot(Edad, Nivel, geom=c("point"), 
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes por Edad y Nivel CRG",
        colour = Gen, size=Num
  )
  
  rm(df)
  rm(NivelX1)
  rm(NivelX2)
  rm(EdadX1)
  rm(EdadX2)
  rm(NumX1)
  rm(NumX2)
  rm(Nivel)
  rm(Edad)
  rm(Num)
  rm(Gen)
  
  ##
  # Jitter
  ##
  #Juntos
  qplot(csv2$Edad, csv2$nivel, geom=c("jitter"), colour = csv2$Genero,
        xlab="Edad", ylab = "Nivel de CRG",
        main = "Distribución de Pacientes por Edad y Nivel CRG")
  
  #Género 1
  qplot(csv2[csv2$Genero==1, "Edad"], csv2[csv2$Genero==1, "nivel"], geom=c("jitter"), colour = I("red"),
        xlab="Edad", ylab = "Nivel de CRG",
        main = "Distribución de Edad, y Nivel CRG por Género 1")
  
  #Género 2
  qplot(csv2[csv2$Genero==2, "Edad"], csv2[csv2$Genero==2, "nivel"], geom=c("jitter"), colour = I("blue"),
        xlab="Edad", ylab = "Nivel de CRG", 
        main = "Distribución de Género 2 por Edad y Nivel CRG por Género 2")
  
  qplot(csv2$nivel, geom=c("jitter", "smooth"), method="loess", fill = csv2$nivel,
        xlab="Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes por Edad y Nivel CRG")
  
  qplot(csv2[csv2$Genero==1, "RangoEdad"], csv2[csv2$Genero==1, "nivel"], geom=c("jitter"), colour = c("red"),
        xlab="Rango de Edad", ylab = "Nivel de CRG", main = "Distribución de Pacientes de Género 1 por Edad y Nivel CRG"
  )
}

####################################
## Función: ejecutaRangoEdad 
#     --> Hace el ejercicio con la ejecución de todos los rangos de edad que tenemos.
#
ejecutaRangoEdad <- function(csv) {
  tipo = "[Q]"
  csv <- obtieneRangoEdad(1, csv)
  ejecutaComparativaRangoEdad(csv, tipo)
  
  tipo = "[K-Edad]"
  csv <- obtieneRangoEdad(2, csv)
  ejecutaComparativaRangoEdad(csv, tipo)
  
  tipo = "[K-Edad+Sexo]"
  csv <- obtieneRangoEdad(3, csv)
  ejecutaComparativaRangoEdad(csv, tipo)
  
  return(csv)
}

####################################
## Función: ejecutaComparativaRangoEdad 
#     --> Ejecuta la comparativa para cada rango de Edad.
#
ejecutaComparativaRangoEdad <- function(csv, tipo) {
  ##
  # Histograma
  ##
  qplot(csv$nivel, geom="histogram", fill = csv$RangoEdad,
        xlab="Edad", ylab = "Número Pacientes", 
        main = paste("Distribución de Pacientes por CRG y Rango de Edad", tipo))
  
  df <- sqldf("select Genero, RangoEdad, nivel, count(*) as Num from csv group by Genero, RangoEdad, nivel")
  
  ##
  # Jitter
  ##
  #Juntos
  qplot(csv$RangoEdad, csv$nivel, geom=c("jitter"), colour = csv$Genero,
        xlab="Edad", ylab = "Nivel de CRG",
        main = paste("Distribución de Pacientes por Rango de Edad ", tipo, ", Nivel CRG y Género.", sep="")
  )
  
  csvG1 <- csv[csv$Genero == 1, ]
  csvG2 <- csv[csv$Genero == 2, ]
  
  #Género 1
  qplot(csvG1$RangoEdad, csvG1$nivel, geom=I("jitter"), colour = I("red"),
        xlab="Rango de Edad", ylab = "Nivel de CRG",
        main = paste("Distribución de Pacientes de Género 1por Rango de Edad ", tipo, ", Nivel CRG.", sep="")
  )
  
  #Género 2
  qplot(csvG2$RangoEdad, csvG2$nivel, geom=I("jitter"), colour = I("blue"),
        xlab="Rango de Edad", ylab = "Nivel de CRG",
        main = paste("Distribución de Pacientes de Género 2 por Rango de Edad ", tipo, ", Nivel CRG.", sep="")
  )
  
  rm(df)
  rm(csvG1)
  rm(csvG2)
  
  ##
  # Puntos
  ##
  df <- sqldf("select Genero, RangoEdad, nivel, count(*) as Num from csv group by Genero, RangoEdad, nivel")
  
  #Mujeres
  qplot(df[df$Genero == 1, "RangoEdad"], df[df$Genero == 1, "nivel"], geom=c("point"), 
        xlab="Rango de Edad", ylab = "Nivel de CRG", 
        main = paste("Distribución de Pacientes de Género 1 por Rango de Edad ", tipo, ", Nivel CRG.",sep=""),
        colour = I("red"), size=df[df$Genero == 1, "Num"]
  )
  
  #Hombres
  qplot(df[df$Genero == 2, "RangoEdad"], df[df$Genero == 2, "nivel"], geom=c("point"), 
        xlab="Rango de Edad", ylab = "Nivel de CRG", 
        main = paste("Distribución de Pacientes de Género 2 por Rango de Edad ", tipo, ", Nivel CRG."),
        colour = I("blue"), size=df[df$Genero == 2, "Num"]
  )
  
  rm(df)
  
} 

####################################
## Función: procesaCSV --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
#                          lee el fichero y genera las columnas necesarias.
#Tipos de distribución
# -> 1: Distribución lineal en base a percentiles.
# -> 2: Distribución que salió del K-Means. 
#
obtieneRangoEdad <- function(tipo, csv) {
  if (tipo == 1) {
    #Cogemos la distribución "normal" en los 7 rangos iniciales.
    csv[csv$Edad >= 0 & csv$Edad < 18, "RangoEdad"] <- "[0, 18)"
    csv[csv$Edad >= 18 & csv$Edad < 29, "RangoEdad"] <- "[18, 29)"
    csv[csv$Edad >= 29 & csv$Edad < 37, "RangoEdad"] <- "[29, 37)"
    csv[csv$Edad >= 37 & csv$Edad < 48, "RangoEdad"] <- "[37, 48)"
    csv[csv$Edad >= 48 & csv$Edad < 57, "RangoEdad"] <- "[48, 57)"
    csv[csv$Edad >= 57 & csv$Edad < 66, "RangoEdad"] <- "[57, 66)"
    csv[csv$Edad >= 66 & csv$Edad <= 98, "RangoEdad"] <- "[66, 98]"
  }
  else if ( tipo == 2 ) {
    csv[csv$Edad >= 0 & csv$Edad <= 18, "RangoEdad"] <- "[0, 18]"
    csv[csv$Edad >= 19 & csv$Edad <= 33, "RangoEdad"] <- "[19, 33]"
    csv[csv$Edad >= 34 & csv$Edad <= 46, "RangoEdad"] <- "[34, 46]"
    csv[csv$Edad >= 47 & csv$Edad <= 57, "RangoEdad"] <- "[47, 57]"
    csv[csv$Edad >= 58 & csv$Edad <= 67, "RangoEdad"] <- "[58, 67]"
    csv[csv$Edad >= 68 & csv$Edad <= 78, "RangoEdad"] <- "[68, 78]"
    csv[csv$Edad >= 79 & csv$Edad <= 98, "RangoEdad"] <- "[79, 98]"
  }
  else if ( tipo == 3 ) {
    csv[csv$Edad >= 0 & csv$Edad <= 15, "RangoEdad"] <- "[0, 15]"
    csv[csv$Edad >= 16 & csv$Edad <= 27, "RangoEdad"] <- "[16, 27]"
    csv[csv$Edad >= 28 & csv$Edad <= 36, "RangoEdad"] <- "[28, 36]"
    csv[csv$Edad >= 37 & csv$Edad <= 47, "RangoEdad"] <- "[37, 47]"
    csv[csv$Edad >= 48 & csv$Edad <= 57, "RangoEdad"] <- "[48, 57]"
    csv[csv$Edad >= 58 & csv$Edad <= 70, "RangoEdad"] <- "[58, 70]"
    csv[csv$Edad >= 71 & csv$Edad <= 98, "RangoEdad"] <- "[71, 98]"
  }
  
  return (csv)
}

####################################
## Función: procesaCSV --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
#                          lee el fichero y genera las columnas necesarias.
#
#
procesaCSV <- function(ficheroDestino) {
  #Códigos ATC para diabéticos.
  ATCsDiabeticos <- c("A10AB","A10AC","A10AD","A10AE","A10AF","A10BA","A10BB","A10BC","A10BD","A10BF","A10BG","A10BH","A10BX","A10XA")
  
  #Lee el fichero de entrada.
  csv <- read.csv( ficheroDestino )
  
  #PUNTO 1: Se añaden variables "factor".
  #Variables categóricas: Se generan las variables categóricas a partir de los daots del fichero:
  #  -> nivel: Se genera a partir de la columna CRG (nivel de CRG) que está como numérico
  #  -> Genero: Se genera a partir de la columna Sexo.
  #Esta conversión es necesaria para los gráficos en R que necesitan que sea factores. 
  csv <- cbind(csv, as.character(csv$CRG))
  colnames(csv)[753] <- "nivel"
  
  csv <- cbind(csv, as.character(csv$Sexo))
  colnames(csv)[754] <- "Genero"
  
  #PUNTO 2: Se añaden variables que suman, por filas, todas las columnas de ATC.
  csv <- cbind(csv, apply(csv[,colnames(csv) %in% ATCsDiabeticos], 1, sum))
  colnames(csv)[755] <- "totalATCDiabeticos"
  
  csv <- cbind(csv, apply(csv[, colnames(csv) %in% setdiff(colnames(csv)[7:752], ATCsDiabeticos)], 1, sum))
  colnames(csv)[756] <- "totalATCOtros"
  
  #PUNTO 3: Se eliminan variables
  #Variables ATC que no tienen ninguna dispensación. Se eliminan para hacer una primera reducción.
  atcsVacios <- estadisticosPosATCsVacios(csv)
  av <- estadisticosATCsVacios(csv)
  #además, se saca un warning con el listado que no tienen dispensación
  warning(paste("Warning:", length(av), " familias que no tienen dispensación:"))
  warning(paste(av, "\n"))
  
  csv <- csv[,-c(atcsVacios)]
  
  #Eliminación de las variables creadas
  rm(av)
  rm(atcsVacios)
  rm(ATCsDiabeticos)
  
  return (csv)
}

####################################
## Función: estadisticosId --> Pretendemos hacer un análisis estadístico sencillo y básico sobre
#                              la distribución de los datos. 
#
#
estadisticosId <- function(Id) {
  #Id
  # -> No tiene sentido analizar la información proporcionada por summary. 
  # -> No tiene elementos duplicados.
  # -> No tiene valores NAs.
  summary(Id)
  length(Id) == length(unique(Id))
  is.na(Id) == TRUE
  
  print("ID --> Duplicados:NO;Nulos:NO")
}

####################################
## Función: estadisticosId --> Pretendemos hacer un análisis estadístico sencillo y básico sobre
#                              la distribución de los datos. 
#
#
estadisticosSexo <- function(Sexo) {
  ##Sexo
  # Categórica: Dos Valores
  #   1 --> 4.278 (45,54%) 
  #   2 --> 5.114 (54,45%)
  Sexo <- csv$Sexo
  summary(Sexo)
  table(Sexo)
  table(Sexo)/length(Sexo)
  plot(table(Sexo), type="h", col = c("red", "blue"), lwd = 100)

  print("Sexo --> 1 (4.278 - 45,54%);2 (5.114 - 54,45%)")
}

## Edad
# Variable contínua
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.00   27.00   43.00   42.96   59.00   98.00 
#
# --> Mínimo: Edad de 0 registros. ¿Puede nacer un niño ya diabético?
# --> Máximo: Edad de 98 regitros.
# --> No tiene valores medio.
estadisticosEdad <- function(Edad) {
  Edad <- csv$Edad
  summary(Edad)
  is.na(Edad)
  table(Edad)
  plot(table(Edad), 
       col=c("blue"),
       ylab = "Número Pacientes", 
       main="Distribución de Pacientes por Edad"
  )
  
  qplot(Edad, ylab = "Número Pacientes", main = "Distribución de Pacientes por Edad")
  qplot(table(Edad, Sexo), colours=c("red", "blue"))
  boxplot(Edad)
  
  boxplot(Edad~csv$Sexo)
  
  qplot(Edad, csv$Sexo, colour = csv$CRG, geom = "jitter", alpha = I(1 / 10))
  qplot(csv$Sexo, geom = "histogram", fill=csv$CRG)
  
  table(Sexo)/length(Sexo)
  plot(table(Sexo), type="h", col = c("red", "blue"), lwd = 100)
  
  quantile(Edad, probs = c(0.14, 0.28, 0.42, 0.56, 0.70, 0.86, 1.0) )
  
  csv2 <- csv
  
  #Rango lineal.
  csv2[csv$Edad >= 0 & csv$Edad < 18, "RangoEdad"] <- "[0, 18)"
  csv2[csv$Edad >= 18 & csv$Edad < 29, "RangoEdad"] <- "[18, 29)"
  csv2[csv$Edad >= 29 & csv$Edad < 37, "RangoEdad"] <- "[29, 37)"
  csv2[csv$Edad >= 37 & csv$Edad < 48, "RangoEdad"] <- "[37, 48)"
  csv2[csv$Edad >= 48 & csv$Edad < 57, "RangoEdad"] <- "[48, 57)"
  csv2[csv$Edad >= 57 & csv$Edad < 66, "RangoEdad"] <- "[57, 66)"
  csv2[csv$Edad >= 66 & csv$Edad <= 98, "RangoEdad"] <- "[66, 98]"
  
  qplot(csv2$RangoEdad)
  
  cluster <- kmeans(csv$Edad, 7, iter.max = 99, nstart = 1)
  
  csv2$RangoEdad <- cluster$cluster
  
  qplot(csv2$RangoEdad)
  
  for (i in 1:7) {
    print(summary(csv2[csv2$RangoEdad == i, "Edad"]))
  }
  
  #Rango agrupando solo la Edad.
  csv2[csv$Edad >= 0 & csv$Edad <= 18, "RangoEdad"] <- "[0, 18]"
  csv2[csv$Edad >= 19 & csv$Edad < 33, "RangoEdad"] <- "[19, 33]"
  csv2[csv$Edad >= 34 & csv$Edad < 46, "RangoEdad"] <- "[34, 46]"
  csv2[csv$Edad >= 47 & csv$Edad < 57, "RangoEdad"] <- "[47, 57]"
  csv2[csv$Edad >= 58 & csv$Edad < 67, "RangoEdad"] <- "[58, 67]"
  csv2[csv$Edad >= 68 & csv$Edad < 78, "RangoEdad"] <- "[68, 78]"
  csv2[csv$Edad >= 79 & csv$Edad <= 98, "RangoEdad"] <- "[79, 98]"
  
  qplot(csv2$RangoEdad)
  
  #Rango agrupando la edad y el género.
  d <- cbind(csv$Sexo, csv$Edad, csv$CRG)
  cluster <- kmeans(d, 7, iter.max = 99, nstart = 1)
  d <- cbind(d, cluster$cluster)
  d <- as.data.frame(d)
  colnames(d) <- c("Sexo", "Edad", "CRG", "cluster")
  
  qplot(d$cluster, d$Edad, geom="boxplot", fill=as.character(d$cluster),
        xlab="Sexo", ylab = "Edad", main = "Distribución de Pacientes por Edad y Género")
  
  min_1 <- min(d[d$cluster==1,"Edad"])
  min_2 <- min(d[d$cluster==2,"Edad"])
  min_3 <- min(d[d$cluster==3,"Edad"])
  min_4 <- min(d[d$cluster==4,"Edad"])
  min_5 <- min(d[d$cluster==5,"Edad"])
  min_6 <- min(d[d$cluster==6,"Edad"])
  min_7 <- min(d[d$cluster==7,"Edad"])
  
  max_1 <- max(d[d$cluster==1,"Edad"])
  max_2 <- max(d[d$cluster==2,"Edad"])
  max_3 <- max(d[d$cluster==3,"Edad"])
  max_4 <- max(d[d$cluster==4,"Edad"])
  max_5 <- max(d[d$cluster==5,"Edad"])
  max_6 <- max(d[d$cluster==6,"Edad"])
  max_7 <- max(d[d$cluster==7,"Edad"])
  
  RangoEdad <- cbind(c(min_1), c(max_1))
  RangoEdad <- rbind(RangoEdad, c(min_2, max_2))
  RangoEdad <- rbind(RangoEdad, c(min_3, max_3))
  RangoEdad <- rbind(RangoEdad, c(min_4, max_4))
  RangoEdad <- rbind(RangoEdad, c(min_5, max_5))
  RangoEdad <- rbind(RangoEdad, c(min_6, max_6))
  RangoEdad <- rbind(RangoEdad, c(min_7, max_7))
  
  RangoEdad
  
  rm(RangoEdad)
  rm(max_1)
  rm(max_2)
  rm(max_3)
  rm(max_4)
  rm(max_5)
  rm(max_6)
  rm(max_7)
  rm(min_1)
  rm(min_2)
  rm(min_3)
  rm(min_4)
  rm(min_5)
  rm(min_6)
  rm(min_7)
}

## nivelCRG
#  Variable categórica con 21 valores posibles. 
#  No tiene valores nulos. 
#
estadisticosNivelCRG <- function(nivelCRG) {
  summary(nivelCRG)
  is.na(nivelCRG)
  melt(sort(table(nivelCRG)))
  plot(x=nivelCRG, 
       col=c("blue"),
       ylab = "Número Pacientes", 
       main="Distribución de Pacientes por CRG"
  )
  
  barplot(sort(table(nivelCRG)))
  
  qplot(nivelCRG, ylab = "Número Pacientes", main = "Distribución de Pacientes por CRG")
}


## ATCs que están vacíos
#  Se identifican para quitarlos del experimento para una primera reducción.
#
#
estadisticosATCsVacios <- function(csv) {
  #Iteramos por cada columna para detectar las que la media es 0, es decir, no tiene 
  #ninguna dispensación.
  ATCs_vacios <- c()
  for (i in 7:752) {
    if ( mean(csv[,i]) == 0) {
      ATCs_vacios <- rbind(ATCs_vacios, colnames(csv)[i])
    }
  } 
  
  return(ATCs_vacios)
}

## ATCs que están vacíos
#  Se identifican para quitarlos del experimento para una primera reducción.
#
#
estadisticosPosATCsVacios <- function(csv) {
  #Iteramos por cada columna para detectar las que la media es 0, es decir, no tiene 
  #ninguna dispensación.
  ATCs_vacios <- c()
  for (i in 7:752) {
    if ( mean(csv[,i]) == 0) {
      ATCs_vacios <- rbind(ATCs_vacios, i)
    }
  } 
  
  return(ATCs_vacios)
}
