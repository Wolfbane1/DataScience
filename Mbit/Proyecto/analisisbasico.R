
suppressWarnings(suppressMessages(library("ggplot2")))  #Librería de gráficos "mínimos".
suppressMessages(library("grid"))
suppressMessages(library("lattice"))  #Librería de gráficos para la comparativa de variables.
suppressMessages(library("sqldf"))    #Librería para tratar con SQL los objetos DataFrame.
suppressMessages(library("R.matlab")) #Librería para escribir los .mat
suppressMessages(library("reshape2"))
suppressMessages(library("crayon"))   #Librería para emitir textos de color. 
library(nortest)                      #Librería para usar el test de Lillieforst.
library(MASS)                         #Librería para usar el Test de Wilcoxon.

N <- 8

####################################
## Función: estadisticos 
#       --> Punto de entrada para ver los datos de un año concreto. 
#
estadisticos <-function(anyo, ficheroTomanA10Destino, ficheroNoTomanA10Destino) {
  d <- csv[csv$Anyo == anyo,]
  
  #Estadisticos básicos
  cat("###Edad")
  estadisticosEdad(d$Edad)
  
  cat("###Sexo")
  estadisticosSexo(d$Sexo)
  
  cat("###Id")
  estadisticosId(d$Id)
  
  cat("###Nivel CRG-base")
  estadisticosNivelCRG(d$nivel)
  
  #Estadisticos "básicos" complejos
  #ejecutaRangoEdad(d)
  #ejecutaComparativa(d)
  #revisaDispensacionATCsDiabeticos(d,ficheroTomanA10Destino, ficheroNoTomanA10Destino)
}


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

  print(paste("Toman A10:", nrow(tomanA10), " (", round(nrow(tomanA10)*100/nrow(csv),2), "%) - No toman A10:", nrow(noTomanA10), " (", round(nrow(noTomanA10)*100/nrow(csv)), "%)", sep=""))
        
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
  ## Género vs Nivel
  ######################
  
  ##
  # Histograma
  ##
  cat("**Gráfica Comparativa.1** -> Número de Pacientes por CRG-base y Género (Histograma)")
  print(
    qplot(csv2$nivel, geom="histogram", fill = csv2$Genero,
        xlab="CRG-base", ylab = "Número Pacientes", main = "Distribución de Pacientes por CRG-base y Género"))
  
  ##
  # Puntos
  ##
  df <- sqldf("select Genero, nivel, count(*) as num from csv2 group by Genero, nivel")
  
  cat("\n\n**Gráfica Comparativa.2** -> Número de Pacientes por CRG-base y Género (Burbujas)")
  print(
    qplot(df$Genero, df$nivel, geom="point", colour = df$Genero, size=df$num,
        ylab="CRG-base", xlab = "Género", main = "Distribución de Pacientes por CRG-base y Género"))
  
  rm(df)
  
  ######################
  ## Edad vs Nivel
  ######################
  
  ##
  # Histograma
  ##
  cat("\n\n**Gráfica Comparativa.3** -> Número de Pacientes por Edad y CRG-base (Histograma)")
  print(
    qplot(csv2$Edad, geom="histogram", fill = csv2$nivel,
        xlab="Edad", ylab = "Número Pacientes", main = "Distribución de Pacientes por Edad y CRG-base"))
  
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
## Función: obtieneRangoEdad --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
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
## Función: modificaDatos --> Punto centralizado en el código para que se puedan hacer transformaciones
#                             sobre los datos, como por ejemplo, el ajuste en la edad del año 2012
#                             que en los ficheros .mat recibidos venían con un dato menos.
#
#
modificaDatos <- function(csv, ficheroFiltro) {
  #FILTRO de datos.
  # Se tienen que filtrar los IDs que vienen en el fichero de Filtro porque son pacientes asociados a 
  # centros de salud no vinculados al HUF. Eso significa que no tenemos los ATCs de estos pacientes y
  # sin embargo el CRG puede ser muy variado, por lo que podrían "falsear" los resultados. 
  #Guardamos una copia de lo leído para "revisar"
  filtro <- procesaFicheroFiltro(ficheroFiltro)
  csv <- csv[!csv$Id %in% filtro$Id, ]
  
  #AJUSTES para 2011. 
  # No se han detectado ajustes específicos a realizar en 2011.
  
  #AJUSTES para 2012.
  # EDAD --> Todas las edades viene con un año menos por problemas en la generación. Se suma 1.
    csv[csv$Anyo == 2012, "Edad"] <- csv[csv$Anyo == 2012, "Edad"] + 1
  
  # PACIENTE 16158224 --> Viene con Edad -17. Nació en 1929 por lo que su edad real en 2012 es 83 años.
    csv[csv$Id == 16158224 & csv$Anyo == 2012, "Edad"] <- 83
  
    return(csv)
}

####################################
## Función: procesaFicheroFiltro --> Lee los datos del fichero con los IDs a filtrar.
#  Parámetros: ficheroFiltro --> Path al fichero que hay que leer.
#
procesaFicheroFiltro <- function(ficheroFiltro) {
  filtro <- read.csv(ficheroFiltro, sep=";")
  filtro <- as.data.frame(filtro[,1])
  colnames(filtro) <- c("Id")
  
  return (filtro)
}

####################################
## Función: procesaCSV --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
#                          lee el fichero y genera las columnas necesarias.
#  Parámetros: 
#     ficheros -> Vector de nombres de ficheros a tratar. La posición de los ficheros es:
#        posición 1 --> Fichero CSV a leer con todos los datos. 
#        posicion 2 --> Fichero CSV a leer con los Ids de los pacientes que no se deben tratar. 
#
procesaCSV <- function(ficheros) {
  
  ficheroDestino <- ficheros[1]
  ficheroFiltro <- ficheros[2]
  
  #Códigos ATC para diabéticos.
  ATCsDiabeticos <- c("A10AB","A10AC","A10AD","A10AE","A10AF","A10BA","A10BB","A10BC","A10BD","A10BF","A10BG","A10BH","A10BX","A10XA")
  cat(paste("Medicamentos que se consideran como ATCs para Diabéticos:"), imprime(ATCsDiabeticos), "\n\n", sep="")
  
  #Lee el fichero de entrada. Se invoca a la función que aplica transformaciones sobre los CSV leídos.
  #csv <- read.csv( ficheroDestino, nrows=100 )
  csv <- modificaDatos(read.csv( ficheroDestino), ficheroFiltro)
  
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

  csv <- cbind(csv, apply(csv[, colnames(csv)[7:752]], 1, sum))
  colnames(csv)[757] <- "totalATCTodos"
  
  #PUNTO 3: Se añaden tres variables nuevas contando el número de medicamentos (separando diabetes de no) y una total sin separla.
  csv <- cbind(csv, apply(csv[,colnames(csv) %in% ATCsDiabeticos], 1, cuentaMedicamentos))
  colnames(csv)[758] <- "numATCDiabeticos"
  
  csv <- cbind(csv, apply(csv[, colnames(csv) %in% setdiff(colnames(csv)[7:752], ATCsDiabeticos)], 1, cuentaMedicamentos))
  colnames(csv)[759] <- "numATCOtros"
  
  csv <- cbind(csv, apply(csv[, colnames(csv)[7:752]], 1, cuentaMedicamentos))
  colnames(csv)[760] <- "numATCTodos"
  
  #PUNTO 4: Se eliminan variables
  #Variables ATC que no tienen ninguna dispensación. Se eliminan para hacer una primera reducción.
  atcsVacios <- estadisticosPosATCsVacios(csv)
  av <- estadisticosATCsVacios(csv)
  #además, se saca un warning con el listado que no tienen dispensación
  #warning(paste("Warning:", length(av), " familias que no tienen dispensación:"))
  #warning(paste(av, "\n"))
  
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
  cat("\n\nResumen de ID: ")
  print(summary(Id))
  
  cat("\n\nIDs duplicados: ")
  print(!(length(Id) == length(unique(Id))))
}

####################################
## Función: estadisticosSexo --> Pretendemos hacer un análisis estadístico sencillo y básico sobre
#                              la distribución de los datos. 
#
#
estadisticosSexo <- function(Genero) {
  ##Sexo
  # Categórica: Dos Valores
  #   1 --> 4.278 (45,54%) 
  #   2 --> 5.114 (54,45%)
  cat("\n\n% de Pacientes por género")
  print(table(Genero)/length(Genero))
  
  cat("\n\nNúmero de Pacientes por género")
  print(melt(table(Genero)))
  
  cat("\n\n**Gráfica Género.1** -> Número de Pacientes por género")
  print(qplot(Genero, type="h", fill = Genero, ylab="Número de pacientes"))
}

## Edad
# Variable contínua
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.00   27.00   43.00   42.96   59.00   98.00 
#
# --> Mínimo: Edad de 0 registros. ¿Puede nacer un niño ya diabético?
# --> Máximo: Edad de 98 regitros.
# --> No tiene valores medio.
estadisticosEdad <- function(d) {
  cat("Resumen de la Edad:\n\n")
  print(summary(d$Edad)) 
  
  cat("\n\n**Gráfica Edad.1** -> Distribución de pacientes por Edad - Histograma")
  gg <- qplot(d$Edad, fill=d$Genero, 
        ylab = "Número Pacientes", xlab="Edad",
        main = "Distribución de Pacientes por Edad según Género")
  print(gg)

  cat("\n\n**Gráfica Edad.2** -> Distribución de pacientes por Edad - Boxplot")
  print(
    qplot(d$Genero, d$Edad, fill=d$Genero, geom="boxplot",
          main = "Distribución de Pacientes por Edad según Género",
          ylab = "Número Pacientes", xlab="Edad"))
  
  cat("\n\n**Gráfica Edad.3** -> Distribución de pacientes por Edad - Jitter")
  print(
    qplot(d$Edad, d$nivel, colour=d$Genero, geom = "jitter",
        ylab = "Edad", xlab="CRG-base",
        main="Distribución de pacientes por Edad, CRG-base y Género."))
  
#  quantile(d$Edad, probs = c(0.14, 0.28, 0.42, 0.56, 0.70, 0.86, 1.0) )
#  
#  #TODO: Hacer que esto se ejecute de forma dinámica. Rango lineal
#  d[d$Edad >= 0 & d$Edad < 18, "RangoEdad"] <- "[0, 18)"
#  d[d$Edad >= 18 & d$Edad < 29, "RangoEdad"] <- "[18, 29)"
#  d[d$Edad >= 29 & d$Edad < 37, "RangoEdad"] <- "[29, 37)"
#  d[d$Edad >= 37 & d$Edad < 48, "RangoEdad"] <- "[37, 48)"
#  d[d$Edad >= 48 & d$Edad < 57, "RangoEdad"] <- "[48, 57)"
#  d[d$Edad >= 57 & d$Edad < 66, "RangoEdad"] <- "[57, 66)"
#  d[d$Edad >= 66 & d$Edad <= 98, "RangoEdad"] <- "[66, 98]"
#  
#  qplot(d$RangoEdad)
#  
#  cluster <- kmeans(d$Edad, 7, iter.max = 99, nstart = 1)
#  
#  d$RangoEdad <- cluster$cluster
#  
#  qplot(d$RangoEdad)
#  
#  for (i in 1:7) {
#    print(summary(d[d$RangoEdad == i, "Edad"]))
#  }
#  
#  #TODO: Hacer que esto se ejecute de forma dinámica. #Rango agrupando solo la Edad.
#  d[d$Edad >= 0 & d$Edad <= 18, "RangoEdad"] <- "[0, 18]"
#  d[d$Edad >= 19 & d$Edad < 33, "RangoEdad"] <- "[19, 33]"
#  d[d$Edad >= 34 & d$Edad < 46, "RangoEdad"] <- "[34, 46]"
#  d[d$Edad >= 47 & d$Edad < 57, "RangoEdad"] <- "[47, 57]"
#  d[d$Edad >= 58 & d$Edad < 67, "RangoEdad"] <- "[58, 67]"
#  d[d$Edad >= 68 & d$Edad < 78, "RangoEdad"] <- "[68, 78]"
#  d[d$Edad >= 79 & d$Edad <= 98, "RangoEdad"] <- "[79, 98]"
#  
#  qplot(d$RangoEdad)
  
#  #Rango agrupando la edad y el género.
#  a <- cbind(d$Sexo, d$Edad)
#  cluster <- kmeans(a, 7, iter.max = 99, nstart = 1)
#  a <- cbind(a, cluster$cluster)
#  a <- as.data.frame(a)
#  colnames(a) <- c("Sexo", "Edad", "cluster")
#  
#  qplot(a$cluster, a$Edad, geom="boxplot", fill=as.character(a$cluster),
#        xlab="Sexo", ylab = "Edad", main = "Distribución de Pacientes por Edad y Género")
#  min_1 <- min(a[a$cluster==1,"Edad"])
#  
#  min_2 <- min(a[a$cluster==2,"Edad"])
#  min_3 <- min(a[a$cluster==3,"Edad"])
#  min_4 <- min(a[a$cluster==4,"Edad"])
#  min_5 <- min(a[a$cluster==5,"Edad"])
#  min_6 <- min(a[a$cluster==6,"Edad"])
#  min_7 <- min(a[a$cluster==7,"Edad"])
#  
#  max_1 <- max(a[a$cluster==1,"Edad"])
#  max_2 <- max(a[a$cluster==2,"Edad"])
#  max_3 <- max(a[a$cluster==3,"Edad"])
#  max_4 <- max(a[a$cluster==4,"Edad"])
#  max_5 <- max(a[a$cluster==5,"Edad"])
#  max_6 <- max(a[a$cluster==6,"Edad"])
#  max_7 <- max(a[a$cluster==7,"Edad"])
#  
#  RangoEdad <- cbind(c(min_1), c(max_1))
#  RangoEdad <- rbind(RangoEdad, c(min_2, max_2))
#  RangoEdad <- rbind(RangoEdad, c(min_3, max_3))
#  RangoEdad <- rbind(RangoEdad, c(min_4, max_4))
#  RangoEdad <- rbind(RangoEdad, c(min_5, max_5))
#  RangoEdad <- rbind(RangoEdad, c(min_6, max_6))
#  RangoEdad <- rbind(RangoEdad, c(min_7, max_7))
#  
  
#  RangoEdad
#  rm(RangoEdad)
#  rm(max_1)
#  rm(max_2)
#  rm(max_3)
#  rm(max_4)
#  rm(max_5)
#  rm(max_7)
#  rm(max_6)
#  rm(min_1)
#  rm(min_2)
#  rm(min_3)
#  rm(min_4)
#  rm(min_5)
#  rm(min_6)
#  rm(min_7)
#  rm(a)
}

## nivelCRG
#  Variable categórica con 21 valores posibles. 
#  No tiene valores nulos. 
#
estadisticosNivelCRG <- function(nivelCRG) {
  cat("\n\nResumen de la variable Nivel: \n")  
  print(summary(nivelCRG))

#  melt(sort(table(nivelCRG)))
  cat("\n\nDistribución de la variable Nivel: \n\n")  
  plot(x=nivelCRG, 
       col=c("blue"),
       ylab = "Número Pacientes", 
       main="Distribución de Pacientes por CRG"
  )
  
  cat("\n\nDistribución de la variable Nivel ordenando de menos a más: \n")  
  barplot(sort(table(nivelCRG)))
  
  #qplot(nivelCRG, xlab="CRG-base", ylab = "Número Pacientes", main = "Distribución de Pacientes por CRG-base", fill=I("blue"))
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
    if ( min(csv[,i]) == 0 & max(csv[,i]) == 0) {
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

## Función: cuentaMedicamentos
#  Calcula el número de medicamentos distintos que ha tomado un paciente.
#  
#
cuentaMedicamentos <- function(x) {
  c <- 0
  
  for (i in 1:length(x) ) {
    if ( x[i] > 0 ) {
      c <- c + 1
    }
  }
  return(c)
}

imprime <- function(v) {
  cad <- ""
  for (elemento in v) {
    cad <- paste(cad, elemento, ",", sep="")
  }
  return (cad)
}

## Función: cuentaCRGAño
#  Agrupa el número de pacientes por CRG en cada año. Se añade el CRG que falta para 2012 para que cuando se 
#  muestren comparativamente por año veamos siempre la misma proporción. 
#  
#
cuentaCRGAño <- function(csv) {
  c <- sqldf("select Anyo, CRG, count(*) as total from csv group by Anyo, CRG")
  
  #Se añade el registro que falta.
  df <- data.frame(c(2012), c(9010), c(0))
  colnames(df) <- colnames(c)
  c <- rbind(c, df)
  
  #Se devuelve la cuenta.
  return(c)
}
