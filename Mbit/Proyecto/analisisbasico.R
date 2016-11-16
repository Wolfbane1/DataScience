
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

colorAnyo <- c("springgreen2", "turquoise")
colorGenero <- c("slateblue3", "snow4")
colorGeneroSanos <- c("saddlebrown", "salmon2")
colorCRG <- c("thistle", 
              "green", "yellow4", "yellow4", "yellow4", "red1", "yellow4", "red2",
              "slateblue", "red3",       
              "yellow2", "green1", "green2", "green3", "yellow2", "yellow2", "yellow2", "red4", 
              "blue", "darkblue", "darkgrey")
colorCRGConSanos <- c("salmon1", "thistle", 
              "green", "yellow4", "yellow4", "yellow4", "red1", "yellow4", "red2",
              "slateblue", "red3",       
              "yellow2", "green1", "green2", "green3", "yellow2", "yellow2", "yellow2", "red4", 
              "blue", "darkblue", "darkgrey")
salidaGrafica = "/Users/zzddfge/Desktop/Compartida/Proyecto_Master/graficas/"
salidaDatos = "/Users/zzddfge/Desktop/Compartida/Proyecto_Master/salida_datos/"

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
#  Parámetros:
#    csv: Datos con los pacientes.
#    ficheroFiltro: Fichero con los IDs que no están adscritos al hospital de fuenlabrada
#    tipoPaciente: Si es diabético o es hipertenso por si hay que hacer ajustes distintos.
#
modificaDatos <- function(csv, ficheroFiltro, tipoPaciente, ejecutaAjustes = TRUE) {
  #FILTRO de datos.
  # Se tienen que filtrar los IDs que vienen en el fichero de Filtro porque son pacientes asociados a 
  # centros de salud no vinculados al HUF. Eso significa que no tenemos los ATCs de estos pacientes y
  # sin embargo el CRG puede ser muy variado, por lo que podrían "falsear" los resultados. 
  #Guardamos una copia de lo leído para "revisar"
  filtro <- procesaFicheroFiltro(ficheroFiltro)
  csv <- csv[!csv$Id %in% filtro$Id, ]
  
  if ( tipoPaciente == "Diabeticos" || tipoPaciente == "Sanos") {
    if (ejecutaAjustes == TRUE ) {
      #AJUSTES para 2011. 
      # No se han detectado ajustes específicos a realizar en 2011.
      
      #AJUSTES para 2012.
      # EDAD --> Todas las edades viene con un año menos por problemas en la generación. Se suma 1.
      csv[csv$Anyo == 2012, "Edad"] <- csv[csv$Anyo == 2012, "Edad"] + 1
      
      # PACIENTE 16158224 --> Viene con Edad -17. Nació en 1929 por lo que su edad real en 2012 es 83 años.
      csv[csv$Id == 16158224 & csv$Anyo == 2012, "Edad"] <- 83
    }  
  }else if (tipoPaciente == "Hipertensos" ) {
    if (ejecutaAjustes == TRUE ) {
      #AJUSTES para 2011. 
      # No se han detectado ajustes específicos a realizar en 2011.
      
      #AJUSTES para 2012.
      # EDAD --> Todas las edades viene con un año menos por problemas en la generación. Se suma 1.
      csv[csv$Anyo == 2012, "Edad"] <- csv[csv$Anyo == 2012, "Edad"] + 1
    }      
  }
  
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
## Función: procesaCSVSanos --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
#                          lee el fichero y genera las columnas necesarias.
#  Parámetros: 
#     ficheros -> Vector de nombres de ficheros a tratar. La posición de los ficheros es:
#        posición 1 --> Fichero CSV a leer con todos los datos. 
#        posicion 2 --> Fichero CSV a leer con los Ids de los pacientes que no se deben tratar. 
#        posicion 3 --> Fichero CSV con sanos
#
procesaCSVSanos <- function(ficheros) {
  ficheroFiltro <- ficheros[2]
  ficheroSanos <- ficheros[3]
  
  ##SANOS  
  #Lee el fichero de entrada. Se invoca a la función que aplica transformaciones sobre los CSV leídos.
  csv2 <- modificaDatos( read.csv( ficheroSanos ), ficheroFiltro, "Sanos", ejecutaAjustes = TRUE)
  
  #Se añaden las columnas.
  csv2 <- añadeColumnas(csv2)
  
  #Filtro global.
  csv2 <- filtroGlobal(csv2)
  
  return (csv2)
}

####################################
## Función: procesaCSVEnfermo --> Dado un nombre de fichero de destino (con la estructura CSV esperada)
#                          lee el fichero y genera las columnas necesarias.
#  Parámetros: 
#     ficheros -> Vector de nombres de ficheros a tratar. La posición de los ficheros es:
#        posición 1 --> Fichero CSV a leer con todos los datos. 
#        posicion 2 --> Fichero CSV a leer con los Ids de los pacientes que no se deben tratar. 
#        posicion 3 --> Fichero CSV con sanos
#     tipoPaciente -> Si son pacientes Hipertensos o Diabéticos
procesaCSVEnfermos <- function(ficheros, tipoPaciente) {
  ficheroDestino <- ficheros[1]
  ficheroFiltro <- ficheros[2]
  
  ##DIABETICOS  
  #Lee el fichero de entrada. Se invoca a la función que aplica transformaciones sobre los CSV leídos.
  csv <- modificaDatos(read.csv( ficheroDestino), ficheroFiltro, tipoPaciente)
  
  #Se añaden las columnas.
  csv <- añadeColumnas(csv)
  
  #Filtro global.
  csv <- filtroGlobal(csv)
  
  return (csv)
}

####################################
## Función: filtroGlobal --> Añadimos filtros globales que aplican a todos los años.
#
#
filtroGlobal <- function(csv) {

  #AJUSTES GLOBALES
  # ELIMINAR PACIENTES CON TODOS ATCs a 0's.
  csv <- csv[csv$totalATCTodos > 0, ]
  return (csv)
}

####################################
## Función: añadeColumnas --> Se añaden las columnas necesarias sobre el resumen de los ATCs.
#
#
añadeColumnas <- function(csv) {
  #Códigos ATC para diabéticos.
  ATCsDiabeticos <- c("A10AB","A10AC","A10AD","A10AE","A10AF","A10BA","A10BB","A10BC","A10BD","A10BF","A10BG","A10BH","A10BX","A10XA")
  cat(paste("Medicamentos que se consideran como ATCs para Diabéticos:"), imprime(ATCsDiabeticos), "\n\n", sep="")
  
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
  
  #csv <- csv[,-c(atcsVacios)]
  #Eliminación de las variables creadas
  rm(av)
  rm(atcsVacios)
  rm(ATCsDiabeticos)
  
  return(csv)
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

## Función: IC_low
#  Devuelve el valor mínimo de un intervalo de confianza, asumiendo que tiene dos colas. 
#  
#
IC_low <- function(X) {
  quantile(X, 0.025)
}

## Función: IC_low
#  Devuelve el valor máximo de un intervalo de confianza, asumiendo que tiene dos colas. 
#  
#
IC_high <- function(X) {
  quantile(X, 0.975)
}

## Función: generaMatriz
#  Función que genera una matriz de lon filas donde cada fila es la media de una muestra seleccionada
#  de forma aleatoria y sin elementos de repetición.
#  
#
generaMatriz <- function(csv, anyo, crg, lon, size) {
  #selección de anyo, crg, variables y obtener el id para el muestreo y el sexo para las gráficas.
  atc <- subset(csv, csv$Anyo == anyo & csv$CRG==crg, c(1,2,6,7:(ncol(csv) - N)))
  Id <- atc[,1]
  Sexo <- atc[,2]
  atc <- atc[,c(-1,-2,-3)]
  
  #Convertimos en matriz de existencia.
  t <- as.data.frame(apply(atc, 2, convierteExistencia))
  matriz <- matrix(nrow=0, ncol=length(t))
  
  #Añadimos el Id para el muestreo.
  t <- as.data.frame(cbind(Id, Sexo, t))
  
  #Ejecutamos lon veces el muestreo y calculamos la media de cada muestreo.
  for(i in 1:lon) {
    #Ajustamos el tamaño al 80% de lo que tenga un crg dado si no llega al tamaño especificado.
    size <- ifelse(length(Id) < size, length(Id)*0.8, size)
    
    Id_s <- sample(Id, size=size, replace=FALSE)
    t_b <- subset(t, t$Id %in% Id_s, c(-1, -2))
    
    media <- apply(t_b, 2, mean)
    matriz <- rbind(matriz, media)
  }
  
  t <- t[,c(-1,-2)]
  colnames(matriz) <- colnames(t)
  rownames(matriz) <- c()
  matriz <- as.data.frame(matriz)
  
  return(matriz)
}

## Función: generaMatrizSexo
#  Función que genera una matriz de lon filas donde cada fila es la media de una muestra seleccionada
#  de forma aleatoria y sin elementos de repetición. La selección de elementos es de forma aleatoria
#  pero luego se filtra por aquellos que corresponden al sexo especificado como parámetro.
#
generaMatrizSexo <- function(anyo, crg, lon, size, sexo) {
  #selección de anyo, crg, variables y obtener el id para el muestreo y el sexo para las gráficas.
  atc <- subset(csv, csv$Anyo == anyo & csv$CRG==crg, c(1,2,6,7:(ncol(csv) - N)))
  Id <- atc[,1]
  Sexo <- atc[,2]
  atc <- atc[,c(-1,-2,-3)]
  
  #Convertimos en matriz de existencia.
  t <- as.data.frame(apply(atc, 2, convierteExistencia))
  matriz <- matrix(nrow=0, ncol=length(t))
  
  #Añadimos el Id para el muestreo.
  t <- as.data.frame(cbind(Id, Sexo, t))
  
  #Ejecutamos lon veces el muestreo y calculamos la media de cada muestreo.
  for(i in 1:lon) {
    #Ajustamos el tamaño al 80% de lo que tenga un crg dado si no llega al tamaño especificado.
    size <- ifelse(length(Id) < size, length(Id)*0.8, size)
    
    Id_s <- sample(Id, size=size, replace=FALSE)
    t_b <- subset(t, t$Id %in% Id_s & t$Sexo == sexo, c(-1, -2))
    
    media <- apply(t_b, 2, mean)
    matriz <- rbind(matriz, media)
  }
  
  t <- t[,c(-1,-2)]
  colnames(matriz) <- colnames(t)
  rownames(matriz) <- c()
  matriz <- as.data.frame(matriz)
  
  return(matriz)
}

## Función: generaMatriz
#  Función que genera una matriz de lon filas donde cada fila es la media de una muestra seleccionada
#  de forma aleatoria y sin elementos de repetición. Para seleccionar los elementos 
#  
#
generaMatrizEvolucion <- function(anyo, crg, lon, size) {
  #selección de anyo, crg, variables y obtener el id para el muestreo y el sexo para las gráficas.
  IdEvolucion <- getIds2011CRGs2012(csv)
  atc <- subset(csv, csv$Id %in% IdEvolucion$Id & csv$Anyo == anyo & csv$CRG==crg, c(1,2,6,7:(ncol(csv) - N)))
  Id <- atc[,1]
  Sexo <- atc[,2]
  atc <- atc[,c(-1,-2,-3)]
  
  #Convertimos en matriz de existencia.
  t <- as.data.frame(apply(atc, 2, convierteExistencia))
  matriz <- matrix(nrow=0, ncol=length(t))
  
  #Añadimos el Id para el muestreo.
  t <- as.data.frame(cbind(Id, Sexo, t))
  
  #Ejecutamos lon veces el muestreo y calculamos la media de cada muestreo.
  for(i in 1:lon) {
    Id_s <- sample(Id, size=size, replace=FALSE)
    #t_b <- subset(t, t$Id %in% Id_s & t$Sexo == sexo, c(-1, -2))
    t_b <- subset(t, t$Id %in% Id_s, c(-1, -2))
    
    media <- apply(t_b, 2, mean)
    matriz <- rbind(matriz, media)
  }
  
  t <- t[,c(-1,-2)]
  colnames(matriz) <- colnames(t)
  rownames(matriz) <- c()
  matriz <- as.data.frame(matriz)
  
  return(matriz)
}

getIds2011CRGs2012 <- function(csv, listaCRG = c(5424, 6144, 7071)) {
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
  
  if ( is.null(listaCRG) == TRUE ) {
    Ids <- subset(Si_2011_Si_2012, Si_2011_Si_2012$CRG_11 == 5424, c(Id))
  } else {
    Ids <- subset(Si_2011_Si_2012, 
                  Si_2011_Si_2012$CRG_11 == 5424 & Si_2011_Si_2012$CRG_12 %in% listaCRG, 
                  c(Id))
  }

  return(Ids)
}

generaMatrizEvolucionSexo <- function(anyo, crg, lon, size, sexo) {
  #selección de anyo, crg, variables y obtener el id para el muestreo y el sexo para las gráficas.
  IdEvolucion <- getIds2011CRGs2012(csv)
  atc <- subset(csv, csv$Id %in% IdEvolucion$Id & csv$Anyo == anyo & csv$CRG==crg, c(1,2,6,7:(ncol(csv) - N)))
  Id <- atc[,1]
  Sexo <- atc[,2]
  atc <- atc[,c(-1,-2,-3)]
  
  #Convertimos en matriz de existencia.
  t <- as.data.frame(apply(atc, 2, convierteExistencia))
  matriz <- matrix(nrow=0, ncol=length(t))
  
  #Añadimos el Id para el muestreo.
  t <- as.data.frame(cbind(Id, Sexo, t))
  
  #Ejecutamos lon veces el muestreo y calculamos la media de cada muestreo.
  for(i in 1:lon) {
    Id_s <- sample(Id, size=size, replace=FALSE)
    t_b <- subset(t, t$Id %in% Id_s & t$Sexo == sexo, c(-1, -2))
    
    media <- apply(t_b, 2, mean)
    matriz <- rbind(matriz, media)
  }
  
  t <- t[,c(-1,-2)]
  colnames(matriz) <- colnames(t)
  rownames(matriz) <- c()
  matriz <- as.data.frame(matriz)
  
  return(matriz)
}

## Función: ejecutaWilcoxon
#  Dada dos matrices con los 746s ATCs, ejecutamos el método para identificar aquellos ATCs que tienen una distribución
#  distinta. El resultado lo añade a la matriz m de resultados y devuelve la matriz.
#  
#
ejecutaWilcoxonCRGs <- function(crg1, crg2, atcs_crg1, atcs_crg2) {
  #Matriz para almacenar el resultado.
  m <- matrix(nrow=0, ncol=6)
  colnames(m) <- c("CrgOrigen", "CrgDestino", "ATC", "MediaCrg1", "MediaCrg2", "pValue")
  
  #Iteramos para cada ATC.
  for (atc in colnames(atcs_crg1)) {
    suppressWarnings(res <- wilcox.test(atcs_crg1[,atc], atcs_crg2[,atc]))
    a <- matrix(nrow=1, ncol=6, c(crg1, crg2, atc, 
                                  round(mean(atcs_crg1[,atc]),4), round(mean(atcs_crg2[,atc]),4),
                                  round(res$p.value, 4)))
    m <- rbind(m, a)
  }
  
  rm(atc)
  rm(res)
  rm(a)

  return(m)
}

ajustaResultado <- function(m) {
  m <- as.data.frame(m)
  m$pValue <- ifelse(m$pValue == "NaN", 1000, as.numeric(as.character(m$pValue),4))
  m$MediaCrg1 <- as.numeric(as.character(m$MediaCrg1), 4)
  m$MediaCrg2 <- as.numeric(as.character(m$MediaCrg2), 4)
  m$Iteracion <- as.integer(as.character(m$Iteracion))
  m$CrgOrigen <- as.integer(as.character(m$CrgOrigen))
  m$CrgDestino <- as.integer(as.character(m$CrgDestino))
  m$Tipo <- as.character(m$Tipo)
  m$Año <- as.integer(as.character(m$Año))
  

  return(m)  
}


################
### Funciones para hacer más legible el fichero graficosMemoria.rmd
### Básicamente las funciones que pintan los gráficos. 
################

getDatosPacientesCoincidentes <- function(csv) {
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
  rm(n)

  return(Si_2011_Si_2012)
}

calculaPacientesCoincidencias <- function(csv) {
  #Vamos a calcular los pacientes que son comunes en 2011 y en 2012.
  d11 <- subset(csv, csv$Anyo == 2011)
  d12 <- subset(csv, csv$Anyo == 2012)
  
  #Vemos los pacientes que están en 2011 y en 2012.
  Si_2011_Si_2012 <- d11[d11$Id %in% d12$Id, c("Id","Genero", "Edad", "CRG")]
  n <- nrow(Si_2011_Si_2012)

  rm(d11)
  rm(d12)
  
  return(n)
}

pintaGrafica1 <- function(csv, tipoPaciente) {
  n <- calculaPacientesCoincidencias(csv)
  Anyo <- factor(csv$Anyo)
  cuentas <- suppressMessages(sqldf("select Anyo, count(*) as total from csv group by Anyo"))
  max <- max(cuentas$total)
  plot(Anyo, xlab="Año", ylab="Número de pacientes", ylim=c(0, max+300), col=colorAnyo)
  title(main=paste("Distribución datos por Año:", tipoPaciente), cex.main=0.7)
  text(1, max, labels=c(cuentas[1,2]), adj=0.5, font=4, cex=0.9)
  text(2, max, labels=c(cuentas[2,2]), adj=0.5, font=4, cex=0.9)
  abline(h=n, lwd=3, col="red")
  text(1, n+0.05*n, paste(n, " pacientes coincidentes."), col="red", cex = 0.7)
  
  rm(Anyo)
  rm(max)
  rm(n)
}

pintaGrafica2a <- function(csv, tipoPaciente) {
  #Sacamos las gráficas de distribución por Edad.
  i <- 1 
  for (anyo in unique(csv$Anyo)) {
    t <- table(subset(csv$Edad, csv$Anyo==anyo))
    plot(t, xlab = "Edad", ylab="Número de Pacientes", cex.axis = 0.7, type="h", col=colorAnyo[i])
    title(main=paste("Distribución de Edad en ", anyo, " (", tipoPaciente, ")", sep=""), cex.main=0.7)
    i<- i + 1
  }
  
  rm(i)
  rm(t)
  rm(anyo)
}

pintaGrafica3a <- function(csv, tipoPaciente) {
  cuentas <- sqldf("select Anyo, Sexo, count(*) as total from csv group by Anyo, Sexo")
  i <- 1
  for (anyo in unique(csv$Anyo)) {
    barplot(table(subset(csv$Sexo, csv$Anyo==anyo)), col=colorGenero, width = 21, 
            cex.names = 0.7, cex.axis = 0.7, 
            xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0, max(cuentas$total)+100))
    title(main=paste("Distribución de Sexo en ", anyo, " (", tipoPaciente, ")",
                     ". Total Pacientes = ", cuentas[i,3] + cuentas[i+1,3], sep=""), cex.main=0.7)
    text(14, 3000, labels=c(cuentas[i,3]), adj=0.5, font=4, cex=0.9)
    text(40, 3000, labels=c(cuentas[i+1,3]), adj=0.5, font=4, cex=0.9)
    i <- i + 2
  }
  rm(cuentas)
  rm(i)
  rm(anyo)
}

pintaGrafica3b <- function(diab, sanos) {
  cuentas <- sqldf("select Anyo, Edad, Sexo, count(*) as total from csv group by Anyo, Edad, Sexo")
  cuentasSanos<- sqldf("select Anyo, Edad, Sexo, count(*) as total from sanos group by Anyo, Edad, Sexo")
  for (anyo in unique(cuentas$Anyo)) {
    d <- subset(cuentas, cuentas$Anyo==anyo)
    s <- subset(cuentasSanos, cuentasSanos$Anyo==anyo)
    plot(subset(s, Sexo==2)$Edad, subset(s, Sexo==2)$total, 
         xlab = "Edad", ylab="Número de Pacientes", 
         cex.axis = 0.7, type="l", col=colorGeneroSanos[2], xaxt="n")
    lines(subset(s, Sexo==1)$Edad, subset(s, Sexo==1)$total, col=colorGeneroSanos[1])
    lines(subset(d, Sexo==1)$Edad, subset(d, Sexo==1)$total, col=colorGenero[1])
    lines(subset(d, Sexo==2)$Edad, subset(d, Sexo==2)$total, col=colorGenero[2])
    title(main=paste("Distribución de Edad y Género para ", anyo, sep=""), cex.main=0.7)
    legend("topright", ncol=2, legend = c("Género 1", "Género 2", "Género 1", "Género 2"), 
           title ="Sanos / Diabéticos", fill=c(colorGeneroSanos, colorGenero))
    axis(1,  at = unique(d$Edad), labels=unique(d$Edad), cex.axis=0.7)
  }
  rm(anyo)
  rm(cuentas)
  rm(cuentasSanos)
  rm(s)
  rm(d)
}

pintaGrafica4a <- function(csv) {
  cuentas <- cuentaCRGAño(csv)
  j <- 0
  for ( anyo in unique(csv$Anyo)) {
    d <- subset(cuentas, Anyo == anyo)
    suma <- sum(d$total)
    barplot(d$total, horiz=TRUE, beside = TRUE, las = 2, cex.names=0.7, cex.axis=0.7,col=colorCRG,
            names=as.character(factor(d$CRG)))
    title(paste(main="Distribución de Pacientes por CRG-base en ", anyo, sep=""), cex.main=0.7)
    for (i in 1:nrow(d)) {
      text(1900, i+(i-1)*0.2-0.5, 
           labels=c(paste(d[i,3], " [", round(100*d[i,3]/suma,2), "%]",sep="")), 
           adj=0.5, font=4, cex=0.6)
    }
    j<- j + 1
  }
  rm(i)
  rm(j)
  rm(d)
  rm(anyo)
  rm(suma)
  rm(cuentas)
}

pintaGrafica4b <- function(csv) {
  for (anyo in unique(csv$Anyo)) {
    cat(paste("\n\n <b>Año ", anyo, "</b>\n\n", sep=""))
    
    #Buscamos conseguir la lista de colores que tenemos que mostrar: 
    #rojo para indicar que es significativo, negro para indicar que no.
    color <- c()
    cat("\n\n--> Ejecución de los Test de Wilcoxon con p-value < 0.05 \n\n")
    for (i in unique(csv[csv$Anyo == anyo, "nivel"])) {
      cb <- "black"
      mu1 <- subset(csv$Edad, csv$Anyo == anyo & csv$nivel==i & csv$Genero == 1)
      mu2 <- subset(csv$Edad, csv$Anyo == anyo & csv$nivel==i & csv$Genero == 2)
      
      if (length(mu1) > 4 & length(mu2) > 4 ) {
        suppressWarnings(prueba <- wilcox.test(mu1, mu2))
        if (round(prueba$p.value, 2) < 0.05 ) {
          cb <- ifelse(length(mu1) <= 100 | length(mu2) <= 100, "red", "blue")
        }
        
        #Imprimimos para ver todos los resultados.
        cat(red(paste("\t\tCRG-base = ", i, ", p-Value: ", round(prueba$p.value, 5), 
                      ", Género 1: ", length(mu1), ", Género 2: ", length(mu2), "\n\n", sep="")))
      }
      color <- c(color, cb)
    }
    
    csv2 <- subset(csv, csv$Anyo == anyo)
    g <- ggplot(csv2, aes(x=nivel, y=Edad, fill=Genero)) + geom_boxplot(notch=TRUE) + 
      scale_fill_manual(values=colorGenero) +
      theme(axis.text.x = element_text(size = 7, colour = color)) + 
      theme(title = element_text(size = 7, colour = "black")) + 
      labs(x="CRG-base", y="Edad",
           title=paste("Distribución de pacientes según CRG-base y Sexo en ", anyo, sep=""))
    suppressMessages(print(g))
  }
  
  rm(anyo)
  rm(color)
  rm(cb)
  rm(mu1)
  rm(mu2)
  rm(prueba)
  rm(csv2)
  rm(g)
  rm(i)
}

pintaGraficasATCs <- function(d) {
  for (anyo in unique(d$Anyo)) {
    #Generamos título Nivel 2
    cat(paste("###Año ", anyo, "\n\n"))
    
    #Generamos el conjunto de datos.
    dx <- eliminaATCsVacios(d, anyo, 7:(ncol(d)-N))
    
    #Generamos las familias
    m_cuentaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "CUENTA_FAMILIAS")
    m_sumaFamilias <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "SUMA_FAMILIAS")
    m_existeFamilia <- reduceMatrizATC(dx, 7:(ncol(dx)-N), 1, "EXISTENCIA")
    
    #Generamos el gráfico de Existencia.
    cat("####Análisis de Existencia \n\n")
    m <- calculaBurbujaATC(dx, m_existeFamilia)
    
    #Pintamos el gráfico. 
    cat(paste("\n\n<b>Gráfica ATC.1.", anyo, " - Distribución de Existencia por CRG en la toma de ATC</b>\n\n", sep=""))
    g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
      scale_fill_manual("CRG-base", values=colorCRGConSanos) +
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
    cat("####Análisis de Cuentas\n\n")
    
    #Generamos el dato definitivo
    m <- calculaBurbujaATC(dx, m_cuentaFamilias)
    
    #Pintamos el gráfico. 
    cat(paste("\n\n<b>Gráfica ATC.2.", anyo, " - Distribución de Existencia por CRG en la toma de ATC></b>\n\n", sep=""))
    g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
      scale_fill_manual("CRG-base", values=colorCRGConSanos) +
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
    cat("####Análisis de Sumas\n\n")
    
    #Generamos el dato definitivo
    m <- calculaBurbujaATC(dx, m_sumaFamilias)
    
    #Pintamos el gráfico. 
    cat(paste("\n\n<b>Gráfica ATC.3.", anyo, " - Distribución de Existencia por CRG en la toma de ATC></b>\n\n", sep=""))
    g <- ggplot(m, aes(x=ATC, y=CRG_base, size = Total)) + geom_point(shape=21, aes(fill=CRG_base)) + 
      scale_fill_manual("CRG-base", values=colorCRGConSanos) +
      scale_size("Número") + 
      theme(axis.text.x = element_text(size = 8, colour = "black")) + 
      theme(title = element_text(size = 8, colour = "black")) + 
      theme(legend.text = element_text(size = 6)) + 
      theme(legend.key.height = unit (0.4, "cm")) + 
      labs(x="ATC", y="CRG-base", 
           title="Suma total de medicamentos por grupo de Familia ATC y por CRG")
    print(g)
    
    cat("\n\n\n")
  }
  
  rm(dx)
  rm(m_cuentaFamilias)
  rm(m_sumaFamilias)
  rm(m_existeFamilia)
  rm(m)
  rm(g)
  rm(anyo)
}

pintaGraficasPerfilesZoom <- function(csv, lineas, tipo) {
  m <- matrix(nrow=0, ncol=4)
  
  #Iteramos por año.
  for (anyo in unique(csv$Anyo)) {
    #Generamos título Nivel 2
    cat(paste("\n\n###Año ", anyo, "\n\n"))
    
    #Generamos el gráfico para cada CRG.
    cat("####Generacíon de gráficas individuales por CRG-base\n\n")
    
    atc <- subset(csv, csv$Anyo == anyo)
    atc <- atc[,c(6,7:(ncol(atc) - N))]
    i <- 1
    par(mfrow=c(2,1), mar=c(3,2,3,2))
    for (crg in unique(atc$CRG)) {
      cat(paste("\n\n<b>Gráfica ATC.CRG.", i, ".", anyo, ": ", crg, 
                " - Distribución de la Proporción de la toma de ATCs",
                ", Número de pacientes: ", length(atc[atc$CRG==crg, "CRG"]), "</b>\n\n", sep=""))
      
      #Filtramos por el ATC
      atc_crg <- subset(atc, atc$CRG == crg)
      atc_crg <- atc_crg[,-1]
      
      #Sumamos el total de las dispensaciones.
      if (tipo == 1) {
        atc_col <- 100*apply(atc_crg, 2, sum)/sum(atc_crg)
      }else if (tipo == 2) {
        t <- apply(atc_crg, 2, convierteExistencia)
        atc_col <- 100*apply(t, 2, sum)/nrow(t) 
        rm(t)
      }
      p <- atc_col
      n <- names(atc_col)
      a <- rep(anyo, length(p))
      c <- rep(crg, length(p))
      m <- rbind(m, cbind(a, c, n, p))
      rownames(m) <- c()
      
      label=c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
      pos=c(92,128,207,273,314,335,415,452,484,563,586,629,663,748)
      
      #Pintamos la gráfica de perfil.
      plot(atc_col, type = "l", cex.axis = 0.7, xaxt="n", 
           col=colorCRGConSanos[i], 
           xlab = "ATCs", ylab="Ocurrencia")
      axis(1, at=pos, labels= label, cex.axis = 0.5)
      title(main=paste("CRG:", crg, ", Año:", anyo, " - Ocurrencia de ATC", sep=""), 
            cex.main=0.65)
      
      #Pintamos la gráfica de ZOOM
      plot(atc_col[atc_col >= quantile(atc_col, 0.96)], type = "l", cex.axis = 0.7, xaxt="n",
           col=colorCRGConSanos[i], 
           xlab = "ATCs", ylab="Ocurrencia")
      points(atc_col[atc_col >= quantile(atc_col, 0.96)], pch=19)
      axis(1, at=1:length(names(atc_col[atc_col >= quantile(atc_col, 0.96)])), 
           labels=names(atc_col[atc_col >= quantile(atc_col, 0.96)]), cex.axis = 0.5, las=3)
      title(main=paste("ZOOM: ATCs con mayor Ocurrencia", sep=""), 
            cex.main=0.65)
      abline(h=lineas[1], col="red")
      abline(h=lineas[2], col="red")
      abline(h=lineas[3], col="red") 
      i <- i + 1
    }
  }
  
  rm(atc_crg)
  rm(atc_col)
  rm(atc)
  rm(i)
  rm(crg)
  rm(anyo)
  rm(label)
  rm(pos)
  rm(p)
  rm(n)
  rm(a)
  rm(c)
  
  #Una vez que se ha finalizado la ejecución para los años, obtenemos los valores mayores.
  m <- as.data.frame(cbind(m, as.character(m[,2])))
  colnames(m) <- c("Anyo", "CRG", "Names", "Prob", "PosX")
  m$Anyo <- as.factor(m$Anyo)
  m$CRG <- as.factor(m$CRG)
  m$Prob <- round(as.numeric(as.character(m$Prob)), 4)
  
  return (m)
}

pintaGráficaCristina <- function(atc, m, etiquetas, tipo) {
  m$PosX <- ifelse(m$CRG == 0, 1, m$PosX)
  m$PosX <- ifelse(m$CRG == 5424, 2, m$PosX)
  m$PosX <- ifelse(m$CRG == 6144, 3, m$PosX)
  m$PosX <- ifelse(m$CRG == 7071, 4, m$PosX)
  
  
  #Ahora hay que concatenar las columnas que no estén en los otros CRGs y que estén en el previo.
  u <- unique(atc[atc$CRG != 0, "Names"])
  atc_f <- subset(m, m$CRG %in% c(0, 5424, 6144, 7071) & m$Names %in% u)
  
  par(mfrow=c(1,2), mar=c(3,2,3,2))
  for (anyo in unique(m$Anyo)) {
    t <- subset(atc_f, atc_f$Anyo==anyo)
    t$CRG <- as.integer(as.character(t$CRG))
    t$Prob <- as.numeric(t$Prob)
    
    t0 <- subset(t, t$CRG==0)
    t5424 <- subset(t, t$CRG==5424)
    t6144 <- subset(t, t$CRG==6144)
    t7071 <- subset(t, t$CRG==7071)
    plot(t0$PosX, t0$Prob*2, type ="p", ylim=c(0, max(atc_f$Prob)*2), xlim=c(0.5, 4.5), yaxt="n", xaxt="n", pch=0)
    points(t5424$PosX, t5424$Prob*2, pch=1)
    points(t6144$PosX, t6144$Prob*2, pch=5)
    points(t7071$PosX, t7071$Prob*2, pch=2)
    axis(1,  at = unique(t$PosX), labels=unique(t$CRG), cex.axis=0.6)
    axis(2, at = etiquetas*2, 
         labels=etiquetas, cex.axis=0.6)
    title(main=paste("Año ", anyo, " - Evolución de ATCs por CRG-base", sep=""), cex.main=0.7)
    if (tipo == 1 ) pintaSegmentos1(t0, t5424, t6144, t7071)
    if (tipo == 2 ) pintaSegmentos2(t0, t5424, t6144, t7071)
  }
  
  rm(t5424)
  rm(t6144)
  rm(t7071)
  rm(t0)
  rm(u)
  rm(atc)
  rm(atc_f)
}

pintaGráficaCristinaHipertension <- function(atc, m, etiquetas, tipo) {
  m$PosX <- ifelse(m$CRG == 0, 1, m$PosX)
  m$PosX <- ifelse(m$CRG == 5190, 2, m$PosX)
  m$PosX <- ifelse(m$CRG == 6124, 3, m$PosX)
  m$PosX <- ifelse(m$CRG == 6144, 4, m$PosX)

  
  
  #Ahora hay que concatenar las columnas que no estén en los otros CRGs y que estén en el previo.
  u <- unique(atc[atc$CRG != 0, "Names"])
  atc_f <- subset(m, m$CRG %in% c(0, 5190, 6124, 6144) & m$Names %in% u)
  
  par(mfrow=c(1,2), mar=c(3,2,3,2))
  for (anyo in unique(m$Anyo)) {
    t <- subset(atc_f, atc_f$Anyo==anyo)
    t$CRG <- as.integer(as.character(t$CRG))
    t$Prob <- as.numeric(t$Prob)
    
    t0 <- subset(t, t$CRG==0)
    t5424 <- subset(t, t$CRG==5190)
    t6144 <- subset(t, t$CRG==6124)
    t7071 <- subset(t, t$CRG==6144)
    plot(t0$PosX, t0$Prob*2, type ="p", ylim=c(0, max(atc_f$Prob)*2), xlim=c(0.5, 4.5), yaxt="n", xaxt="n", pch=0)
    points(t5424$PosX, t5424$Prob*2, pch=1)
    points(t6144$PosX, t6144$Prob*2, pch=5)
    points(t7071$PosX, t7071$Prob*2, pch=2)
    axis(1,  at = unique(t$PosX), labels=unique(t$CRG), cex.axis=0.6)
    axis(2, at = etiquetas*2, 
         labels=etiquetas, cex.axis=0.6)
    title(main=paste("Año ", anyo, " - Evolución de ATCs por CRG-base", sep=""), cex.main=0.7)
    if (tipo == 1 ) pintaSegmentos1(t0, t5424, t6144, t7071)
    if (tipo == 2 ) pintaSegmentos2(t0, t5424, t6144, t7071)
  }
  
  rm(t5424)
  rm(t6144)
  rm(t7071)
  rm(t0)
  rm(u)
  rm(atc)
  rm(atc_f)
}

pintaSegmentos1 <- function(t0, t5424, t6144, t7071) {
  x1 <- 1.6
  x2 <- 4.4
  
  for(i in 1:nrow(t0)) {
    if (i %% 2 == 0) {
      text(x1, t5424[i,"Prob"]*2, t5424[i, "Names"], cex=0.6)
      segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col="steelblue")
      segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col="steelblue")
      segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col="steelblue")
    } else {
      text(x2, t7071[i,"Prob"]*2, t7071[i, "Names"], cex=0.6)
      segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col="red")
      segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col="red")
      segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col="red")
    }
  }
  
  rm(i)
  rm(x1)
  rm(x2)
}

pintaSegmentos2 <- function(t0, t5424, t6144, t7071) {
  x1 <- 1.6
  x2 <- 4.4
  
  for(i in 1:8) {
    text(x1, t5424[i,"Prob"]*2, t5424[i, "Names"], cex=0.6)
    segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col="steelblue")
    segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col="steelblue")
    segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col="steelblue")
  }
  for(i in 9:15) {
    text(x2, t7071[i,"Prob"]*2, t7071[i, "Names"], cex=0.6)
    segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col="red")
    segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col="red")
    segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col="red")
  }
  i<-16
  text(x1, t5424[i,"Prob"]*2, t5424[i, "Names"], cex=0.6)
  segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col="steelblue")
  segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col="steelblue")
  segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col="steelblue")
  
  rm(i)
  rm(x1)
  rm(x2)
}

pintaPerfilIC <- function(m, high, low, crg, anyo, fichero) {
  label=c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
  pos=c(92,128,207,273,314,335,415,452,484,563,586,629,663,748)
  par(mfrow=c(1,1), mar=c(3,2,3,2))
  png(filename = fichero, 
      width = 4096, height = 2048, 
      units = "px", bg = "transparent")
  plot(m, type = "l", cex.axis = 0.7, xaxt="n", 
       col="black", 
       xlab = "ATCs", ylab="Ocurrencia")
  points(high, col="red", type="l")
  points(low, col="red", type = "l")
  axis(1, at=pos, labels= label, cex.axis = 0.5)
  title(main=paste("CRG:", crg, ", Año:", anyo, " - Ocurrencia de ATC", sep=""), 
        cex.main=0.65)
  dev.off()
}

pintaDiferenciaMedia <- function(h, m, crg, anyo, fichero, color, leyenda) {
  label=c("A", "B", "C", "D", "G", "H", "J", "L", "M", "N", "P", "R", "S", "V")
  pos=c(92,128,207,273,314,335,415,452,484,563,586,629,663,748)
  par(mfrow=c(1,1), mar=c(3,2,3,2))
  png(filename = fichero, 
      width = 4096, height = 2048, 
      units = "px", bg = "transparent")
  plot(h, type = "l", cex.axis = 0.7, xaxt="n", 
       col=color[1], 
       xlab = "ATCs", ylab="Ocurrencia")
  points(m, col=color[2], type="l")
  axis(1, at=pos, labels= label, cex.axis = 0.5)
  title(main=paste("CRG:", crg, ", Año:", anyo, " - Ocurrencia de ATC", sep=""), 
        cex.main=0.65)
  legend("topleft", legend = leyenda, fill=color)
  dev.off()
}

pintaATCsWilcoxon <- function(g, sexo) {
  for (anyo in unique(g$Año)) {
    cat(paste("\n\n<b>Año ", anyo, "</b>\n\n", sep=""))
    gx <- subset(g, g$Año==anyo & g$pValue <= 0.0025, c(-1))
    gx <- cbind(gx, abs(gx$MediaCrg1 - gx$MediaCrg2))
    colnames(gx)[ncol(gx)] <- c("DifMedias")
    df <- sqldf("SELECT CrgOrigen, CrgDestino, ATC, DifMedias from gx group by CrgDEstino, ATC, DifMedias 
                order by CrgOrigen, CrgDestino, DifMedias desc")
    par(mfrow=c(1,1), mar=c(3,4,2,2))
    for (crg_origen in unique(df$CrgOrigen)) {
      dfo <- subset(df, df$CrgOrigen == crg_origen, c("CrgDestino", "ATC", "DifMedias"))
      for (crg_destino in unique(dfo$CrgDestino)) {
        cat(paste("\n\n<b>Crg Origen: ", crg_origen, " - Crg Destino: ", crg_destino, "</b>\n\n", sep=""))
        dfx <- subset(dfo, dfo$CrgDestino==crg_destino & dfo$DifMedias >= 0.03, c("ATC", "DifMedias"))
        dfx$ATC <- factor(as.character(dfx$ATC))
        dfx$Orden <- reorder(dfx$ATC, dfx$DifMedias, decreasing=TRUE)
        barplot(height=dfx$DifMedias, names.arg=dfx$ATC, las=2, cex.axis=0.6, cex.names=0.5,
                ylab="Diferencia de Medias")
        title(cex.main= 0.7, main=paste("Año ", anyo, " - Sexo [", sexo, "]",
                                        " [CRG Origen=", crg_origen, "] - ", " [CRG Destino=", crg_destino, "]",  
                                        sep=""))
      }
    }
  }
  
  rm(gx)
  rm(df)
  rm(dfx)
  rm(dfo)
  rm(anyo)
  rm(crg_destino)
  rm(crg_origen)
}

eliminaATCsVacios <- function(r) {
  #Eliminamos los ATCs que están vacíos.
  ATCs_vacios <- c()
  
  for (i in 1:ncol(r)) {
    if ( min(r[, i]) == 0 & max(r[, i]) == 0) {
      ATCs_vacios <- rbind(ATCs_vacios, colnames(r)[i])
    }
  } 
  
  rm(i)
  
  return(r[,!colnames(r) %in% ATCs_vacios])
}
