#Librería que lee la estructura de un fichero de matlab.
library(R.matlab)  

procesa <- function(origenData, origenMat, destinoCSV) {
  #Nombre Fichero cabecera es "ATCs.csv".
  cabecera <- procesaCabecera( file.path(origenData, "ATCs.csv") )
  ficheroDestino <- "pacientesDiabetes.csv"
  
  #Iteramos sobre los directorios de los años.
  dirAnyo <- dir(origenMat)
  csv <- c()
  for (anyo in dirAnyo) {
    #Se obtiene en una única estructura todos los registros de todos los ficheros.   
    datos <- procesaAnyo(anyo, file.path(origenMat, anyo))
    
    #Se concatena para seguir iterando.
    csv <- rbind(csv, datos)
  }
  
  #Añadimos la cabecera a la tabla final.
  csv <- as.data.frame(csv)
  colnames(csv) <- cabecera
  
  #Guardamos el fichero CSV
  write.csv(csv, file=file.path(destinoCSV, ficheroDestino), row.names = FALSE, quote=FALSE, sep = ";", col.names = TRUE)
}


#### 
# Función: procesaFichero -> Dado un fichero .mat lo carga y le añade las variables que son necesarios.
#
####
procesaFicheroMat <- function(pathCompleto, fichero, anyo) {
  #Cargamos el fichero
  mat <- readMat(pathCompleto)
  
  #Obtenemos las variables directas.
  
  #Obtenemos el patrón para la ejecución dinámica.
  matriz <- substr(fichero, 1, nchar(fichero) - 4)           # quitamos el .mat
  matriz <- gsub("[_]", ".", matriz)                        # cambiamos _ por .
  
  #"Id"
  objeto <- paste("Id <- rapply(mat$", matriz, "[1], c)")    # evaluamos la expresión
  eval( parse (text=objeto) )
  
  print(paste("ID=", fichero, "Registros=", length(Id)))
  
  #"Sexo"
  objeto <- paste("Sexo <- rapply(mat$", matriz, "[2], c)")  # evaluamos la expresión
  eval( parse (text=objeto) )
  
  #"Edad"
  objeto <- paste("Edad <- rapply(mat$", matriz, "[3], c)")  # evaluamos la expresión
  eval( parse (text=objeto) )

  #Obtenemos las variables derivadas
  RangoEdad <- rep(-1, length(Id))
  Anyo <- rep( as.numeric(anyo), length(Id) )
  
  #Obtenemos el CRG. 
  pos <- regexpr("\\d+X", fichero)
  CRG <- substr(fichero, pos, pos+3)
  CRGs <- rep(CRG, length(Id))
  
  #Obtenemos la matriz de ATCs.
  objeto <- paste("ATCs <- unlist(mat$", matriz, "[4])")     # evaluamos la expresión
  eval( parse (text=objeto) )
  dim(ATCs) <- c( length(Id), 746)                            # convertimos en matriz
  
  #Concatenamos todos los datos.
  dato <- cbind(Id, Sexo, Edad, RangoEdad, Anyo, CRGs, ATCs)
  
  return (dato)
}

#### 
# Función: procesaAnyo -> Dado un directorio de trabajo con ficheros .mat los lee y devuelve
#                         un DataFrame con la matriz de dentro. 
####
procesaAnyo <- function(anyo, dirAnyo) {
  #Iteramos para cada año.
  ficheros <- dir(dirAnyo)
  datos <- c()
  
  #Iteramos sobre los ficheros del directorio. 
  for (fichero in ficheros) {
    #Procesamos el fichero
    print(paste("Fichero:", file.path(dirAnyo, fichero), "Año:", anyo))
    d <- procesaFicheroMat( file.path(dirAnyo, fichero), fichero, anyo )
    
    #Vamos concatenando los ficheros.
    datos <- rbind(datos, d)
  }
  
  return (datos)
}

####
# Función: procesaCabecera -> Dado un fichero con la estructura de ATC, devuelve los campos del
#                             fichero final:
#                               Id        --> Pos 1 --> Viene en el fichero .mat
#                               Sexo      --> Pos 2 --> Viene en el fichero .mat
#                               Edad      --> Pos 3 --> Viene en el fichero .mat
#                               RangoEdad --> Pos 4 --> Lo añadimos para que se rellene después. 
#                               Anyo      --> Pos 5 --> Lo añadimos a partir de la organización de los ficheros.
#                               CRG       --> Pos 6 --> Lo añadimos a partir del nombre del fichero. 
#                               ATCs      --> Pos 7:753 --> Son 746 columnas que vienen en los ficheros .mat
# El objetivo es montar una matriz con toda la información necesaria para tener una única tabla.
####
procesaCabecera <- function(ficheroCabecera) {
  #Columnas iniciales.
  cabecera <- c("Id", "Sexo", "Edad", "RangoEdad", "Anyo", "CRG")
  
  #Se lee el fichero de cabeceras
  f <- read.csv(ficheroCabecera)

  #Se concatena las cabeceras.
  cabecera <- c(cabecera, names(f))
  rm(f)
  
  return(cabecera)
}

