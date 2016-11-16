#Librería que lee la estructura de un fichero de matlab.
library(R.matlab)  

###################################################################################################
##### FUNCION: procesa. A partir de un directorio con ficheros en matlab procesa y los convierte
#####  a un CSV. 
##  ficheroCabeceraATCs: Fichero con la cabecera de la matriz de ATCs. 
##  origenMat: Directorio origen donde se encuentran los ficheros .mat. Por debajo debe haber
##             un directorio por año (2011, 2012, etc)
##  origenSanos: Directorio donde se encuentran los pacientes sanos. En un único fichero están
##               los de 2011 y 2012. Si se añaden más años, se tiene que tocar lo relacionado.
##  destinoCSV: Directorio de destino donde se van a generar los ficheros.
##  ficheroDestino: Fichero de salida (en función del tipo de paciente)
##  tipoPaciente: En función del tipo de paciente, como cada fichero es distinto, se tiene que hacer
##                una cosa u otra.
###################################################################################################
procesa <- function(ficheroCabeceraATCs, origenMat, origenSanos, ficheroSanos, destinoCSV, ficheroDestino, tipoPaciente) {
  #Nombre Fichero cabecera es "ATCs.csv".
  cabecera <- procesaCabecera( ficheroCabeceraATCs )
  
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
  if  (tipoPaciente == "Hipertensos") {
    csv$CRG <- ifelse(csv$CRG == "5XX_", "5192", as.character(csv$CRG))
    csv$CRG <- ifelse(csv$CRG == "612_", "6124", as.character(csv$CRG))
    csv$CRG <- ifelse(csv$CRG == "614_", "6144", as.character(csv$CRG))
  }
  write.csv(csv, file=ficheroDestino, row.names = FALSE, quote=FALSE, sep = ";", col.names = TRUE)

  #Procesamos ahora los Sanos
  procesaSanos(origenSanos, ficheroSanos, cabecera)
  
}

procesaSanos <- function(origenSanos, ficheroSanos, cabecera) {

  #Leemos fichero de Sanos
  sanos <- readMat(origenSanos)
  
  #"Id"
  Id <- unlist(sanos$MatrATC5el.norepe.Health11y12[1])

  #"Sexo"
  Sexo <- unlist(sanos$MatrATC5el.norepe.Health11y12[2])
  
  #"Edad"
  Edad <- unlist(sanos$MatrATC5el.norepe.Health11y12[3])

  #"ATC_2011"
  Atc_2011 <- unlist(sanos$MatrATC5el.norepe.Health11y12[4])
  dim(Atc_2011) <- c( length(Id), 746) 
  
  #"ATC_2012"
  Atc_2012 <- unlist(sanos$MatrATC5el.norepe.Health11y12[5])
  dim(Atc_2012) <- c( length(Id), 746) 
  
  #Obtenemos las variables derivadas
  CRGs <- rep(0, length(Id))
  RangoEdad <- rep(-1, length(Id))
  Anyo_2011 <- rep(2011, length(Id) )
  Anyo_2012 <- rep(2012, length(Id) )
  
  #Concatenamos todos los datos.
  dato_2011 <- cbind(Id, Sexo, Edad, RangoEdad, Anyo_2011, CRGs, Atc_2011)
  dato_2012 <- cbind(Id, Sexo, Edad, RangoEdad, Anyo_2012, CRGs, Atc_2012)
  dato <- rbind(dato_2011, dato_2012)
  colnames(dato) <- cabecera
  
  #Guardamos el fichero CSV
  write.csv(dato, file=ficheroSanos, row.names = FALSE, quote=FALSE, sep = ";", col.names = TRUE)
  
  rm(Id)
  rm(Edad)
  rm(RangoEdad)
  rm(CRGs)
  rm(Atc_2011)
  rm(Atc_2012)
  rm(Sexo)
  rm(Anyo_2011)
  rm(Anyo_2012)
  rm(dato_2011)
  rm(dato_2012)
  rm(dato)
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
  #pos <- regexpr("N+X", fichero)
  #CRG <- substr(fichero, pos, pos+3)
  
  pos <- regexpr("N[0-9]+", fichero)
  CRG <- substr(fichero, pos+1, pos+4)
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

