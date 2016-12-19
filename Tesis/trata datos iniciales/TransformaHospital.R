
#############################################################
#### Módulo para transformar los datos médicos
#############################################################

### El hospital nos ha ido dando diferentes ficheros para mejorar la calidad de la información.
### El total de ficheros son:
### 1.- Base de datos en MDB con la información de 2010, 2011, 2012 y 2013.
### 2.- Fichero con todos los pacientes del hospital. Este fichero no tiene la información de sexo y 
###     fecha de nacimiento para 14.705 pacientes.
### 3.- Fichero con 14K pacientes que tienen la información de sexo y fecha de nacimiento. 

### Tenemos que convertir la BD que hemos recibido desde el hospital
### para generar dos ficheros:
### a) Fichero con todos los pacientes únicos. Para ello usaremos los ficheros 2 y 3 del apartado de arriba.
### b) Fichero con todos los datos, en vertical, de lo que hay en los ficheros 1. 

#Directorio de trabajo lo asignamos al raiz. 
setwd("/Users/zzddfge/Desktop/Compartida/Tesis/Datos/original")

fichero_pacientes <- "Tabla_Clave.csv"
fichero_pacientes_extra <- "Tabla_Clave_Extra.csv"
fichero_CEX_01 <- "CEX_HOSP_e.csv" #fichero con los ids de 2013 para diferenciar la CEX del ámbito 01. 

#Importación de las librerías necesarias
library(sqldf)
library(reshape2)
library(stringr)

############################################################################################
## Función convierteCSVHospital
## Descripción:
##   Genera un bucle para tratar todos los CSV y genera un fichero con la información
##   de todos los pacientes y de todas las interacciones.

## Transformaciones:
##  1.- Leemos el CSV y procesamos las columnas.
##  2.- Tenemos que generar el fichero de todos los pacientes. Conforme se van leyendo 
##      ficheros con las interacciones, se van generando los pacientes que vienen y completando
##      una lista de pacientes únicos provenientes en los ficheros.
##      Para cada uno de los ficheros que estamos procesando:
##      2.a.- Generamos un resumen de la farmacia del fichero procesado. 
##      2.b.- TO-DO Generamos un resumen de los diagnósticos del fichero procesado. 
##
##  3.- Tenemos que ir generando un fichero con todas las interfacciones a partir del fichero anual
##      de interacciones. 
##      3.a.- Se modifica el código de procedimientos y diagnósticos para añadirle el '.'.
##
##  4.- Tratamiento de Pacientes. Generamos una tabla de pacientes con información procesada.
##      4.a) Se leen los pacientes que han enviado el hospital.
##      4.b) Se completa el fichero del hospital con la información de pacientes procesados en los
##           datos completos.
##      4.c) Se lee la información de los CRGs que ha enviado el hospital.
##      4.d) Se ejecutan una serie de reglas para ajustar la información de los CRGs, por ejemplo:
##           - Si un año intermedio está a NA y el año anterior y posterior tienen el mismo CRG se actualiza a éste.
##           - Si un año intermedio está a 10000 y el año anterior y posterior tienen el mismo CRG se actualiza a éste.
##           - TO-DO: Analizar información para aplicar más reglas coherentes desde un punto de vista clínico.
##      4.e) Actualizamos con el resumen de la farmacia por año
##
##  5.- Tatamiento de Interacciones.
##      5.a) Se ordenan las interacciones por Id, Anyo, Mes_Atencion.
##      5.b) Filtramos las interacciones por aquellas que pertenecen a pacientes que no están en el ámbito del hospital.
##      5.c) TO-DO: Chequeo de las interacciones que correspondan a diagnosticos que no están en la dimensión.
##      5.d) TO-DO: Chequeo de las interacciones que correspondan a procedimientos que no están en la dimensión.
##
##  6.- Escritura de ficheros:
##      6.a) Se escribe el fichero de pacientes. 
##      6.b) Se escribe el fichero de interacciones.
##      6.c) Se escribe el fichero de interacciones que no se van a procesar. 
##
## Parámetros:
##
## Devuelve:
##
############################################################################################
convierteCSVHospital <- function() {
  pacientes <- as.data.frame(matrix(nrow=0,ncol=3))
  names(pacientes) <- c("Id", "Sexo", "FechaNacimiento")
  
  atc_paciente <- as.data.frame(matrix(nrow=0,ncol=5))
  names(atc_paciente) <- c("Id", "Anyo", "NumAtcCom", "NumAtcHosp", "NumMeses")
  
  interacciones <- as.data.frame(matrix(nrow=0,ncol=7))
  names(interacciones) <- c("Id", "Anyo", "Mes_Atencion", "Ambito", "Tipo", "Codigo", "Posicion")
  
  #Bucle donde para cada fichero, se harán los tres primeros puntos espcificados en la cabecera
  for (fichero in dir(pattern = "datos_entrada*") ) {
    anyo <- as.numeric(substr(fichero, 15, 18))
      
    #1.- Leemos el CSV y procesamos las columnas.
    csv <- readCsv(fichero)
    
    #2.- Tenemos que generar el fichero de todos los pacientes: Tratamos los pacientes del fichero y
    #    añadimos a la lista de pacientes si no están ya
    p <- generaPacientes(csv[, c("Id", "Fecha_Nacimiento", "Sexo")])
    p <- p[! (p$Id %in% pacientes$Id), ]
    pacientes <- rbind(pacientes, p)
    pacientes <- pacientes[with(pacientes, order(Id)), ]
    
    # 2.a Generamos el resumen de la farmacia
    atc_paciente <- rbind(atc_paciente, consultaFarmacia(csv))
    
    #3.- Tenemos que generar el fichero de todas las interacciones
    # año, id, mes_atencion, ambito, tipo[A,D,P], Codigo, Posicion
    interacciones <- rbind(interacciones, generaInteracciones(csv, anyo))
    
    #guardamos CSV en una variable aparte
    #cmd <- paste("csv_", anyo, " <- csv", sep="")
    #eval( parse(text = cmd) )
  }
  rm(p)
  rm(cmd)
  rm(fichero)
  setwd("/Users/zzddfge/Desktop/Compartida/Tesis")
  fDimensionDiagnosticos <- file.path("Datos", "entrada", "dimensionDiagnosticos.csv")
  fDimensionProcedimientos <- file.path("Datos", "entrada", "dimensionProcedimientos.csv")
  diagnosticos <- read.table(file=fDimensionDiagnosticos, sep=";", header=TRUE, colClasses = "character")
  procedimientos <- read.table(file=fDimensionProcedimientos, sep=";", header=TRUE, colClasses = "character")
  
  #A partir de aquí tenemos que procesar dos cosas distintas: Tratar Pacientes e Interacciones.
  ##### Pacientes
  # 4.- Generación de un listado de todos los pacientes con el resumen de las interacciones que se han
  #     procesados.
  # 4.a Pacientes únicos. Procesado de los ficheros enviados por el hospital.
  fichero_pacientes <- file.path("Datos", "original", fichero_pacientes)
  fichero_pacientes_extra <- file.path("Datos", "original", fichero_pacientes_extra)
  pacientes_unicos <- leePacientes(fichero_pacientes, fichero_pacientes_extra)
  
  # 4.b Completitud de la información de Sexo, Fecha de Nacimiento. 
  pacientes_unicos <- completaInformacionDemografica(pacientes_unicos, pacientes)
  
  # 4.c Leemos la información de los crgs y completamos la información en columnas. 
  crgs <- leeCRGs()
  pacientes_unicos <- concatenaCRGs(pacientes_unicos, crgs)
  
  # 4.d Rellenamos los huecos de los CRGs.
  pacientes_unicos <- rellenaHuecosCRGs(pacientes_unicos)
  
  # 4.e Concatenamos la farmacia
  pacientes_unicos <- concatenaFarmacia(pacientes_unicos, atc_paciente)
  
  ##### Interacciones
    
  #5.a Ordenamos las interacciones por el Id, Año y Mes de Atención.
  interacciones <- interacciones[with(interacciones, order(Id, Anyo, Mes_Atencion)), ]

  #5.b Eliminamos las interacciones asociadas a pacientes que no están en la tabla de pacientes únicos.
  interacciones_delete <- interacciones[!interacciones$Id %in% pacientes_unicos$Id, ]
  interacciones <- interacciones[interacciones$Id %in% pacientes_unicos$Id, ]
  
  #6.- Escribimos los ficheros procesados.
  fPacientes <- file.path("Datos", "salida", "pacientes.csv")
  fPacientesCsv <- file.path("Datos", "salida", "pacientesDeCSV.csv")
  fInteracciones <- file.path("Datos", "salida", "interacciones.csv")
  fInteraccionesNoProcesadas <- file.path("Datos", "salida", "interacciones_pacientes_no_hospital.csv")
  write.csv(pacientes_unicos, file=fPacientes, row.names = FALSE)
  write.csv(pacientes, file=fPacientesCsv, row.names = FALSE)
  write.csv(interacciones, file=fInteracciones, row.names = FALSE)
  write.csv(interacciones_delete, file=fInteraccionesNoProcesadas, row.names = FALSE)
  
  #Liberamos las variables
  rm(crgs)
  rm(atc_paciente)
  rm(fDimensionDiagnosticos)
  rm(fDimensionProcedimientos)
  rm(fPacientes)
  rm(fPacientesCsv)
  rm(fInteracciones)
  rm(fInteraccionesNoProcesadas)
  rm(csv)
  rm(pacientes)
  rm(interacciones_delete)
  rm(anyo)
  
  pacientes <- pacientes_unicos
  rm(pacientes_unicos)
  
  rm(fichero_pacientes)
  rm(fichero_pacientes_extra)
  rm(fichero_CEX_01)
}

################################################################################
## Función 
##   generaInteracciones
## Descripción:
##    A partir de los datos que se han leído de un fichero, transforma en vertical los datos
##    que se tienen en una única fila. Se tienen que procesar:
##    a) Diagnósticos
##    b) Procedimientos
##    c) ATC
##
## Validaciones:
##    1.- Actuaciones sobre diagnósticos.
##        1.a Hay códigos de diagnósticos que no vienen con los tres dígitos. Añadir el 0 por delante
##        1.b Hay algunos códigos de diagnósticos que vienen con un '.'. Hay que eliminarlos. 
##        1.c Hay algunos códigos de diagnósticos que vienen con una ','. Hay que eliminarlas.
##        1.d Hay algunos códigos de diagnósticos que vienen con una '-'. Hay que eliminarlas.
##        1.e Hay algunos códigos de diagnósticos que vienen con un ' '. Hay que eliminarlos.
##        1.f Hay algunos códigos de diagnósticos que se leen mal por ser multibyte. ¿Qué hacemos?
##    2.- Se le añade el "." en los códigos de Diagnósticos y de Procedimientos para que cruce con la Dimensión.
## Parámetros:
##   datos --> Data Frame con la información del fichero que se tiene que transformar.
##   anyo --> Año de la interacción. 
## Devuelve:
##   Data Frame con la información leída de los CSV y las transformaciones aplicadas.
##   La estructura será: año, id, mes_atencion, ambito, tipo[A,D,P], Codigo, Posicion
################################################################################
generaInteracciones <- function(datos, anyo) {
  interacciones <- as.data.frame(matrix(nrow=0,ncol=7))
  names(interacciones) <- c("Id", "Anyo", "Mes_Atencion", "Ambito", "Tipo", "Codigo", "Posicion")
  
  for (ambito in unique(datos$Ambito)) {
    
    if (ambito == 3) {
      #Preparamos los datos del ATC de la Comunidad
      d <- datos[datos$Ambito == 3 & !is.na(datos$ATC_Com), c("Id", "Mes_Atencion", "Ambito", "ATC_Com")]
      d$Anyo <- anyo
      d$Tipo <- "ATC_COM"
      d$Posicion <- 1
      d <- d[,c("Id", "Anyo", "Mes_Atencion", "Ambito", "Tipo", "ATC_Com", "Posicion")]
      names(d)[6] <- "Codigo"
      
      #Preparamos los datos del ATC del Hospital
      dh <- datos[datos$Ambito == 3 & !is.na(datos$ATC_Hos), c("Id", "Mes_Atencion", "Ambito", "ATC_Hos")]
      dh$Anyo <- anyo
      dh$Tipo <- "ATC_HOS"
      dh$Posicion <- 1
      dh <- dh[,c("Id", "Anyo", "Mes_Atencion", "Ambito", "Tipo", "ATC_Hos", "Posicion")]
      names(dh)[6] <- "Codigo"
    
      #Concatenamos los datos.
      interacciones <- rbind(interacciones, d)
      interacciones <- rbind(interacciones, dh)
      
      rm(dh)
    } else if (ambito %in% c(1, 4, 7)) {
      #Transformamos en vertical.
      d <- datos[datos$Ambito == ambito, c(1,4:21,22:33,35,37:38) ]
      dh <- melt(d, id.vars=c("Id", "Anyo", "Mes_Atencion", "Ambito"))
      
      #Hay que quitar los registros que están a nulos y ponemos los nombres.
      dh <- dh[!is.na(dh$value), ]
      names(dh)[5:6] <- c("Tipo", "Codigo")
      dh$Codigo <- as.character(dh$Codigo)
      
      #Generamos los otros dos columnas que se esperan.
      dh$Posicion <- substr(dh$Tipo, 2, 3)
      dh$Tipo <- substr(dh$Tipo, 1, 1)
      
      #Se le incluye el '.' en el código de diagnosticos (xxx.xx) y de procedimientos (xx.xx). No se hace
      #nada con los códigos M. 
      aux <- dh[substr(dh$Codigo,1,1) == "M", ]
      dh <- dh[substr(dh$Codigo,1,1) != "M", ]
      
      #Validaciones de Diagnósticos
      # 1.a, añadimos el "0" a los códigos con menor de 3 dígitos
      dh[dh$Tipo == "D" & nchar(dh$Codigo) < 3, "Codigo"] <- paste("0", dh[dh$Tipo == "D" & nchar(dh$Codigo) < 3, "Codigo"], sep="")
      # 1.b a 1-d, eliminamos los carácteres que se deben eliminar. 
      dh[dh$Tipo == "D", "Codigo"] <- str_replace(dh[dh$Tipo == "D", "Codigo"], "[,.-]", "")
      # 1.e, eliminamos el espacio después del .
      dh[dh$Tipo == "D", "Codigo"] <- str_replace(dh[dh$Tipo == "D", "Codigo"], "[.]\\s{1}", "")
      
      dh$Codigo <- ifelse(dh$Tipo == "D",
                        paste(substr(dh$Codigo, 1, 3), ".", substr(dh$Codigo, 4, 5), sep=""),
                        paste(substr(dh$Codigo, 1, 2), ".", substr(dh$Codigo, 3, 4), sep=""))
      dh$Codigo <- ifelse(substr(dh$Codigo, nchar(dh$Codigo), nchar(dh$Codigo)) == ".", 
                          substr(dh$Codigo, 1, nchar(dh$Codigo) - 1),
                          dh$Codigo)
      
      interacciones <- rbind(interacciones, dh)
      interacciones <- rbind(interacciones, aux)
      rm(dh)
      rm(aux)
    }
  }
  rm(d)
  rm(ambito)
  
  return(interacciones)
}

################################################################################
## Función 
##   rellenaHuecosCRGs
## Descripción:
##   A partir de las columnas que tienen CRG, se rellena los huecos en los CRGs
##   intermedios cuando el CRG posterior y el CRG anterior son los mismos.
##
## Validaciones:
## Parámetros:
##   pacientes --> Matriz con todos los pacientes.
## Devuelve:
##   Data Frame con la información leída y con los CRGs actualizados.
################################################################################
rellenaHuecosCRGs <- function(pacientes) {
  #Recuperamos la lista de años y quitamos el primer año y el último
  anyos <- creaAnyos(colnames(pacientes))
  anyos <- anyos[2:(length(anyos)-1)]
  pacientesModificados <- as.data.frame(matrix(ncol = 4, nrow = 0))
  colnames(pacientesModificados) <- c("Id", "CRG_Anyo", "Valor", "Tipo")
  
  for(anyo in anyos) {
    anyo <- as.numeric(anyo)
    crg_anyo <- paste("CRG_", anyo, sep="")
    crg_anyo_inferior <- paste("CRG_", anyo -1, sep="")
    crg_anyo_superior <- paste("CRG_", anyo +1, sep="")
    
    p <- pacientes[is.na(pacientes[, crg_anyo]),]
    
    #1.- Cambiamos nulls por el anterior, cuando los anteriores y posteriores son iguales.
    e <- paste("aux <- p[is.na(p$", crg_anyo, ") & p$", crg_anyo_inferior, " == ", "p$", crg_anyo_superior,  
               " & !is.na(p$", crg_anyo_inferior, ") & !is.na(p$", crg_anyo_superior, "), ]", sep="")
    eval( parse(text = e) )
    aux$CRG_Anyo <- crg_anyo
    e <- paste("aux$Valor <- aux$", crg_anyo_inferior, sep="")
    eval( parse(text = e) )
    aux$Tipo <- "Nulos"
    
    pacientesModificados <- rbind(pacientesModificados, aux[, c("Id", "CRG_Anyo", "Valor", "Tipo")])
    pacientes[pacientes$Id %in% aux$Id, crg_anyo] <- aux$Valor
    
    #2.- Cambiamos 10000 por el anterior, cuando los anteriores y posteriores son iguales y distinto de 10000.
    p <- pacientes[pacientes[,crg_anyo]==10000,]
    e <- paste("aux <- p[p$", crg_anyo, "== 10000 & p$", crg_anyo_inferior, " == ", "p$", crg_anyo_superior,  
               " & p$", crg_anyo_inferior, "!= 10000",
               " & !is.na(p$", crg_anyo_inferior, ") & !is.na(p$", crg_anyo_superior, "), ]", sep="")
    eval( parse(text = e) )
    aux$CRG_Anyo <- crg_anyo
    e <- paste("aux$Valor <- aux$", crg_anyo_inferior, sep="")
    eval( parse(text = e) )
    aux$Tipo <- "Cambio Sanos"
    
    pacientesModificados <- rbind(pacientesModificados, aux[, c("Id", "CRG_Anyo", "Valor", "Tipo")])
    pacientes[pacientes$Id %in% aux$Id, crg_anyo] <- aux[pacientes$Id %in% aux$Id, "Valor"]
  }
  
  #Escribimos a disco el fichero de los Ids modificados.
  write.csv(pacientesModificados, file= file.path(getwd(), "Datos", "salida", "huecosRellenados.csv"), quote=FALSE, row.names = FALSE)
  
  #Sobreescribimos el fichero de salida.
  #write.csv(pacientes_unicos, file= file.path(getwd(), "Datos", "pacientes_unicos.csv"), quote=FALSE, row.names = FALSE)
  #pacientesModificados <- read.csv(file= file.path(getwd(), "Datos", "salida", "huecosRellenados.csv"))
  
  
  rm(anyo)
  rm(anyos)
  rm(crg_anyo)
  rm(crg_anyo_inferior)
  rm(crg_anyo_superior)
  rm(p)
  rm(aux)
  rm(e)
  rm(pacientesModificados)
  
  return (pacientes)
}

################################################################################
## Función 
##   concatenaFarmacia
## Descripción:
##    A partir de la información que se ha generado en la lectura de los ficheros 
##    concateneramos la información de la farmacia en la BD de pacientes. 
## Validaciones:
## Parámetros:
##   pacientes --> Data Frame con la información de los pacientes.
##   atc_pacientes --> Data Frame con la información y relación de pacientes y dispensación ATCs.
## Devuelve:
##   Data Frame con la información de los pacientes actualizada.
################################################################################
concatenaFarmacia <- function(pacientes, atc_paciente) {
  for (anyo in unique(atc_paciente$Anyo)) {
    p <- sqldf(paste("SELECT Id, Anyo, sum(CuentaAtcCom) as NumAtcCom, sum(CuentaAtcHos) as NumAtcHosp, sum(CuentaMeses) as NumMeses ",
                     "FROM atc_paciente WHERE Anyo = ", anyo, " ",
                     "GROUP BY Id, Anyo ORDER BY Id", sep="")) 
    
    #Concatenamos el número de ATCs de la Comunidad
    cmd = paste("pacientes[pacientes$Id %in% p$Id, ", "'ATC_", anyo, "_Com", "'] <- p[p$Id %in% pacientes$Id, 'NumAtcCom']", sep="")
    eval( parse(text=cmd) )
    
    #Concatenamos el número de ATCs del Hospital
    cmd = paste("pacientes[pacientes$Id %in% p$Id, ", "'ATC_", anyo, "_Hosp", "'] <- p[p$Id %in% pacientes$Id, 'NumAtcHosp']", sep="")
    eval( parse(text=cmd) )
  
    #Concatenamos el número de meses donde hay ATCs
    cmd = paste("pacientes[pacientes$Id %in% p$Id, ", "'ATC_", anyo, "_NumMeses", "'] <- p[p$Id %in% pacientes$Id, 'NumMeses']", sep="")
    eval( parse(text=cmd) ) 
    
  }
  rm(p)
  rm(cmd)
  
  return(pacientes)
}

################################################################################
## Función 
##   consultaFarmacia
## Descripción:
##    A partir de la información que leída del hospital, generamos una nueva matriz para
##    comprobar si un paciente ha tenido o no información de farmacia, diferenciando 
##    si ha tenido información proveniente de la comunidad (corresponde al día 1 del mes)
##    o si proviene del hospital (cualquier día <> de 1).
## Validaciones:
## Parámetros:
##   csv --> Data Frame con la estructura de los 37 campos.
##   anyo --> Año de donde viene la farmacia.
## Devuelve:
##   Data Frame con la información del Id de pacientes y los dos indicadores de 
##   existencia de farmacia.
################################################################################
consultaFarmacia <- function(csv) {
  #nos quedamos con los ATCs.
  atc <- csv[csv$Ambito == 3, c("Id", "Mes_Atencion", "ATC_Com", "ATC_Hos")]
  atc$Anyo <- substr(atc$Mes_Atencion, 1, 4)
  d <- sqldf(paste("select Id, Anyo, count(ATC_Com) as CuentaAtcCom, count(ATC_Hos) as CuentaAtcHos, count(distinct Mes_Atencion) as CuentaMeses ",
             "from atc ",
             "group by Id, Anyo", sep=""))

  rm(atc)
  
  return(d)
}

################################################################################
## Función 
##   completaInformacionDemografica
## Descripción:
##    Tenemos dos ficheros de pacientes: El fichero de pacientes que nos ha enviado el hospital, al que
##    le falta cierta información demográfica para algunos pacientes; y por otro lado, el fichero de 
##    pacientes que se ha generado a partir del procesado de la información de los ficheros de datos
##    de las interacciones.
##
##    Se tratará de completar la información de Sexo y de Fecha de Nacimiento del hospital con la información
##    que tengamos de los pacientes en el juego de datos.
##
## Validaciones:
## Parámetros:
##   pacientes_unicos --> Data Frame con el listado de pacientes del hospital.
##   pacientes --> Data Frame con el listado de pacientes que se han generado a partir de los datos.
##
## Devuelve:
##   Data Frame con la información de pacientes del hospital actualizados.
################################################################################
completaInformacionDemografica <- function(pacientes_unicos, pacientes) {
  p <- pacientes_unicos[pacientes_unicos$Sexo == -1, ]
  p[p$Id %in% pacientes$Id, "Sexo_Extra"] <- p[p$Id %in% pacientes$Id, "Sexo"]
  
  c <- pacientes_unicos[pacientes_unicos$Sexo == -1, ]
  c <- c[c$Id %in% pacientes$Id, ]
  pacientes_unicos[pacientes_unicos$Id %in% c$Id, "Sexo_Extra"] <- pacientes[pacientes$Id %in% c$Id, "Sexo"]
  pacientes_unicos$Sexo <- ifelse(pacientes_unicos$Sexo == -1 & pacientes_unicos$Sexo_Extra > -1, pacientes_unicos$Sexo_Extra, pacientes_unicos$Sexo)
  
  c <- pacientes_unicos[is.na(pacientes_unicos$FechaNacimiento), ]
  c <- c[c$Id %in% pacientes$Id, ]
  pacientes_unicos[pacientes_unicos$Id %in% c$Id, "FechaNacimiento_Extra"] <- pacientes[pacientes$Id %in% c$Id, "FechaNacimiento"]
  pacientes_unicos$FechaNacimiento <- ifelse(is.na(pacientes_unicos$FechaNacimiento), pacientes_unicos$FechaNacimiento_Extra, pacientes_unicos$FechaNacimiento)
  
  pacientes_unicos <- pacientes_unicos[, -c(7:8)]
  
  rm(c)
  rm(p)
  
  return(pacientes_unicos)
}

################################################################################
## Función 
##   generaPacientes
## Descripción:
##    A partir de los datos que se han leído de un fichero, genera un conjunto de datos únicos
##    asociados al Id del Paciente. Para aquellos pacientes que estén duplicados, va a generar 
##    un fichero con sus ids, fechas de nacimiento y sexo. En aquellos casos que haya duplicados,
##    se generará el registro con el Id pero, sin información de Sexo y de Fecha de Nacimiento.
##
## Validaciones:
##     1.- Que no existen Id de pacientes duplicados. 
##     2.- Para esos IDs que estén duplicados, incorporaremos el Id y -1 como Sexo y Fecha de nacimiento.
## Parámetros:
##   datos --> Data Frame con la estructura de los tres campos de negocio para los pacientes:
##             Id, Sexo, Fecha_Nacimiento.
## Devuelve:
##   Data Frame con la información leída de los CSV y las transformaciones aplicadas.
################################################################################
generaPacientes <- function(datos) {
  #Generamos pacientes únicos para el fichero en curso. Generamos la columna numérica de fecha de 
  #nacimiento para evitar el problema de las horas en algunos registros. 
  d <- cbind(datos, rep(0,nrow(datos)))
  names(d)[4] <- "FechaNacimiento"
  d$FechaNacimiento <- as.numeric(paste(substr(d$Fecha_Nacimiento, 1, 4),
                                        substr(d$Fecha_Nacimiento, 6, 7),
                                        substr(d$Fecha_Nacimiento, 9, 10), sep=""
                                        ))
  d <- d[,c(1,3,4)]
  d <- d[with(d, order(Id)), ]
  d <- unique(d)
  
  #Generamos la variable que cuenta.
  d <- cbind(d, rep(0,nrow(d)))
  names(d)[4] <- "Cuenta"
  
  #Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
  d1 <- aggregate(Cuenta ~ Id, data = d, FUN = NROW)
  d1 <- d1[d1$Cuenta>1,]
  d1 <- d1[with(d1, order(Id)), ]
  
  #Añadimos indicador de paciente repetido
  d <- cbind(d, rep(0,nrow(d)))
  names(d)[5] <- "Repetido"
  d[d$Id %in% d1$Id,"Repetido"] <- 1
  d <- d[, -c(4)]
  
  #Para los pacientes repetidos, tenemos que quitarlos y añadirlos una única vez
  # a) Generamos una única vez esos pacientes.
  repes <- d[d$Repetido == 1, "Id"]
  repes <- as.data.frame(cbind(repes, rep(-1, length(repes)), rep(-1, length(repes))))
  names(repes) <- c("Id", "Sexo", "FechaNacimiento")
  repes <- unique(repes)
  
  # b) Eliminamos los repetidos
  d <- d[d$Repetido == 0, c(1,2,3)]
  
  # c) Añadimos los repes
  d <- rbind(d, repes)
  
  rm(d1)
  rm(repes)
  
  return (d)
}

################################################################################
## Función leePacientes
## Descripción:
##    A partir de los dos ficheros de pacientes que nos ha enviado el hospital, 
##    vamos a leer el primer fichero para analizar la información. Leeremos el segundo fichero
##    para completar el Sexo y la Fecha de Nacimiento para aquellos que hemos tenido que 
##    obtener la información de la tarjeta sanitaria.
##    Transformaciones:
##    1.- Sexo. Hay que convertir H por 1, M por 2. Cualquier otro valor lo dejaremos en -1.
##    2.- Fecha de Nacimiento. Cuando el año está entre 00 y 16 le añadimos 20 por delante. En caso contrario 199
##    3.- Aquellos pacientes que en el primer fichero no tienen información, tratamos de recuperarla del segundo fichero.
## Parámetros:
##   fichero --> Nombre del fichero con todos los pacientes que tiene que leer.
##   fichero_extra --> Nombre del fichero con todos los pacientes que se han calculado 
##                     sexo y fecha de nacimiento a partir de la tarjeta sanitaria.
##   fIdNoProcesar --> Nombre del fichero que contiene los Ids que no debe procesar.
##   fCentroProcesar --> Nombre del fichero con los centros a procesar.
## Devuelve:
##   Data Frame con la información leída de los CSV y las transformaciones aplicadas.
################################################################################
leePacientes <- function(fichero, fichero_extra, fIdNoProcesar = file.path("Datos", "original", "IdNoProcesar.csv"), fCentroProcesar = file.path("Datos", "original", "FiltroCentro.csv")) {
  
  #1.- Tratamiento del primer fichero.
  p_csv <- read.csv(fichero, sep = ";")
  names(p_csv) <- c("Id", "Centro_Ref", "Fecha_Nacimiento", "Medico_Ref", "Sexo", "Hospital_Ref")

  # - Transformación sobre el Sexo.
  p_csv$Sexo <- as.character(p_csv$Sexo)
  p_csv[p_csv$Sexo == "H", "Sexo"] <- 1  
  p_csv[p_csv$Sexo == "M", "Sexo"] <- 2
  p_csv[p_csv$Sexo != "1" & p_csv$Sexo != "2", "Sexo"] <- -1
  
  # - Transformación sobre la fecha de Nacimiento
  p_csv$Anyo <- as.character(substr(p_csv$Fecha_Nacimiento, 7, 8))
  p_csv[p_csv$Anyo >= "0" & p_csv$Anyo <= "16" & !is.na(p_csv$Anyo), "Anyo"] <- as.numeric(paste("20", p_csv[p_csv$Anyo >= "0" & p_csv$Anyo <= "16" & !is.na(p_csv$Anyo), "Anyo"], sep=""))    
  p_csv$Anyo <- as.numeric(p_csv$Anyo)
  p_csv[p_csv$Anyo > 16 & p_csv$Anyo <= 99 & !is.na(p_csv$Anyo), "Anyo"] <- as.numeric(paste("19", p_csv[p_csv$Anyo > 16 & p_csv$Anyo <= 99 & !is.na(p_csv$Anyo), "Anyo"], sep=""))

  p_csv$FechaNacimiento <- as.numeric(paste(p_csv$Anyo, 
                                            substr(p_csv$Fecha_Nacimiento, 4, 5), 
                                            substr(p_csv$Fecha_Nacimiento, 1, 2), sep=""))
  
  # - Cambiamos el orden.
  p_csv <- p_csv[, c(1, 5, 8, 2, 6, 4)]
  p_csv <- p_csv[with(p_csv, order(Id)), ]
  
  #2.- Tratamiento del segundo fichero.
  p_csv_extra <- read.csv(fichero_extra, sep = ";")
  p_csv_extra <- p_csv_extra[, c(1, 3, 2)]
  names(p_csv_extra) <- c("Id", "Sexo", "Fecha_Nacimiento")
  
  # - Transformación sobre el Sexo.
  p_csv_extra$Sexo <- as.character(p_csv_extra$Sexo)
  p_csv_extra[p_csv_extra$Sexo == "H", "Sexo"] <- 1  
  p_csv_extra[p_csv_extra$Sexo == "M", "Sexo"] <- 2
  p_csv_extra[p_csv_extra$Sexo != "1" & p_csv_extra$Sexo != "2", "Sexo"] <- -1
  
  # - Transformación sobre la fecha de Nacimiento
  p_csv_extra$Anyo <- as.character(substr(p_csv_extra$Fecha_Nacimiento, 7, 8))
  p_csv_extra[p_csv_extra$Anyo >= "0" & p_csv_extra$Anyo <= "16" & !is.na(p_csv_extra$Anyo), "Anyo"] <- as.numeric(paste("20", p_csv_extra[p_csv_extra$Anyo >= "0" & p_csv_extra$Anyo <= "16" & !is.na(p_csv_extra$Anyo), "Anyo"], sep=""))    
  p_csv_extra$Anyo <- as.numeric(p_csv_extra$Anyo)
  p_csv_extra[p_csv_extra$Anyo > 16 & p_csv_extra$Anyo <= 99 & !is.na(p_csv_extra$Anyo), "Anyo"] <- as.numeric(paste("19", p_csv_extra[p_csv_extra$Anyo > 16 & p_csv_extra$Anyo <= 99 & !is.na(p_csv_extra$Anyo), "Anyo"], sep=""))
  
  p_csv_extra$FechaNacimiento <- as.numeric(paste(p_csv_extra$Anyo, 
                                            substr(p_csv_extra$Fecha_Nacimiento, 4, 5), 
                                            substr(p_csv_extra$Fecha_Nacimiento, 1, 2), sep=""))
  
  # - Cambiamos el orden.
  p_csv_extra <- p_csv_extra[, c(1, 2, 5)]
  p_csv_extra <- p_csv_extra[with(p_csv_extra, order(Id)), ]  
  
  #3.- Recuperamos la información del segundo fichero.
  p_csv[p_csv$Id %in% p_csv_extra$Id, "Sexo_Extra"] <- p_csv_extra$Sexo
  p_csv[p_csv$Id %in% p_csv_extra$Id, "FechaNacimiento_Extra"] <- p_csv_extra$FechaNacimiento
  p_csv$Sexo <- ifelse(p_csv$Sexo == -1, p_csv$Sexo_Extra, p_csv$Sexo)
  p_csv$FechaNacimiento <- ifelse(is.na(p_csv$FechaNacimiento), p_csv$FechaNacimiento_Extra, p_csv$FechaNacimiento)
  p_csv <- p_csv[, -c(7:8)]
  
  #4.- Leemos los Ids que no hay que procesar y los centros que si se tienen que procesar para filtrarlos.
  IdsNo <- read.csv(fIdNoProcesar, sep = ";")
  Centros <- read.csv(fCentroProcesar, sep = ";")
  
  p_csv <- p_csv[!p_csv$Id %in% IdsNo$Id, ]
  p_csv <- p_csv[p_csv$Centro_Ref %in% Centros$Centro, ]
  
  rm(p_csv_extra)
  rm(IdsNo)
  
  return(p_csv)
}

################################################################################
## Función ReadCsv
## Descripción:
##    Lee un fichero CSV y normaliza los nombres de las columnas.
##    Transformaciones:
##     1.- Eliminación de las columnas inicio y fin que están repetidas.
##     2.- Eliminación de los Ids que no se deben procesar.
##     3.- Ponemos una primera columna que corresponde al año de los datos. 
##     4.- Cambiamos los nombres con "." para normalizar los accesos luego. 
##     5.- Fecha de Atención. Creamos una variable nueva donde tenemos que tener 
##         cuidado con la fecha de la farmacia que da la comunidad, que es la que 
##         mayor volumen tiene y, que viene el día 1. Esa dispensación en realidad 
##         corresponde al mes anterior. 
##     6.- Calculamos una columna para el ATC de hospitalización. 
## Parámetros:
##   fichero --> Nombre del fichero que tiene que leer.
##   fNoProcesar --> Nombre del fichero que contiene los Ids que no debe procesar.
## Devuelve:
##   Data Frame con la información leída de los CSV y las transformaciones aplicadas.
################################################################################
readCsv <- function(fichero, fIdNoProcesar = "IdNoProcesar.csv") {
  #1.- lectura de los ficheros CSVs
  csv <- read.csv(file = fichero, sep=";", fileEncoding="latin1")
  IdsNo <- read.csv(file = fIdNoProcesar, sep=";")

  #2.- eliminación de las columnas y generamos el año.
  mes <- as.numeric(substr(as.character(csv[1,1]), 1, 4))
  csv <- cbind(rep(mes, nrow(csv)), csv[-c(1,2)])
  
  #3.- Cambiamos los nombres de ciertas columnas
  n <- names(csv)
  n[1] <- "Anyo"
  n[2] <- "Fecha_Nacimiento"
  n[36] <- "Fecha_Atencion"
  n[37] <- "Id"
  names(csv) <- n
  
  #4.- Se filtran los Ids.
  csv <- csv[!(csv$Id %in% IdsNo$Id), ]
   
  #5.- Tenemos que generar una columna nueva para el año mes de atención. 
  csv <- cbind(csv, as.numeric(paste(substr(csv$Fecha_Atencion, 1, 4), substr(csv$Fecha_Atencion, 6, 7), sep="")))
  names(csv)[38] <- "Mes_Atencion"
  csv$Mes_Atencion <- ifelse(substr(csv$Fecha_Atencion, 9,10) == "01", csv$Mes_Atencion-1, csv$Mes_Atencion)
  c <- csv[substr(as.character(csv$Mes_Atencion), 5,6) == "00", c("Id", "Mes_Atencion")]
  csv[substr(csv$Mes_Atencion, 5,6) == "00","Mes_Atencion"] <- as.numeric(paste(as.numeric(substr(c$Mes_Atencion, 1,4))-1,
                                                                              12,sep=""))
  
  #6.- Generamos la columna de ATC de farmacia o de comunidad en función de la atención
  csv$ATC_Com <- ifelse(csv$Ambito==3 & substr(csv$Fecha_Atencion, 9,10) == "01", as.character(csv$ATC), NA)
  csv$ATC_Hos <- ifelse(csv$Ambito==3 & substr(csv$Fecha_Atencion, 9,10) != "01", as.character(csv$ATC), NA)
  
  #se quita la columna de fecha de atención
  #csv <- csv[,-36]
  
  #Limpiamos las variables
  rm(c)
  rm(n)
  rm(mes)
  
  return (csv)
}

############################################################################################
## Función leeCRGs
## Descripción:
##   Genera un bucle para leer todos el CRG de todos los Ids de pacientes. Se eliminan los pacientes
##   que tengan varios CRGs en un mismo año. 
## Transformaciones:
##  1.- Leemos el CSV y añadimos la columna del año.
##  2.- Eliminamos los pacientes con CRGs duplicados de la lista.
##  3.- Dejamos un fichero con los Ids que están repetidos. 
## Parámetros:
##  fMascara --> Máscara para buscar los ficheros donde están los CRGs.
## Devuelve:
##  Data frame con todos los Ids que no están repetidos.
############################################################################################
leeCRGs <- function(fMascara = "CRG*") {
  wd <- getwd()
  setwd(file.path(wd, "Datos", "original"))
  
  csv <- as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(csv) <- c("CRG", "Id", "Anyo")
  
  repetidos <- as.data.frame(matrix(ncol = 3, nrow = 0))
  colnames(csv) <- c("CRG", "Id", "Anyo")
  
  #Bucle donde para cada fichero, se harán los tres primeros puntos espcificados en la cabecera
  for (fichero in dir(pattern = fMascara) ) {
    
    #1.- Leemos el CSV y procesamos las columnas.
    c <- read.csv(fichero, sep=";")
    
    #2.1- Añadimos el año.
    anyo <- as.integer(substr(fichero, 5, 8))
    c <- cbind(c, rep(anyo, nrow(c)))
    colnames(c) <- c("CRG", "Id", "Anyo")
    c$CRG <- as.numeric(c$CRG)
    c$Id <- as.numeric(as.character(c$Id))
  
    #2.2.- Eliminamos los pacientes duplicados.
    c <- cbind(c, rep(0,nrow(c)))
    names(c)[4] <- "Cuenta"
    
    #Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
    d <- aggregate(Cuenta ~ Id, data = c, FUN = NROW)
    d1 <- d[d$Cuenta>1,]
    d <- d[d$Cuenta==1, ]
    
    #2.3.- Concatenamos los buenos y los repetidos
    csv <- rbind(csv, c[c$Id %in% d$Id, -c(4)])
    repetidos <- rbind(repetidos, c[c$Id %in% d1$Id, -c(4)])
  }
  rm(c)
  rm(d)
  rm(d1)
  rm(anyo)
  
  #3.- Grabamos los repetidos.
  write.csv(repetidos, file = "repetidos_crg.csv", col.names=TRUE, row.names = FALSE, quote=FALSE)
  
  setwd(wd)
  rm(wd)
  
  return(csv)
}

############################################################################################
## Función concatenaCRGs
## Descripción:
##   Concatena para cada paciente el CRG de los años que vengan en la lista. Importante, esta
##   función no añade pacientes a la lista de pacientes únicos, ni tampoco los elimina.
##   Si un CRG no está asociado a un paciente, añadimos la columna con un CRG -1.
## Transformaciones:
## Parámetros:
##   pacientes_unicos --> Listado de pacientes finales.
##   crg --> Listado de pacientes y el CRG asociado. 
## Devuelve:
##   Data frame con todos los pacientes que se reciben, concatenando en columnas los CRGs por año.
############################################################################################
concatenaCRGs <- function(pacientes_unicos, crg) {

  #Bucle donde para cada fichero, se harán los tres primeros puntos espcificados en la cabecera
  for (anyo in unique(crg$Anyo) ) {
    #Obtenemos la relación Id - CRG.
    a <- crg[crg$Anyo == anyo, ]
    a <- a[with(a, order(Id)), ]
    
    #Concatenamos
    crg_anyo <- paste("CRG_", anyo, sep="")
    pacientes_unicos[pacientes_unicos$Id %in% a$Id, crg_anyo] <- a[a$Id %in% pacientes_unicos$Id, "CRG"]
  }
  rm(a)
  rm(anyo)
  rm(crg_anyo)

  return(pacientes_unicos)
}



###################################
#### AUXILIARES
###################################

##################################################################
#### Función --> creaAnyos
#### Tipo: Interna
# Calcula el número de años en función de las columnas con tipo
# CRG_xxxx que tiene el juego de datos. 
##################################################################
creaAnyos <- function(columnas) {
  anyos <- c()
  for (columna in columnas) {
    if (substr(columna, 1, 4) == "CRG_") {
      anyo <- substr(columna, 5, 8)
      anyos <- rbind(anyos, anyo)
    }
  }
  
  return (anyos)
}

