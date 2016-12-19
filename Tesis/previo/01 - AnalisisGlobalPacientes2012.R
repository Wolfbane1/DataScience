suppressWarnings(library(ggplot2))

##################################################################
##################################################################
############### INTERNAS
##################################################################
##################################################################

##################################################################
#### Función --> posicionCRGAnyo
#### Tipo: Interna
# Calcula la posición en el vector de un CRG dado.
##################################################################
posicionCRGAnyo <- function(columnas, crg_anyo) {
  i <- 1
  
  for (pos in 1:length(columnas)) {
    if (columnas[pos] == crg_anyo) {
      i <- pos
    }
  }
  
  rm(pos)
  
  return (i)
}

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


##################################################################
##################################################################
############### INVOCADAS DESDE KNITR
##################################################################
##################################################################

##################################################################
#### Función --> pintaTotalPacientesCRG
#### Tipo: Llamada desde KnitR
#### Número: G.1
# Se pinta el total de pacientes que están clasificados por CRG. 
##################################################################
pintaTotalPacientesCRG <- function(pacientes) { 
  columnas <- colnames(pacientes)
  anyos <- creaAnyos(columnas)
  g <- c()
  g <- rbind(g, c("1 - Todos", nrow(pacientes)))
  i <- 2
  anyos <- sort(anyos, decreasing = TRUE)
  
  for (anyo in anyos) {
    crg_anyo <- paste("CRG_", anyo, sep="") 
    p <- pacientes[, c("Id", crg_anyo)]
    colnames(p) <- c("Id", "CRG")
    g <- rbind(g, c(paste(i, " - Año ", anyo, sep=""),  nrow( p[!is.na(p$CRG),]) ) ) 
    
    i <- i + 1
  }
  
  g <- as.data.frame(g)
  colnames(g) <- c("Pacientes", "Numero")
  gp <- ggplot(g, aes(x=Pacientes, y=Numero)) + geom_bar(stat="identity") +
    labs(x="Pacientes", y="Num.Pacientes", 
         title="Pacientes clasificados por año") +
    theme_minimal() 
  print(gp)
  
  rm(gp)
  rm(columnas)
  rm(anyos)
  rm(anyo)
  rm(crg_anyo)
  rm(p)
  rm(i)
  rm(g)
}

##################################################################
#### Función --> pintaCaidaPacientesCRG
#### Tipo: Llamada desde KnitR
#### Número: G.1
# Se pinta el total de pacientes que están clasificados por CRG 
# en 2012, y cómo caen los pacientes conforme vamos años atrás. 
##################################################################
pintaCaidaPacientesCRG <- function(pacientes) { 
  columnas <- colnames(pacientes)
  anyos <- creaAnyos(columnas)
  g <- c()
  g <- rbind(g, c("1 - Todos", nrow(pacientes)))
  i <- 2
  anyos <- sort(anyos, decreasing = TRUE)
  p <- pacientes
  
  for (anyo in anyos) {
    crg_anyo <- paste("CRG_", anyo, sep="") 
    colnames(p) <- columnas
    colnames(p)[ posicionCRGAnyo(columnas, crg_anyo) ] <- "CRG"
    p <- p[!is.na(p$CRG), ]
    g <- rbind(g, c(paste(i, " - Año ", anyo, sep=""),  nrow(p) ) ) 
    
    i <- i + 1
  }
  
  g <- as.data.frame(g)
  colnames(g) <- c("Pacientes", "Numero")
  gp <- ggplot(g, aes(x=Pacientes, y=Numero)) + geom_bar(stat="identity") +
    labs(x="Pacientes", y="Num.Pacientes", 
         title="Pacientes clasificados por año") +
    theme_minimal() 
  print(gp)
  
  rm(gp)
  rm(columnas)
  rm(anyos)
  rm(anyo)
  rm(crg_anyo)
  rm(p)
  rm(i)
  rm(g)
}

