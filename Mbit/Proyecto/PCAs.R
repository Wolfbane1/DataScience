
## Función: standMatrix(m) 
#  Función que escala una matrix.
#
#
standMatrix <- function (m) {
  apply(m, 2, standVector)
}

standVector <- function(v) {
  (v-mean(v))/sd(v)
}

## ATCs que están vacíos
#  Se identifican para quitarlos del experimento para una primera reducción.
#
#
eliminaATCsVacios <- function(csv, anyo, rango) {
  #Iteramos por cada columna para detectar las que la media es 0, es decir, no tiene 
  #ninguna dispensación.
  ATCs_vacios <- c()
  for (i in rango) {
    #print(paste("Mínimo: ", min(csv[,i]), "Máximo: ", max(csv[,i])))
    if ( min(csv[csv$Anyo == anyo, i]) == 0 & max(csv[csv$Anyo == anyo, i]) == 0) {
      ATCs_vacios <- rbind(ATCs_vacios, colnames(csv)[i])
    }
  } 
  
  return( csv[csv$Anyo == anyo, !colnames(csv) %in% ATCs_vacios] )
}

## Función: calculaPCA
#  A partir de un DataFrame con la información, de los componentes ATCs, en un rango, 
#  podemos calcular la matriz de componentes ATCs. La matriz nos debe llegar sin 
#  estandarizar.
#  Parámetros
#     --> d: Data Frame
#     --> vectorPosicion: Vector con la posición de inicio:fin donde está la matriz a tener en cuenta.
#
calculaPCA_nivel <- function (d, vectorPosicion) {
  #Paso 1: Generación de la matriz necesaria
  ATC.nivel2 <- unique(substr(colnames(d)[vectorPosicion], 1, 3))
  m <- matrix(nrow=nrow(d), ncol=length(ATC.nivel2))
  colnames(m) <- ATC.nivel2
  rm(ATC.nivel2)
  
  #Paso 2: Generación de la matriz ATC reducida
  m <- reduceMatrizATC(d, m, 2)
  
  
  # Paso 1: Ejecución de los principales componentes. Se escala (normaliza)
  pca <- prcomp(m, center=TRUE, scale=TRUE)
  
  # Paso 2: Revisión de la agrupación de los PC
  round(unclass(pca$rotation)[, 1:6], 2)
  
  # Paso 3: Revisión de las varianzas de los PC.
  # a) Eigenvalues
  eig <- (pca$sdev)^2
  
  # b) Varianza en %
  variance <- eig*100/sum(eig)
  
  # c) Varianza acumulativa.
  cumvar <- cumsum(variance)
  
  eig.active <- data.frame(eig = eig, variance = variance,
                           cumvariance = cumvar)
  head(eig.active)
  
  summary(pca)
  
  # d) Visualizamos gráficamente
  #   % de Varianza.
  barplot(eig.active[, 2], names.arg=1:nrow(eig.active), 
          main = "Varianzas",
          xlab = "Componentes Principales",
          ylab = "Porcentaje de Varianza",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(eig.active), 
        eig.active[, 2], 
        type="b", pch=10, col = "red")
  
  #   Eugenvalue.
  barplot(eig.active[, 1], names.arg=1:nrow(eig.active), 
          main = "Varianzas",
          xlab = "Componentes Principales",
          ylab = "Eigenvalues",
          col ="steelblue")
  # Add connected line segments to the plot
  lines(x = 1:nrow(eig.active), 
        eig.active[, 1], 
        type="b", pch=10, col = "red")
  abline(h=1,lty=6)
}

## Función: reduceMatriz
#  Se hace una redución de la matriz según el nivel.
#   --> m: Matriz con todos los elementos a nulos.
#   --> nivel: nivel de ATC al que queremos reducir. Puede tomar dos valores:
#           a) 2: A01, V03, etc
#           b) 1: A, B
#
reduceMatrizATC <- function(d, m, nivel) {
  atc <- colnames(m)
  
  #Iteramos sobre todos los ATCs.
  for (i in 1:atc) {
    #Esto devuelve una matriz de elementos en función de los ATCs.
    a <- d[, grep(paste(atc[i], "[A-Z]", sep=""), colnames(d))]
    
    #Si tenemos más de 1 dimensión
    if ( ! is.vector(a) ) {
      #m[, i] <- apply(a, 1, obtieneExistencia)
      m[, i] <- apply(a, 1, cuentaFamilias)
    }
    else {
      m[, i] <- a
      m[m[,i] > 0, i] <- 1
    }
  }
  rm(i)
  rm(a)
  
}

## Función: obtieneExistencia
#  Calcula si para una familia de ATCs, existen dispensaciones de productos.
#
obtieneExistencia <- function(x) {
  c <- 0

  for (i in 1:length(x) ) {
    if ( x[i] > 0 && c == 0) {
      c <- 1
    }
  }
  return(c)
}

## Función: cuentaFamilias
#  Calcula el número de medicamentos distintos que ha tomado un paciente.
#
cuentaFamilias <- function(x) {
  c <- 0
  
  for (i in 1:length(x) ) {
    if ( x[i] > 0 ) {
      c <- c + 1
    }
  }
  return(c)
}




