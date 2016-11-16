
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
  #print(AcsvTCs_vacios)
  return( csv[csv$Anyo == anyo, !colnames(csv) %in% ATCs_vacios] )
}

## Función: calculaPCA_nivel2
#  A partir de un DataFrame con la información, de los componentes ATCs, en un rango, 
#  podemos calcular la matriz de componentes ATCs. La matriz nos debe llegar sin 
#  estandarizar.
#  Parámetros
#     --> d: Data Frame
#     --> vectorPosicion: Vector con la posición de inicio:fin donde está la matriz a tener en cuenta.
#
calculaPCA_nivel2 <- function (d, vectorPosicion) {
  m <- reduceMatrizATC(d, vectorPosicion, 2, "CUENTA_FAMILIAS")
  
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

## Función: calculaPCA_nivel1
#  A partir de un DataFrame con la información, de los componentes ATCs, en un rango, 
#  podemos calcular la matriz de componentes ATCs. La matriz nos debe llegar sin 
#  estandarizar.
#  Parámetros
#     --> d: Data Frame
#     --> vectorPosicion: Vector con la posición de inicio:fin donde está la matriz a tener en cuenta.
#
calculaPCA_nivel1 <- function (d, vectorPosicion) {
  #Paso 1: Generación de la matriz ATC reducida
  m <- reduceMatrizATC(d, vectorPosicion, 1, "CUENTA_FAMILIAS")
  
  # Paso 2: Ejecución de los principales componentes. Se escala (normaliza)
  pca <- prcomp(m, center=TRUE, scale=TRUE)
  
  # Paso 3: Revisión de la agrupación de los PC
  round(unclass(pca$rotation)[, 1:6], 2)
  
  # Paso 4: Revisión de las varianzas de los PC.
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
#   --> d: Datos a tratar, será el juego de datos completo filtrado por años. 
#   --> vectorPosicion: Vector con las posiciones de las columnas de la matriz de ATC en el conjunto de datos.
#   --> nivel: nivel de ATC al que queremos reducir. Puede tomar dos valores:
#           a) 2: A01, V03, etc
#           b) 1: A, B
#   --> funcion: Función para reducir, puede ser para contar existencias, contar familias distintas, sumar número de dispensacione totales.
#          valor: EXISTENCIA
#          valor: CUENTA_FAMILIAS
#          valor: SUMA_FAMILIAS
reduceMatrizATC <- function(d, vectorPosicion, nivel, funcion) {
  #Calcula la posición final y la expresión regular a usar en función del nivel
  posFinal <- ifelse(nivel == 1, 1, 3)
  expReg <- ifelse(nivel == 1, "[0-9]{2}[A-Z]{2}", "[A-Z]{2}")
  
  #Paso 1: Generación de la matriz necesaria
  atc <- unique(substr(colnames(d)[vectorPosicion], 1, 1))
  m <- matrix(nrow=nrow(d), ncol=length(atc))
  colnames(m) <- atc
  
  #Paso 2: Aplicamos la función sobre cada elemento de la matriz. 
  #Iteramos sobre todos los ATCs.
  for (i in 1:length(atc)) {
    #Esto devuelve una matriz de elementos en función de los ATCs.
    a <- d[, grep(paste("^", atc[i], expReg, sep=""), colnames(d))]
    
    #Si tenemos más de 1 dimensión
    if ( is.vector(a) ) {
      #   m[, i] <- a
      m[m[,i] > 0, i] <- 1
    } else { 
      if (funcion == "EXISTENCIA") {
        m[, i] <- apply(a, 1, obtieneExistencia)  
      }
      else if (funcion == "CUENTA_FAMILIAS") {
        m[, i] <- apply(a, 1, cuentaFamilias)  
      }
      else if (funcion == "SUMA_FAMILIAS") {
        m[, i] <- apply(a, 1, sumaFamilias)  
      }
    }
  }
  rm(i)
  rm(a)
  rm(posFinal)
  
  return(m)
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

## Función: sumaFamilias
#  Calcula el número total de medicamentos dispensados que ha tomado un paciente.
#
sumaFamilias <- function(x) {
  c <- 0
  
  for (i in 1:length(x) ) {
    if ( x[i] > 0 ) {
      c <- c + x[i]
    }
  }
  return(c)
}

calculaBurbujaATC <- function(datos, matriz) {
  d <- as.data.frame(cbind(datos$Genero, datos$Edad, datos$Anyo, datos$CRG, matriz))
  colnames(d)[1:4] <- c("Sexo", "Edad", "Anyo", "CRG_base")
  m <- matrix(nrow = 0,  ncol=3)
  
  #Calculamos el total por CRG
#  if (tipo == "EXISTENCIA") {
#    aux <- as.data.frame(cbind(d$CRG_base, apply(m_existeFamilia,1,sum)))
#    total_crg <- sqldf("select V1 as CRG_base, sum(V2) as Total_CRG from aux group by V1")
#  }
#  else {
    total_crg <- sqldf("SELECT CRG_base, sum(A) + sum(B) + sum(C) + sum(D) + sum(G) + sum(H) + sum(J) + sum(L) + sum(M) + sum(N) + sum(P) + sum(R) + sum(S) + sum(V) as Total_CRG from d group by CRG_base") 
#  }
  
  #Convertimos la matriz en una tabla para el gráfico. Se calcula la suma por CRG del total de elementos para un ATC. 
  for (i in colnames(matriz)) {
    df <- sqldf(paste("select CRG_base, '", i, "' as ATC, sum(", i, ") as Total from d group by CRG_base", sep=""))
    df$Total <- floor(100*df$Total/total_crg$Total_CRG)
    m <- rbind(m, df)
  }
  rm(df)
  rm(d)
  rm(total_crg)
  
  m$CRG_base <- factor(m$CRG_base)
  m$ATC <- factor(m$ATC)
  return(m)
}

convierteExistencia <- function(x) {
  return (ifelse(x>0, 1, 0))
}
