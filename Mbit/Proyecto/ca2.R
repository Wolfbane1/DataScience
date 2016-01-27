data("smoke")
smoke
p <- ca(smoke)
p


#1.- Obtener la tabla de contingencia.
tc <- m

#2.- Hacer el chequeo de Independencia: Chi^2. 
total <- sum(tc)
total.fila    <- apply(m, 1, sum)
total.columna <- apply(m, 2, sum) 

#3.- Matriz de correspondencia --> Tabla de contingencia entre el total.
mc <- tc / total
mc.fila    <- apply(mc, 1, sum)
mc.columna <- apply(mc, 2, sum) 

#Test de Independencia de las variables
chisq.test(mc)





rm(tc)
rm(total)
rm(total.fila)
rm(total.columna)
rm(mc)
rm(mc.fila)
rm(mc.columna)