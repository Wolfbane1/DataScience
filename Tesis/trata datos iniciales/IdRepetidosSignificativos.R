library(sqldf)

#2011
csv <- readCsv("datos_entrada_2011.csv")
d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "AnyoNacimiento"
d$AnyoNacimiento <- as.numeric(substr(d$Fecha_Nacimiento, 1, 4))
d <- d[,c(1,3,4)]
d <- unique(d)

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- sqldf("select Id, Sexo, min(AnyoNacimiento) as min_Anyo, max(AnyoNacimiento) as max_Anyo from d group by Id, Sexo")
d1$dif_Anyo <- d1$max_Anyo - d1$min_Anyo

#Generamos la variable que cuenta.
d1 <- cbind(d1, rep(0,nrow(d1)))
names(d1)[6] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d2 <- aggregate(Cuenta ~ Id, data = d1, FUN = NROW)
d2 <- d2[d2$Cuenta > 1, ]

pacientes <- d1[!(d1$Id %in% d2$Id), c("Id", "dif_Anyo") ]
pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
names(pacientes)[3] <- c("Ambos_Sexos")

d1 <- d1[d1$Id %in% d2$Id & d1$Sexo == 1, c("Id", "dif_Anyo")]
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[3] <- c("Ambos_Sexos")
d1$Ambos_Sexos <- 1

pacientes <- rbind(pacientes, d1)
names(pacientes) <- c("Id", "dif_Anyo_2011", "Ambos_Sexos_2011")

#2012
csv <- readCsv("datos_entrada_2012.csv")

d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "AnyoNacimiento"
d$AnyoNacimiento <- as.numeric(substr(d$Fecha_Nacimiento, 1, 4))
d <- d[,c(1,3,4)]
d <- unique(d)

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- sqldf("select Id, Sexo, min(AnyoNacimiento) as min_Anyo, max(AnyoNacimiento) as max_Anyo from d group by Id, Sexo")
d1$dif_Anyo <- d1$max_Anyo - d1$min_Anyo

#Generamos la variable que cuenta.
d1 <- cbind(d1, rep(0,nrow(d1)))
names(d1)[6] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d2 <- aggregate(Cuenta ~ Id, data = d1, FUN = NROW)
d2 <- d2[d2$Cuenta > 1, ]

p <- d1[!(d1$Id %in% d2$Id), c("Id", "dif_Anyo") ]
p <- cbind(p, rep(0, nrow(p)))
names(p)[2:3] <- c("dif_Anyo_2012", "Ambos_Sexos_2012")

d1 <- d1[d1$Id %in% d2$Id & d1$Sexo == 1, c("Id", "dif_Anyo")]
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[2:3] <- c("dif_Anyo_2012", "Ambos_Sexos_2012")
d1$Ambos_Sexos_2012 <- 1

p <- rbind(p, d1)


Id_2011_SI_2012_SI <- p[p$Id %in% pacientes$Id, "Id"]
Id_2011_NO_2012_SI <- p[! (p$Id %in% pacientes$Id), "Id"]

pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
names(pacientes)[4:5] <- c("dif_Anyo_2012", "Ambos_Sexos_2012")
pacientes[pacientes$Id %in% Id_2011_SI_2012_SI, "dif_Anyo_2012"] <- p[p$Id %in% Id_2011_SI_2012_SI, "dif_Anyo_2012"]
pacientes[pacientes$Id %in% Id_2011_SI_2012_SI, "Ambos_Sexos_2012"] <- p[p$Id %in% Id_2011_SI_2012_SI, "Ambos_Sexos_2012"]

d1 <- p[p$Id %in% Id_2011_NO_2012_SI, ]
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[4:5] <- c("dif_Anyo_2011", "Ambos_Sexos_2011")
d1$dif_Anyo_2011 <- NA
d1$Ambos_Sexos_2011 <- NA
d2 <- as.data.frame(cbind(d1$Id, d1$dif_Anyo_2011, d1$Ambos_Sexos_2011, d1$dif_Anyo_2012, d1$Ambos_Sexos_2012))
names(d2) <- c("Id", "dif_Anyo_2011", "Ambos_Sexos_2011", "dif_Anyo_2012", "Ambos_Sexos_2012")
pacientes <- rbind(pacientes, d2)
pacientes <- pacientes[with(pacientes, order(Id)), ]

#2013
csv <- readCsv("datos_entrada_2013.csv")

d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "AnyoNacimiento"
d$AnyoNacimiento <- as.numeric(substr(d$Fecha_Nacimiento, 1, 4))
d <- d[,c(1,3,4)]
d <- unique(d)

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- sqldf("select Id, Sexo, min(AnyoNacimiento) as min_Anyo, max(AnyoNacimiento) as max_Anyo from d group by Id, Sexo")
d1$dif_Anyo <- d1$max_Anyo - d1$min_Anyo

#Generamos la variable que cuenta.
d1 <- cbind(d1, rep(0,nrow(d1)))
names(d1)[6] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d2 <- aggregate(Cuenta ~ Id, data = d1, FUN = NROW)
d2 <- d2[d2$Cuenta > 1, ]

p <- d1[!(d1$Id %in% d2$Id), c("Id", "dif_Anyo") ]
p <- cbind(p, rep(0, nrow(p)))
names(p)[2:3] <- c("dif_Anyo_2013", "Ambos_Sexos_2013")

d1 <- d1[d1$Id %in% d2$Id & d1$Sexo == 1, c("Id", "dif_Anyo")]
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[2:3] <- c("dif_Anyo_2013", "Ambos_Sexos_2013")
d1$Ambos_Sexos_2013 <- 1

p <- rbind(p, d1)


Id_2011_SI_2012_SI <- p[p$Id %in% pacientes$Id, "Id"]
Id_2011_NO_2012_SI <- p[! (p$Id %in% pacientes$Id), "Id"]

pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
names(pacientes)[6:7] <- c("dif_Anyo_2013", "Ambos_Sexos_2013")
pacientes[pacientes$Id %in% Id_2011_SI_2012_SI, "dif_Anyo_2013"] <- p[p$Id %in% Id_2011_SI_2012_SI, "dif_Anyo_2013"]
pacientes[pacientes$Id %in% Id_2011_SI_2012_SI, "Ambos_Sexos_2013"] <- p[p$Id %in% Id_2011_SI_2012_SI, "Ambos_Sexos_2013"]

d1 <- p[p$Id %in% Id_2011_NO_2012_SI, ]
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[4:7] <- c("dif_Anyo_2011", "Ambos_Sexos_2011", "dif_Anyo_2012", "Ambos_Sexos_2012")
d1$dif_Anyo_2011 <- NA
d1$Ambos_Sexos_2011 <- NA
d1$dif_Anyo_2012 <- NA
d1$Ambos_Sexos_2012 <- NA

d2 <- as.data.frame(cbind(d1$Id, d1$dif_Anyo_2011, d1$Ambos_Sexos_2011, d1$dif_Anyo_2012, d1$Ambos_Sexos_2012, d1$dif_Anyo_2013, d1$Ambos_Sexos_2013))
names(d2) <- c("Id", "dif_Anyo_2011", "Ambos_Sexos_2011", "dif_Anyo_2012", "Ambos_Sexos_2012", "dif_Anyo_2013", "Ambos_Sexos_2013")
pacientes <- rbind(pacientes, d2)
pacientes <- pacientes[with(pacientes, order(Id)), ]

write.table(pacientes, file="ids_dif_anyos.csv", quote=FALSE, sep=";", col.names = TRUE, row.names = FALSE)

rm(d)
rm(d1)
rm(d2)
rm(p)
rm(Id_2011_NO_2012_SI)
rm(Id_2011_SI_2012_SI)
