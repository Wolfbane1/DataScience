



#2011
d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "FechaNacimiento"
d$FechaNacimiento <- as.numeric(paste(substr(d$Fecha_Nacimiento, 1, 4),
                                      substr(d$Fecha_Nacimiento, 6, 7),
                                      substr(d$Fecha_Nacimiento, 9, 10), sep=""
))
d <- d[,c(1,3,4)]
d <- unique(d)

#Generamos la variable que cuenta.
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- aggregate(Cuenta ~ Id, data = d, FUN = NROW)

pacientes <- d1
names(pacientes)[2] <- "Anyo_2011"
pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
pacientes <- cbind(pacientes, rep(0, nrow(pacientes)))
names(pacientes)[3:4] <- c("Anyo_2012", "Anyo_2013")

#2012
csv <- readCsv("datos_entrada_2012.csv")

d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "FechaNacimiento"
d$FechaNacimiento <- as.numeric(paste(substr(d$Fecha_Nacimiento, 1, 4),
                                      substr(d$Fecha_Nacimiento, 6, 7),
                                      substr(d$Fecha_Nacimiento, 9, 10), sep=""
))
d <- d[,c(1,3,4)]
d <- unique(d)

#Generamos la variable que cuenta.
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- aggregate(Cuenta ~ Id, data = d, FUN = NROW)

Id_2011_SI_2012_SI <- d1[d1$Id %in% pacientes$Id, "Id"]
Id_2011_NO_2012_SI <- d1[! (d1$Id %in% pacientes$Id), "Id"]

d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[3:5] <- c("Anyo_2011", "Anyo_2012", "Anyo_2013")

d1$Anyo_2012 <- d1$Cuenta
d1 <- d1[, -c(2)]

pacientes[pacientes$Id %in% d1$Id,"Anyo_2012"] <- d1[d1$Id %in% pacientes$Id, "Anyo_2012"]
d2 <- d1[!(d1$Id %in% pacientes$Id),]
pacientes <- rbind(pacientes, d2)
pacientes <- pacientes[with(pacientes, order(Id)), ]

#2013
csv <- readCsv("datos_entrada_2013.csv")

d <- csv[,c("Id", "Fecha_Nacimiento", "Sexo")]
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "FechaNacimiento"
d$FechaNacimiento <- as.numeric(paste(substr(d$Fecha_Nacimiento, 1, 4),
                                      substr(d$Fecha_Nacimiento, 6, 7),
                                      substr(d$Fecha_Nacimiento, 9, 10), sep=""
))
d <- d[,c(1,3,4)]
d <- unique(d)

#Generamos la variable que cuenta.
d <- cbind(d, rep(0,nrow(d)))
names(d)[4] <- "Cuenta"

#Agregamos para identificar a los pacientes duplicados y nos quedamos con los que están duplicados
d1 <- aggregate(Cuenta ~ Id, data = d, FUN = NROW)

Id_2011_SI_2012_SI <- d1[d1$Id %in% pacientes$Id, "Id"]
Id_2011_NO_2012_SI <- d1[! (d1$Id %in% pacientes$Id), "Id"]

d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
d1 <- cbind(d1, rep(0, nrow(d1)))
names(d1)[3:5] <- c("Anyo_2011", "Anyo_2012", "Anyo_2013")

d1$Anyo_2013 <- d1$Cuenta
d1 <- d1[, -c(2)]

pacientes[pacientes$Id %in% d1$Id,"Anyo_2013"] <- d1[d1$Id %in% pacientes$Id, "Anyo_2013"]
d2 <- d1[!(d1$Id %in% pacientes$Id),]
pacientes <- rbind(pacientes, d2)
pacientes <- pacientes[with(pacientes, order(Id)), ]

rm(d)
rm(d1)
rm(d2)
rm(Id_2011_NO_2012_SI)
rm(Id_2011_SI_2012_SI)

write.table(pacientes, file="ids_repetidos.csv", quote=FALSE, sep=";", col.names = TRUE, row.names = FALSE)
