d <- as.data.frame(unique(diagnosticos$CODIGO))
di <- as.data.frame(unique(interacciones[interacciones$Tipo == "D", "Codigo"]))
resto <- as.data.frame(di[!di$`unique(interacciones[interacciones$Tipo == "D", "Codigo"]` %in% d$`unique(diagnosticos$CODIGO)`, ])
names(resto) <- c("Diag")
rm(d)
rm(di)
resto_2010 <- resto
names(resto_2010) <- c("Diag")
d <- as.data.frame(unique(diagnosticos$CODIGO))
di <- as.data.frame(unique(interacciones[interacciones$Tipo == "D", "Codigo"]))
resto <- as.data.frame(di[!di$`unique(interacciones[interacciones$Tipo == "D", "Codigo"]` %in% d$`unique(diagnosticos$CODIGO)`, ])
rm(d)
rm(di)
resto_2011 <- as.data.frame(resto[! resto$Diag %in% resto_2010$Diag,])
names(resto_2011) <- c("Diag")
resto_2011 <- rbind(resto_2011, resto_2010)
d <- as.data.frame(unique(diagnosticos$CODIGO))
di <- as.data.frame(unique(interacciones[interacciones$Tipo == "D", "Codigo"]))
resto <- as.data.frame(di[!di$`unique(interacciones[interacciones$Tipo == "D", "Codigo"]` %in% d$`unique(diagnosticos$CODIGO)`, ])
names(resto) <- c("Diag")
rm(d)
rm(di)
resto_2012 <- as.data.frame(resto[! resto$Diag %in% resto_2011$Diag,])


d <- as.data.frame(unique(diagnosticos$CODIGO))
di <- as.data.frame(unique(interacciones[interacciones$Tipo == "D", "Codigo"]))
resto <- as.data.frame(di[!di$`unique(interacciones[interacciones$Tipo == "D", "Codigo"]` %in% d$`unique(diagnosticos$CODIGO)`, ])
names(resto) <- c("Diag")
rm(d)
rm(di)

i <- interacciones[interacciones$Codigo %in% resto$Diag, ]


df <- sqldf("SELECT Anyo, Codigo, count(*) as NumInteracciones from i group by Anyo, Codigo having count(*) > 1")

f <- file.path("Datos", "salida", "Diagnosticos_No_Validos.csv")
write.table(df, file=f, row.names = FALSE, col.names = TRUE, quote=FALSE, sep=";")

i <- interacciones[interacciones$Codigo %in% diagnosticos$CODIGO, ]


######## PROCEDIMIENTOS

i <- interacciones[interacciones$Tipo == "P", "Codigo"]
i <- as.data.frame(i)
i <- as.data.frame(unique(i$i))
names(i) <- c("Codigo")
x <- as.data.frame(i[!i$Codigo %in% procedimientos$CODIGO, "Codigo"])
View(x)

proc <- procedimientos[,c(3,4,5,6,5,6,7,8)]
names(proc) <- names(procedimientos)
proc <- unique(proc)

proc2 <- proc[,c(3,4,3,4,5,6,7,8)]
names(proc2) <- names(procedimientos)
proc2 <- unique(proc2)

procedimientos <- rbind(procedimientos,proc, proc2)
procedimientos <- procedimientos[with(procedimientos, order(CODIGO)), ]
