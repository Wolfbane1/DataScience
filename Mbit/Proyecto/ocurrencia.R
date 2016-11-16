
source("ac4.R")

c <- subset(csv, csv$Anyo==2011 & csv$CRG %in% c(5424))
res <- as.data.frame(cbind(c$Id, c$CRG, rep(0, nrow(c)), rep(0, nrow(c))))
colnames(res) <- c("Id", "CRG", "OA", "OC")
res$OA <- ifelse(c[,"A10BA"] > 0, 1, 0)
res$OC <- ifelse(c[,"C10AA"] > 0, 1, 0)
sum(res$OA)
sum(res$OC)

c <- subset(csv, csv$Anyo==2011 & csv$CRG %in% c(6144))
res <- as.data.frame(cbind(c$Id, c$CRG, rep(0, nrow(c)), rep(0, nrow(c))))
colnames(res) <- c("Id", "CRG", "OA", "OC")
res$OA <- ifelse(c[,"A10BA"] > 0, 1, 0)
res$OC <- ifelse(c[,"C10AA"] > 0, 1, 0)
sum(res$OA)
sum(res$OC)

c <- subset(csv, csv$Anyo==2011 & csv$CRG %in% c(6144))
res <- as.data.frame(cbind(c$Id, c$CRG, rep(0, nrow(c)), rep(0, nrow(c))))
colnames(res) <- c("Id", "CRG", "OA", "OC")
res$OA <- ifelse(c[,"A10BA"] > 0, 1, 0)
res$OC <- ifelse(c[,"C10AA"] > 0, 1, 0)
sum(res$OA)
sum(res$OC)

1309/2054
2371/2939


sinA10 <- subset(csv, csv$totalATCDiabeticos == 0, c("Id", "Anyo", "CRG"))
writeMat("Pacientes_Sin_A10.mat", A=sinA10)


####################################################

matriz_e_5424 <- generaMatrizEvolucion(5424, 1000, 100)
  media_e_m5424 <- apply(matriz_e_5424, 2, mean)

matriz_e_6144 <- generaMatrizEvolucion(6144, 1000, 100)
  media_e_m6144 <- apply(matriz_e_6144, 2, mean)

  pintaDiferenciaSexo(media_e_m5424, media_e_m6144, 5424, 2011, "perfilEvolucion5424y6144.png")
  
matriz_eh_5424 <- generaMatrizEvolucionSexo(5424, 1000, 100, 1)
matriz_em_5424 <- generaMatrizEvolucionSexo(5424, 1000, 100, 2)
  media_eh_m5424 <- apply(matriz_eh_5424, 2, mean)
  media_em_m5424 <- apply(matriz_em_5424, 2, mean)
  
matriz_eh_6144 <- generaMatrizEvolucionSexo(6144, 1000, 100, 1)
matriz_em_6144 <- generaMatrizEvolucionSexo(6144, 1000, 100, 2)
  media_eh_m6144 <- apply(matriz_eh_6144, 2, mean)
  media_em_m6144 <- apply(matriz_em_6144, 2, mean)

pintaDiferenciaSexo(media_eh_m5424, media_eh_m6144, 5424, 2011, "perfilEvolucionHombre5424y6144.png")
pintaDiferenciaSexo(media_em_m5424, media_em_m6144, 5424, 2011, "perfilEvolucionMujer5424y6144.png")

################################
################################

