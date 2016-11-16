
###Distribución de pacientes.
#Diabéticos vs Sanos por Año
ndiab <- calculaPacientesCoincidencias(csv)
nsanos <- calculaPacientesCoincidencias(sanos)
cuentasDiab <- suppressMessages(sqldf("select Anyo, count(*) as total from csv group by Anyo"))
cuentasSanos <- suppressMessages(sqldf("select Anyo, count(*) as total from sanos group by Anyo"))
AnyoDiab <- factor(csv$Anyo)
AnyoSanos <- factor(sanos$Anyo)

max <- max(cuentasDiab[1,2], cuentasDiab[2,2], cuentasSanos[1,2], cuentasSanos[1,2]) + 300

par(mfrow=c(1,2), mar=c(4,4,3,3))
tipoPaciente ="Diabéticos"
plot(AnyoDiab, xlab="Año", ylab="Número de pacientes", ylim=c(0, max+500), col=colorAnyo)
title(main=paste("", tipoPaciente), cex.main=0.7)
text(1, 1000, labels=c(cuentasDiab[1,2]), adj=0.5, font=4, cex=0.9)
text(2, 1000, labels=c(cuentasDiab[2,2]), adj=0.5, font=4, cex=0.9)
abline(h=ndiab, lwd=3, col="red")
text(1, ndiab+0.05*max, paste(ndiab, " pacientes coincidentes."), col="red", cex = 0.9)
#Sanos
tipoPaciente ="Sanos"
plot(AnyoSanos, xlab="Año", ylab="Número de pacientes", ylim=c(0, max+300), col=colorAnyo)
title(main=paste("", tipoPaciente), cex.main=0.7)
text(1, 1000, labels=c(cuentasSanos[1,2]), adj=0.5, font=4, cex=0.9)
text(2, 1000, labels=c(cuentasSanos[2,2]), adj=0.5, font=4, cex=0.9)
abline(h=nsanos, lwd=3, col="red")
text(1, nsanos+0.05*max, paste(nsanos, " pacientes coincidentes."), col="red", cex = 0.9)

rm(cuentasDiab)
rm(cuentasSanos)
rm(AnyoDiab)
rm(AnyoSanos)
rm(max)
rm(ndiab)
rm(nsanos)
rm(tipoPaciente)

#Distribución por Edad
png(filename = "pp.png", 
    width = 775, height = 456, 
    units = "px", bg = "transparent")
par(mfrow=c(2,2), mar=c(3,3,2,2))
t <- table(subset(csv$Edad, csv$Anyo==2011))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 900), cex.axis = 0.7, type="h", col=colorAnyo[1])
title(main="Diabéticos", cex.main=0.7)
legend("topleft", title="2011", legend=" ")
t <- table(subset(sanos$Edad, csv$Anyo==2011))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 900), cex.axis = 0.7, type="h", col=colorAnyo[1])
title(main="Sanos", cex.main=0.7)
legend("topright", title="2011", legend=" ")
t <- table(subset(csv$Edad, csv$Anyo==2012))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 900), cex.axis = 0.7, type="h", col=colorAnyo[2])
legend("topleft", title="2012", legend=" ")
t <- table(subset(sanos$Edad, csv$Anyo==2012))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 900), cex.axis = 0.7, type="h", col=colorAnyo[2])
legend("topright", title="2012", legend=" ")
dev.off()


par(mfrow=c(1,1), mar=c(3,3,2,2))
t <- table(subset(csv$Edad, csv$Anyo==2011))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 950), cex.axis=1.2, type="h", col=colorAnyo[1])
title(main="Diabéticos", cex.main=1.2)
abline(v=mean(csv[csv$Anyo==2011,"Edad"]), col="red")

t <- table(subset(sanos$Edad, sanos$Anyo==2011))
plot(t, xlab = "Edad", ylab="Número de Pacientes", ylim=c(0, 950), cex.axis=1.2, type="h", col=colorAnyo[1])
title(main="Sanos", cex.main=1.2)
abline(v=mean(sanos[sanos$Anyo==2011,"Edad"]), col="red")

rm(t)

#Sexo Diabéticos vs Sanos
#cuentasDiab <- sqldf("select Sexo, count(*) as total from csv group by Sexo")
#cuentasSanos <- sqldf("select Sexo, count(*) as total from sanos group by Sexo")
max <- 23402
IdDiab2011 <- csv[csv$Anyo==2011, "Id"]
IdDiab2012 <- csv[csv$Anyo==2012, "Id"]
SexoDiab <- csv[csv$Anyo==2011, "Sexo"]
SexoDiab2 <- csv[csv$Anyo==2012 & csv$Id %in% IdDiab2012 & 
                                   !(csv$Id %in% IdDiab2011),"Sexo"]
SexoDiab <- c(SexoDiab, SexoDiab2)
IdSanos2011 <- sanos[sanos$Anyo==2011, "Id"]
IdSanos2012 <- sanos[sanos$Anyo==2012, "Id"]
SexoSano <- sanos[sanos$Anyo==2011, "Sexo"]
SexoSano2 <- sanos[sanos$Anyo==2012 & sanos$Id %in% IdSanos2012 & 
                   !(sanos$Id %in% IdSanos2011),"Sexo"]
SexoSano <- c(SexoSano, SexoSano2)
par(mfrow=c(1,2), mar=c(3,3,3,3))
barplot(table(SexoDiab), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
          xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0,24000))
title(main="Diabéticos (Total años 2011 y 2012)", cex.main=0.7)
total <- 11049
text(14, 3000, labels=c(paste(100*round(6084/total,2), "%")), adj=0.5, font=4, cex=0.9)
text(40, 3000, labels=c(paste(100*round(4965/total,2), "%")), adj=0.5, font=4, cex=0.9)
barplot(table(SexoSano), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
        xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0,24000))
title(main="Sanos (Total años 2011 y 2012)", cex.main=0.7)
total <- 43190
text(14, 3000, labels=c(paste(round(100*20088/total,2), "%")), adj=0.5, font=4, cex=0.9)
text(40, 3000, labels=c(paste(round(100*23102/total,2), "%")), adj=0.5, font=4, cex=0.9)
#rm(cuentasDiab)
#rm(cuentasSanos)
rm(max)
rm(total)
rm(IdDiab2011)
rm(IdDiab2012)
rm(SexoDiab)
rm(SexoDiab2)
rm(IdSanos2011)
rm(IdSanos2012)
rm(SexoSano)
rm(SexoSano2)

barplot(table(d11Baja$Sexo), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
        xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0,1200))
barplot(table(d12Nuevos$Sexo), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
        xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0,1200))

CambiosDiab <- rep("Baja", length(d11Baja$Anyo))
CambiosDiab <- c(CambiosDiab, rep("Nuevos", length(d12Nuevos$Anyo)))
CambiosDiab <- factor(CambiosDiab)

par(mfrow=c(1,1), mar=c(3,3,3,3))
plot(CambiosDiab, col=colorAnyo,
     main="Cambios de pacientes diabéticos en 2011 y 2012", cex.main=0.7, cex.axis=0.7, cex.names=0.7)
text(1, 500, labels=c("10%"), adj=0.5, font=4, cex=0.7)
text(2, 500, labels=c("25%"), adj=0.5, font=4, cex=0.7)

d11$Anyo <- factor(d11$Anyo)
d11$nivel <- factor(d11$CRG)
qplot(d11$Anyo, stat="identity", geom="histogram", fill=d11$CRG)

df <- sqldf("SELECT Anyo, CRG, count(*) as Num from csv group by Anyo, CRG")
df$porcentaje <- df$Num
df[df$Anyo==2011, "porcentaje"] <- round(100*df[df$Anyo==2011, "porcentaje"]/8851, 2)
df[df$Anyo==2012, "porcentaje"] <- round(100*df[df$Anyo==2012, "porcentaje"]/10128, 2)
df$CRG <- factor(df$CRG)
df$Anyo <- factor(df$Anyo)

ggplot(df, aes(x=Anyo, y=porcentaje)) + geom_bar(stat="Identity", aes(fill=df$CRG)) + 
  scale_fill_manual("CRG-base", values=colorCRG) +
  labs(y="%", title="Distribución % CRGs por Año") +
  theme_minimal()
  
rm(df)

#> sum(d11[,7:(length(d11)-N)])
#[1] 808504
#> sum(d12[,7:(length(d12)-N)])
#[1] 728032
#> sum(d11[d11$Id %in% d11_d12$Id,7:(length(d11)-N)])
#[1] 632505
#> sum(d12[d12$Id %in% d11_d12$Id,7:(length(d12)-N)])
#[1] 580457

m <- matrix(nrow=4, ncol=4)
m <- as.data.frame(m)
colnames(m) <- c("Anyo", "Sexo", "TotalATC", "NumPacientes")
m$Anyo <- c(2011, 2011, 2012, 2012)
m$Sexo <- c(1, 2, 1, 2)
m$TotalATC <- c(393152, 415352, 352286, 375746)
m$NumPacientes <- c(4920, 3931, 5629, 4499)
m$Anyo <- factor(m$Anyo)
m$Sexo <- factor(m$Sexo)
m$TotalATC <- m$TotalATC/1000

ggplot(m, aes(x=Anyo, y=TotalATC)) + geom_bar(stat="Identity", aes(fill=m$Anyo)) + 
  scale_fill_manual("", values=colorAnyo) +
  labs(y="Total ATC (Miles)", title="Total ATCs por Año") +
  theme_classic()

ggplot(m, aes(x=Anyo, y=TotalATC, fill=Sexo)) + geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual("", values=colorGenero) +
  labs(y="Total ATC (Miles)", title="Total ATCs por Año") +
  theme_classic()

ggplot(m, aes(x=Anyo, y=TotalATC, fill=Sexo)) + geom_line() + 
  scale_fill_manual("", values=colorGenero) +
  labs(y="Total ATC (Miles)", title="Total ATCs por Año") +
  theme_classic()



###Gráfica de Cristina
pintaLinea <- function(x, i, t0, t5424, t6144, t7071, color) {
  if ( color == "red" ) {
    text(x, t7071[i,"Prob"]*2, t7071[i, "Names"], cex=0.6)
  } else {
    text(x, t5424[i,"Prob"]*2, t5424[i, "Names"], cex=0.6) 
  }
  segments(t0[i,"PosX"], t0[i, "Prob"]*2, t5424[i,"PosX"], t5424[i,"Prob"]*2, col=color)
  segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col=color)
  segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col=color)
}

atcsConclusiones <- c("A02BC", "A03FA", "A06AD", "A10AB", "A10BA", "A10BD", "A10AE", "B01AC",
                      "C03CA", "C08CA", "C09AA", "C09BA", "C10AA", "G04CA", "H02AB",
                      "J01MA", "M01AE", "M02AA", "N02BB", "N02BE", 
                      "N05AL", "N05BA", "R03AC", "R03AK", "R03BB", "R05CB")
atcsEspecificos <- c("A02BC", "A03FA", "A06AD", "A10AB", "A10BA", "A10BD", "A10AE", "B01AC",
                   "C03CA", "C08CA", "C09AA", "C09BA", "C10AA")
atcsGenericos <- c("G04CA", "H02AB",
                   "J01MA", "M01AE", "M02AA", "N02BB", "N02BE", 
                   "N05AL", "N05BA", "R03AC", "R03AK", "R03BB", "R05CB")
  
todos <- rbind(sanos, csv)
lineas <- c(25, 50, 75)
m <- pintaGraficasPerfilesZoom(todos, lineas, 2)
atc <- subset(m, m$CRG %in% c(0, 5424, 6144, 7071) & m$Names %in% atcsConclusiones ) 
etiquetas <- c(10, 20, 30, 40, 50, 60, 70, 80)
m$PosX <- ifelse(m$CRG == 0, 1, m$PosX)
m$PosX <- ifelse(m$CRG == 5424, 2, m$PosX)
m$PosX <- ifelse(m$CRG == 6144, 3, m$PosX)
m$PosX <- ifelse(m$CRG == 7071, 4, m$PosX)
#Ahora hay que concatenar las columnas que no estén en los otros CRGs y que estén en el previo.
u <- unique(atc[atc$CRG != 0, "Names"])
atc_f <- subset(m, m$CRG %in% c(0, 5424, 6144, 7071) & m$Names %in% u)

par(mfrow=c(1,1), mar=c(3,2,3,2))
t <- subset(atc_f, atc_f$Anyo==anyo)
t$CRG <- as.integer(as.character(t$CRG))
t$Prob <- as.numeric(t$Prob)

#Pintamos los específicos
t0 <- subset(t, t$CRG==0 & t$Names %in% atcsEspecificos)
t5424 <- subset(t, t$CRG==5424 & t$Names %in% atcsEspecificos)
t6144 <- subset(t, t$CRG==6144 & t$Names %in% atcsEspecificos)
t7071 <- subset(t, t$CRG==7071 & t$Names %in% atcsEspecificos)
plot(t0$PosX, t0$Prob*2, type ="p", ylim=c(0, max(atc_f$Prob)*2), xlim=c(0.5, 4.5), yaxt="n", xaxt="n", pch=0)
points(t5424$PosX, t5424$Prob*2, pch=1)
points(t6144$PosX, t6144$Prob*2, pch=5)
points(t7071$PosX, t7071$Prob*2, pch=2)
axis(1,  at = unique(t$PosX), labels=unique(t$CRG), cex.axis=0.6)
axis(2, at = etiquetas*2, labels=etiquetas, cex.axis=0.6)
title(main=paste("Evolución de ATCs Específicos por CRG-base", sep=""), cex.main=0.7)
x1 <- 1.8
x2 <- 4.2
pintaLinea(x2, 1, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 2, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 3, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 4, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 5, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x2, 6, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 7, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x2, 8, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 9, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 10, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x1, 11, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x2, 12, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 13, t0, t5424, t6144, t7071, "red")

#Pintamos los genéricos
t0 <- subset(t, t$CRG==0 & t$Names %in% atcsGenericos)
t5424 <- subset(t, t$CRG==5424 & t$Names %in% atcsGenericos)
t6144 <- subset(t, t$CRG==6144 & t$Names %in% atcsGenericos)
t7071 <- subset(t, t$CRG==7071 & t$Names %in% atcsGenericos)
plot(t0$PosX, t0$Prob*2, type ="p", ylim=c(0, max(atc_f$Prob)*2), xlim=c(0.5, 4.5), yaxt="n", xaxt="n", pch=0)
points(t5424$PosX, t5424$Prob*2, pch=1)
points(t6144$PosX, t6144$Prob*2, pch=5)
points(t7071$PosX, t7071$Prob*2, pch=2)
axis(1,  at = unique(t$PosX), labels=unique(t$CRG), cex.axis=0.6)
axis(2, at = etiquetas*2, labels=etiquetas, cex.axis=0.6)
title(main=paste("Evolución de ATCs Genéricos por CRG-base", sep=""), cex.main=0.7)
x1 <- 1.8
x2 <- 4.2
pintaLinea(x2, 1, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 2, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 3, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x1, 4, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x2, 5, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 6, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 7, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 8, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 9, t0, t5424, t6144, t7071, "red")
pintaLinea(x2, 10, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 11, t0, t5424, t6144, t7071, "steelblue")
pintaLinea(x2, 12, t0, t5424, t6144, t7071, "red")
pintaLinea(x1, 13, t0, t5424, t6144, t7071, "steelblue")

rm(i)
rm(x1)
rm(x2)
rm(y1)
rm(y2)
rm(t)
rm(t5424)
rm(t6144)
rm(t7071)
rm(t0)
rm(u)
rm(atc)
rm(atc_f)
rm(anyo)
rm(m)
rm(etiquetas)
rm(lineas)
rm(todos)
rm(atcsConclusiones)
rm(atcsGenericos)
rm(atcsEspecificos)
rm(pintaLinea)

##Constraste entre 

#selección de anyo, crg, variables y obtener el id para el muestreo y el sexo para las gráficas.
IdEvolucion <- getIds2011CRGs2012(csv)
atc <- subset(csv, csv$Id %in% IdEvolucion$Id & csv$Anyo == anyo & csv$CRG==crg, c(1,2,6,7:(ncol(csv) - N)))
Id <- atc[,1]
Sexo <- atc[,2]
atc <- atc[,c(-1,-2,-3)]

#Convertimos en matriz de existencia.
t <- as.data.frame(apply(atc, 2, convierteExistencia))
matriz <- matrix(nrow=0, ncol=length(t))

#Añadimos el Id para el muestreo.
t <- as.data.frame(cbind(Id, Sexo, t))

#Ejecutamos lon veces el muestreo y calculamos la media de cada muestreo.
for(i in 1:lon) {
  Id_s <- sample(Id, size=size, replace=FALSE)
  #t_b <- subset(t, t$Id %in% Id_s & t$Sexo == sexo, c(-1, -2))
  t_b <- subset(t, t$Id %in% Id_s, c(-1, -2))
  
  media <- apply(t_b, 2, mean)
  matriz <- rbind(matriz, media)
}

t <- t[,c(-1,-2)]
colnames(matriz) <- colnames(t)
rownames(matriz) <- c()
matriz <- as.data.frame(matriz)

return(matriz)

par(mfrow=c(1,1), mar=c(4,4,3,3))
plot(h, type = "l", cex.axis = 0.7, xaxt="n", 
     col=color[1], ylab="", xlab="", ylim = c(0,0.8))
points(m, col=color[2], type="l")
axis(1, at=pos, labels= label, cex.axis = 0.5)


######
######

pintaLinea <- function(x, i, t5424, t6144, t7071, color, sexo) {
#  if ( color == "red" ) {
    text(x, t7071[i,"Prob"]*2, t7071[i, "ATC"], cex=0.6, col=colorGenero[sexo])
#  } else {
#    text(x, t5424[i,"Prob"]*2, t5424[i, "ATC"], cex=0.6, col=colorGenero[sexo]) 
#  }
  segments(t5424[i,"PosX"], t5424[i, "Prob"]*2, t6144[i,"PosX"], t6144[i,"Prob"]*2, col=color)
  segments(t6144[i, "PosX"], t6144[i, "Prob"]*2, t7071[i, "PosX"], t7071[i, "Prob"]*2, col=color)
}


atcsAnalizar <- c("R03BB", "R03AC", "R03AK", "R05CB", "N05BA", "N05AL", "A02BC")
lineas <- c(25, 50, 75)
atc <- subset(g, g$CrgOrigen %in% c(5424, 6144, 7071) & g$Año == 2011 & g$ATC %in% atcsAnalizar ) 
dfOrigen <- sqldf("SELECT ATC, Sexo, CrgOrigen as CRG, avg(MediaCrg1) as Media from atc group by ATC, Sexo, CrgOrigen")
dfDestino <- sqldf("SELECT ATC, CrgDestino as CRG, Sexo, avg(MediaCrg2) as Media from atc group by ATC, Sexo, CrgDestino")
df <- rbind(dfOrigen, dfDestino)
atc <- sqldf("SELECT ATC, CRG as CRG, Sexo, avg(Media) as Media from df group by ATC, Sexo, CRG")
rm(df)
rm(dfOrigen)
rm(dfDestino)
etiquetas <- c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90)
atc$PosX <- ifelse(atc$CRG == 5424, 1, 3)
atc$PosX <- ifelse(atc$CRG == 6144, 2, atc$PosX)
#atc$PosX <- ifelse(atc$CRG == 7071, 3, atc$PosX)
#Ahora hay que concatenar las columnas que no estén en los otros CRGs y que estén en el previo.
u <- unique(atc[atc$CRG != 0, "ATC"])
atc_f <- subset(atc, atc$CRG %in% c(0, 5424, 6144, 7071) & atc$ATC %in% u)

par(mfrow=c(1,1), mar=c(4,4,3,3))
t <- atc_f
t$CRG <- as.integer(as.character(t$CRG))
t$Prob <- as.numeric(t$Media)

#Pintamos los específicos
t5424 <- subset(t, t$CRG==5424 & t$ATC %in% atcsAnalizar)
t6144 <- subset(t, t$CRG==6144 & t$ATC %in% atcsAnalizar)
t7071 <- subset(t, t$CRG==7071 & t$ATC %in% atcsAnalizar)

t5424_h <- subset(t5424, t5424$Sexo == "Sexo_1")
t5424_m <- subset(t5424, t5424$Sexo == "Sexo_2")
t6144_h <- subset(t6144, t6144$Sexo == "Sexo_1")
t6144_m <- subset(t6144, t6144$Sexo == "Sexo_2")
t7071_h <- subset(t7071, t7071$Sexo == "Sexo_1")
t7071_m <- subset(t7071, t7071$Sexo == "Sexo_2")
rm(t5424)
rm(t6144)
rm(t7071)

plot(t5424_h$PosX, t5424_h$Prob*2, type ="p", ylim=c(0, max(atc_f$Media)*2), xlim=c(0.5, 3.5), xaxt="n", yaxt="n",pch=0, xlab="", cex.axis=0.7,ylab="")
#points(t5424$PosX, t5424$Prob*2, pch=1)
points(t6144_h$PosX, t6144_h$Prob*2, pch=0)
points(t7071_h$PosX, t7071_h$Prob*2, pch=0)
points(t5424_m$PosX, t5424_m$Prob*2, pch=5)
points(t6144_m$PosX, t6144_m$Prob*2, pch=5)
points(t7071_m$PosX, t7071_m$Prob*2, pch=5)
axis(1,  at = unique(t$PosX), labels=unique(t$CRG), cex.axis=0.6)
axis(2,  at = etiquetas*2, labels=etiquetas, cex.axis=0.6)
x1 <- 0.6
x2 <- 3.4
pintaLinea(x2, 1, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)
pintaLinea(x2, 2, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)
pintaLinea(x2, 3, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)
pintaLinea(x2, 4, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)
pintaLinea(x2, 5, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)
pintaLinea(x2, 6, t5424_h, t6144_h, t7071_h, colorGenero[1], 1)

pintaLinea(x2, 1, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)
pintaLinea(x2, 2, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)
pintaLinea(x2, 3, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)
pintaLinea(x2, 4, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)
pintaLinea(x2, 5, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)
pintaLinea(x2, 6, t5424_m, t6144_m, t7071_m, colorGenero[2], 2)

rm(atcsAnalizar)
rm(lineas)
rm(atc)
rm(etiquetas)
rm(u)
rm(atc_f)
rm(t)
rm(x1)
rm(x2)
rm(g)
rm(t5424_m)
rm(t5424_h)
rm(t6144_m)
rm(t6144_h)
rm(t7071_m)
rm(t7071_h)


#####MAC

source("ac.R")
df <- getATCsCRGsEdad(csv, anyo)
p <- getATCsMCA(df)
a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("5424", "6144", "7070", "7071", 
                                              "A_0","B_0","C_0","D_0","G_0","H_0","J_0","L_0","M_0","N_0","P_0","R_0","S_0","V_0")) 
) +
  labs(title="", cex=0.7) +
  xlab("") + ylab("") + theme_classic()
print(a)
                
a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("5424", "6144", "7070", "7071", 
                                                              "A_0","A_1","B_1","C_1","D_1","G_1","H_1","J_1","L_1","M_1","N_1","P_1","R_1","S_1","V_1")) 
) +
  labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)

source("ac4.R")
df <- getATCsCRGsSexo(csv, anyo)
p <- getATCsMCA(df)
a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("5424", "6144", "7070", "7071", 
                                              "1", "2"))) +
  labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)

a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("1","2", 
                                              "A_1","B_1","C_1","D_1","G_1","H_1","J_1","L_1","M_1","N_1","P_1","R_1","S_1","V_1"))) +
  labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)


df <- getATCsCRGsEdad(csv, anyo)
p <- getATCsMCA(df)
a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("5424", "6144", "7070", "7071", 
                                              "A02BC_0","A03FA_0","A06AD_0","A10BA_0","A10AE_0","B01AC_0","C03CA_0","C08CA_0","C09AA_0",
                                              "C10AA_0","G04CA_0","J01MA_0","M02AA_0","N02BB_0","N02BE_0","N05AL_0","R03AC_0","R03AK_0",
                                              "R03BB_0","R05CB_0", "N05BA_0", "A10AE_1")) 
) + labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)

a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("5424", "6144", "7070", "7071", 
                                                              "A02BC_1","A03FA_1","A06AD_1","A10BA_1","A10AE_1","B01AC_1","C03CA_1","C08CA_1","C09AA_1",
                                                              "C10AA_1","G04CA_1","J01MA_1","M02AA_1","N02BB_1","N02BE_1","N05AL_1","R03AC_1","R03AK_1",
                                                              "R03BB_1","R05CB_1", "N05BA_1", "A10AE_1")) 
) + labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)

a <- fviz_mca_biplot(p,label="var",axes=c(1,2),geom="text",arrows=c(FALSE, TRUE), mode="colprincipal", 
                     col.var="blue", select.var = list(name=c("Sexo_1","Sexo_2", 
                                              "A02BC_1","B01AC_1","C03CA_1","C08CA_1","C09AA_1",
                                              "C10AA_1","J01MA_1","M02AA_1","N02BB_1","N02BE_1","N05AL_1","R03AC_1","R03AK_1",
                                              "R03BB_1","R05CB_1", "N05BA_1"))) +
 labs(title="") + xlab("") + ylab("") + theme_classic()
print(a)

