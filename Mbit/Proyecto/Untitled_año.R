diabn11_ocur <- pasaAExistencia( subset(diab, diab$Anyo==2011) )
diabn12_ocur <- pasaAExistencia( subset(diab, diab$Anyo==2012) )

diabn11_ocur <- diabn11_ocur[,1:(length(diabn11_ocur)-3)]
diabn12_ocur <- diabn12_ocur[,1:(length(diabn12_ocur)-3)]

diab11 <- subset(diab, diab$Anyo==2011, 7:(length(diab)-N))
diab12 <- subset(diab, diab$Anyo==2012, 7:(length(diab)-N))

diab11_suma <- apply(diab11, 1, sum)
diab12_suma <- apply(diab12, 1, sum)

diab11_suma_ocur <- apply(diabn11_ocur, 1, sum)
diab12_suma_ocur <- apply(diabn12_ocur, 1, sum)

rm(diabn11_ocur)
rm(diabn12_ocur)
rm(diab11)
rm(diab12)

diab11_suma <- as.data.frame(cbind(subset(diab, diab$Anyo==2011, c("CRG")), diab11_suma))
diab12_suma <- as.data.frame(cbind(subset(diab, diab$Anyo==2012, c("CRG")), diab12_suma))

diab11_suma_ocur <- as.data.frame(cbind(subset(diab, diab$Anyo==2011, c("CRG")), diab11_suma_ocur))
diab12_suma_ocur <- as.data.frame(cbind(subset(diab, diab$Anyo==2012, c("CRG")), diab12_suma_ocur))

par(mfrow=c(1,2), mar=c(4,4,3,3))
boxplot(diab11_suma$diab11_suma ~ diab11_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Distribución total Dispensación ATCs en 2011 por CRG", 
        ylab="Número total de dispensación", cex.main=0.7, cex.lab=0.7, ylim=c(0,3000))
boxplot(diab12_suma$diab12_suma ~ diab12_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Distribución total Dispensación ATCs en 2012 por CRG", 
        ylab="Número total de dispensación", cex.main=0.7, cex.lab=0.7, ylim=c(0,3000))

boxplot(diab11_suma_ocur$diab11_suma ~ diab11_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Distribución Ocurrencia Dispensación ATCs en 2011 por CRG", 
        ylab="Número ATCs dispensados", cex.main=0.7, cex.lab=0.7, ylim=c(0,60))
boxplot(diab12_suma_ocur$diab12_suma ~ diab12_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Distribución Ocurrencia Dispensación ATCs en 2012 por CRG", 
        ylab="Número ATCs dispensados", cex.main=0.7, cex.lab=0.7, ylim=c(0,60))

rm(diab11_suma)
rm(diab12_suma)
rm(diab11_suma_ocur)
rm(diab12_suma_ocur)

######################PACIENTES COMUNES

Ids <- getDatosPacientesCoincidentes(csv)

diabn11_ocur <- pasaAExistencia( subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id ) )
diabn12_ocur <- pasaAExistencia( subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id) )

diabn11_ocur <- diabn11_ocur[,1:(length(diabn11_ocur)-3)]
diabn12_ocur <- diabn12_ocur[,1:(length(diabn12_ocur)-3)]

diab11 <- subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id, 7:(length(diab)-N))
diab12 <- subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id, 7:(length(diab)-N))

diab11_suma <- apply(diab11, 1, sum)
diab12_suma <- apply(diab12, 1, sum)

diab11_suma_ocur <- apply(diabn11_ocur, 1, sum)
diab12_suma_ocur <- apply(diabn12_ocur, 1, sum)

rm(diabn11_ocur)
rm(diabn12_ocur)
rm(diab11)
rm(diab12)

diab11_suma <- as.data.frame(cbind(subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id, c("CRG")), 
                                   diab11_suma))
diab12_suma <- as.data.frame(cbind(subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id, c("CRG")), 
                                   diab12_suma))

diab11_suma_ocur <- as.data.frame(cbind(subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id, c("CRG")), 
                                        diab11_suma_ocur))
diab12_suma_ocur <- as.data.frame(cbind(subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id, c("CRG")), 
                                        diab12_suma_ocur))

par(mfrow=c(1,2), mar=c(4,4,3,3))
boxplot(diab11_suma$diab11_suma ~ diab11_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Pacientes Comunes. Distribución total Dispensación ATCs en 2011 por CRG", 
        ylab="Número total de dispensación", cex.main=0.7, cex.lab=0.7, ylim=c(0,3000))
boxplot(diab12_suma$diab12_suma ~ diab12_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Pacientes Comunes. Distribución total Dispensación ATCs en 2012 por CRG", 
        ylab="Número total de dispensación", cex.main=0.7, cex.lab=0.7, ylim=c(0,3000))

boxplot(diab11_suma_ocur$diab11_suma ~ diab11_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Pacientes Comunes. Distribución Ocurrencia Dispensación ATCs en 2011 por CRG", 
        ylab="Número ATCs dispensados", cex.main=0.7, cex.lab=0.7, ylim=c(0,60))
boxplot(diab12_suma_ocur$diab12_suma ~ diab12_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, 
        main="Pacientes Comunes. Distribución Ocurrencia Dispensación ATCs en 2012 por CRG", 
        ylab="Número ATCs dispensados", cex.main=0.7, cex.lab=0.7, ylim=c(0,60))

rm(diab11_suma)
rm(diab12_suma)
rm(diab11_suma_ocur)
rm(diab12_suma_ocur)

######################PACIENTES COMUNES

Ids <- getDatosPacientesCoincidentes(csv)
Ids <- subset(Ids, Ids$CRG_11==5424 & Ids$CRG_12 %in% c(5424, 6144, 7071))

diabn5424_ocur <- pasaAExistencia( subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id ) )
diabn5424_ocur <- diabn5424_ocur[,1:(length(diabn5424_ocur)-3)]

diab5424 <- subset(diab, diab$Anyo==2011 & diab$Id %in% Ids$Id, 7:(length(diab)-N))

diab5424_suma <- apply(diab5424, 1, sum)

diab5424_suma_ocur <- apply(diabn5424_ocur, 1, sum)

rm(diabn5424_ocur)
rm(diab5424)

diab5424_suma <- as.data.frame(cbind(subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id, c("CRG")), 
                                     diab5424_suma))

diab5424_suma_ocur <- as.data.frame(cbind(subset(diab, diab$Anyo==2012 & diab$Id %in% Ids$Id, c("CRG")), 
                                          diab5424_suma_ocur))

par(mfrow=c(1,2), mar=c(4,4,3,3))
boxplot(diab5424_suma$diab5424_suma ~ diab5424_suma$CRG, col=c(colorCRG[1], colorCRG[9], colorCRG[20]), cex.axis=0.7, las=2, 
        main="Pacientes Evolución. Distribución total Dispensación ATCs en 2011 por CRG", 
        ylab="Número total de dispensación", cex.main=0.7, cex.lab=0.7, ylim=c(0,50), notch=TRUE)
boxplot(diab5424_suma_ocur$diab5424_suma ~ diab5424_suma_ocur$CRG, col=c(colorCRG[1], colorCRG[9], colorCRG[20]), cex.axis=0.7, las=2, 
        main="Pacientes Evolución. Distribución Ocurrencia Dispensación ATCs en 2011 por CRG", 
        ylab="Número ATCs dispensados", cex.main=0.7, cex.lab=0.7, ylim=c(0,30))


rm(diab5424_suma)
rm(diab5424_suma_ocur)
rm(Ids)





#```{r results='asis', echo=FALSE, messages=FALSE, warnings=FALSE, eval=TRUE}
csv_a <- csv[csv$Id %in% Ids$Id,]
cat("\n\n\t\tATCS: ")
cat(atcImportantes)
cat("\n\n")
par(mfrow=c(1,4), mar=c(3,3,2,2))
d <- csv_a[, c(atcImportantes, "Anyo")]
cat("\n\n<b>Distribución de los ATCs importantes</b>\n\n")
for (i in 1:16) {
  boxplot(d[,i] ~ d$Anyo, main=colnames(d)[i], ylab="Disposiciones")
}

boxplot(d[,1:16] ~ d$CRG_12, main=colnames(d)[i], ylab="Disposiciones")

cat("\n\n\t\tSe puede observar que en general parece que la distribución en 2012 tiene una cola un poco 
    menor, aunque por lo que se puede observar parece que la mediana no cambia. Como hay muchos pacientes 
    que no están tomando estos ATCs puede que afecte a la mediana, así que merece la pena revisar la 
    distribución sin contabilizar los 0's.\n\n")

cat("\n\n<b>Distribución de los ATCs importantes sin 0's</b>\n\n")
for (i in 1:16) {
  a <- d[d[,i] >0, c(i,17)]
  boxplot(a[,1] ~ a$Anyo, main=colnames(d)[i], ylab="Disposiciones")
}
cat("\n\n\t\tNo se observa grandes cambios (las cajas parecen que estar un poco más arriba) en relación 
    a la mediana. La escala es muy grande por el número de outliers posiblemente se vea algo más si 
    aplicamos el logaritmo para 'bajar' la escala.\n\n")

cat("\n\n<b>Distribución de los ATCs importantes sin 0's y reducido a Logs</b>\n\n")
for (i in 1:16) {
  a <- d[d[,i] >0, c(i,17)]
  a[,1] <- log(a[,1])
  boxplot(a[,1] ~ a$Anyo, main=paste("Log", colnames(d)[i]), ylab="Disposiciones")
}
cat("\n\n\t\tSe puede observar que efectivamente tiene menos outliers en el 2012 que en 2011 de forma 
    general. Además, algunas cajas sí que se ven algo más chicas en 2012 que en 2011 pero, la mediana 
    no cambia entre los años. Todo apunta a que, al menos en esto ATCs, en el 2012 a pesar de tener más 
    pacientes la distribución de la suma de disposiciones.\n\n")

par(mfrow=c(1,1))
rm(d)
rm(i)
rm(a)
rm(csv_a)

#```



d11 <- subset(csv, csv$Anyo==2011, c(3,7:(length(csv)-N)))
d12 <- subset(csv, csv$Anyo==2012, c(3,7:(length(csv)-N)))
d_11 <- reduceMatrizATC(d11, 2:length(d11), 1, "SUMA_FAMILIAS")
d_12 <- reduceMatrizATC(d12, 2:length(d12), 1, "SUMA_FAMILIAS")
d_11 <- as.data.frame(cbind(d_11, d11$Edad))
d_12 <- as.data.frame(cbind(d_12, d12$Edad))

par(mfrow=c(2,1), mar=c(4,4,3,3))
boxplot(d_11$A, d_12$A, ylim=c(0,50))
boxplot(d_11$N, d_12$N, ylim=c(0,50))




glm <- svm( Diabetico~A01AB+A01AC+A02AD+A02AH+A02BA+A02BC+A02BX+A03AA+A03AB+A03AX+A03BB+A03FA+A04AA+A05AA+A06AC+A06AD+A07CA+A07DA+A07XA+A10AB+A10AC+A10AD+A10AE+A10BA+A10BB+A10BD+A10BF+A10BG+A10BH+A10BX+A11CA+A11CC+A11DA+A11HA+A12AA+A12AX+A12BA+B01AA+B01AB+B01AC+B02AA+B03AA+B03AB+B03BA+B03BB+B05BB+B05XA+B06AA+C01DA+C01EB+C02AC+C02CA+C03BA+C03CA+C04AD+C04AE+C05AA+C05AE+C05BX+C05CA+C07AB+C07AG+C07BB+C08CA+C08DA+C09AA+C09BA+C09BB+C09CA+C09DA+C09DB+C09DX+C09XA+C10AA+C10AB+C10AX+C10BA+D01AC+D01AE+D05AX+D06AX+D06BA+D06BB+D06BX+D07AA+D07AB+D07AC+D07AD+D07BC+D07XC+D10AF+D11AH+G01AA+G01AF+G02CB+G03AA+G03CA+G03DA+G03DC+G03HA+G03HB+G04BC+G04BD+G04BE+G04CA+G04CB+G04CX+H01BA+H01CB+H02AA+H02AB+H03AA+H03BB+H03CA+H04AA+J01AA+J01CA+J01CE+J01CF+J01CR+J01DB+J01DC+J01DD+J01EE+J01FA+J01FF+J01GB+J01MA+J01RA+J01XD+J01XE+J01XX+J02AC+J04AB+J04AK+J04AM+J05AB+J05AH+J07AP+M01AB+M01AC+M01AE+M01AH+M01AX+M02AA+M02AC+M03AX+M03BX+M04AC+M05BA+N01BA+N01BB+N01BX+N02AA+N02AX+N02BA+N02BB+N02BE+N02BG+N02CA+N02CC+N03AE+N03AF+N03AG+N03AX+N05BA+N05BB+N05CD+N05CF+N06AA+N06AB+N06AX+N06BX+N06DX+N07CA+P01AB+P01BA+P01BB+P02CA+P03AC+R01AC+R01AD+R01AX+R03AC+R03AK+R03BA+R03BB+R03DC+R05CB+R05DA+R05DB+R06AB+R06AC+R06AD+R06AE+R06AX+S01AA+S01AD+S01AX+S01BA+S01BC+S01CA+S01CC+S01EA+S01ED+S01EE+S01FA+S01FB+S01GX+S01XA+S02AA+S02CA+S03CA+V03AE+V03AF+V04CX+Edad+Sexo , data = train , 
kernel = "radial", nu=0.3, cost=1e1)
