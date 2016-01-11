
colorAnyo <- c("springgreen2", "turquoise")
colorGenero <- c("slateblue3", "snow4")
colorCRG <- c("tomato", 
              "slateblue", "darkblue", "mediumblue", "blue", "steelblue", "deepskyblue", "skyblue", "lightskyblue", "aliceblue",       
              "saddlebrown", "brown", "chocolate", "burlywood", "wheat", "bisque", "blanchedalmond", "beige", "cornsilk", "whitesmoke",  
#              "seagreen" "darkseagreen", "olivedrab", "aquamarine",  "green", "lightseagreen", "springgreen", "palegreen", "mediumspringgreen", "cyan",  
              "yellow" )


#Gráfico 1 - AÑO.
Anyo <- factor(d$Anyo)
cuentas <- sqldf("select Anyo, count(*) as total from d group by Anyo")
plot(Anyo, xlab="Año", ylab="Número de pacientes", ylim=c(0, 11500), col=colorAnyo)
text(1, 11210, labels=c(cuentas[1,2]))
text(2, 11210, labels=c(cuentas[2,2]))
abline(h=767, lwd=3, col="red")
text(1, 1000, "767 Pacientes coincidentes entre ambos años", col="red")

#Gráfico 2 - EDAD.
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(table(subset(d$Edad, Anyo==2011)), main="Distribución de Edad en 2011", xlab = "Edad", ylab="Número de Pacientes", type="h", col=colorAnyo[1])
plot(table(subset(d$Edad, Anyo==2012)), main="Distribución de Edad en 2012",xlab = "Edad", ylab="Número de Pacientes", type="h", col=colorAnyo[2])

#Gráfico 3 - SEXO.
par(mfrow = c(1, 2), mar = c(4, 4, 3, 3))
cuentas <- sqldf("select Anyo, Sexo, count(*) as total from d group by Anyo, Sexo")
barplot(table(subset(d$Sexo, d$Anyo==2011)), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
        main="Distribución de Sexo en 2011", xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0, 6000))
text(14, 3000, labels=c(cuentas[1,3]),  adj=0.5, font=4, cex=0.9)
text(40, 3000, labels=c(cuentas[2,3]), adj=0.5, font=4, cex=0.9)
barplot(table(subset(d$Sexo, d$Anyo==2012)), col=colorGenero, width = 21, cex.names = 0.7, cex.axis = 0.7, 
        main="Distribución de Sexo en 2012", xlab = "Sexo", ylab="Número de Pacientes", ylim=c(0, 6000))
text(14, 3500, labels=c(cuentas[3,3]), adj=0.5, font=4, cex=0.9)
text(40, 3500, labels=c(cuentas[4,3]), adj=0.5, font=4, cex=0.9)

#Gráfico 4 - CRG
cuentas <- sqldf("select Anyo, CRG, count(*) as total from d group by Anyo, CRG")
#cuentas <- rbind(cuentas, c(2012, 9010, 0))
y <- 900
par(mfrow = c(1, 2), mar = c(4, 4, 3, 3))
barplot(table(subset(d$CRG, d$Anyo==2011)), col=colorCRG, width=21, las=2, cex.names = 0.7, cex.axis = 0.7, horiz=TRUE,
        main="Distribución de CRG-base en 2011", ylab = "CRG-base", xlab="Número de Pacientes")
for (i in 1:21) {
  text(y, i*21 + (i-1)*0.2*20, labels=c(cuentas[i,3]), adj=0.5, font=4, cex=0.6)
}
t <- table(subset(d$CRG, d$Anyo==2012))
t1 <- as.table(as.numeric(c(0)))
rownames(t1) <- "9010"
t <- t +t1
barplot(table(subset(d$CRG, d$Anyo==2012)), col=colorCRG, width=21, las=2, cex.names = 0.7, cex.axis = 0.7, horiz=TRUE,
        main="Distribución de CRG-base en 2012", ylab = "CRG-base", xlab="Número de Pacientes")
for (i in 1:20) {
  text(y, i*21 + (i-1)*0.2*20, labels=c(cuentas[i+21,3]), adj=0.5, font=4, cex=0.6)
}
rm(i)
rm(t)
rm(t1)
rm(y)

#Gráfico 5 - CRG, Sexo y Edad - Boxplot.
#par(mfrow = c(1,2), mar = c(4, 4, 2, 2))
#csv2 <- subset(d, d$Anyo==2011)
#boxplot(csv2$Edad ~ csv2$nivel + csv2$Genero, cex.names = 0.5, cex.axis = 0.7, las=2, horiz=TRUE,
#        col=c(colorGenero1, colorGenero2))
#csv2 <- subset(d, d$Anyo==2012)
#boxplot(csv2$Edad ~ csv2$nivel+ csv2$Genero, cex.names = 0.5, cex.axis = 0.7, las=2, horiz=TRUE,
#        col=c(colorGenero1, colorGenero2))
#
csv2 <- subset(d, d$Anyo==2011)
ggplot(csv2, aes(x=nivel, y=Edad, fill=Genero)) + geom_boxplot(notch=TRUE) + 
  scale_fill_manual(values=colorGenero) +
  labs(x="CRG-base", y="Edad", 
  title="Distribución de pacientes según CRG-base y Sexo en 2011")
csv2 <- subset(d, d$Anyo==2012)
ggplot(csv2, aes(x=nivel, y=Edad, fill=Genero)) + geom_boxplot(notch=TRUE) + 
  scale_fill_manual(values=colorGenero) +
  labs(x="CRG-base", y="Edad", 
       title="Distribución de pacientes según CRG-base y Sexo en 2012")

#Significancia 
f <- unique(d$nivel)
for (i in f) {
  mu1 <- subset(d, d$Anyo==2011 & d$nivel==i & d$Genero == 1)
  mu2 <- subset(d, d$Anyo==2011 & d$nivel==i & d$Genero == 2)
  prueba <- t.test(mu1$Edad, mu2$Edad)
  print(paste("Año = 2011, CRG-base = ", i, ":", prueba$p.value, "\n"))
  
  mu1 <- subset(d, d$Anyo==2012 & d$nivel==i & d$Genero == 1)
  mu2 <- subset(d, d$Anyo==2012 & d$nivel==i & d$Genero == 2)
  prueba <- t.test(mu1$Edad, mu2$Edad)
  print(paste("Año = 2012, CRG-base = ", i, ":", prueba$p.value, "\n"))
}
rm(i)
rm(f)
rm(prueba)
rm(mu1)
rm(mu2)


#Significancia de 6111
mu1 <- subset(d, d$Anyo==2012 & d$nivel==7071 & d$Genero == 1)
mu2 <- subset(d, d$Anyo==2012 & d$nivel==7071 & d$Genero == 2)
prueba <- t.test(mu1$Edad, mu2$Edad)
prueba

