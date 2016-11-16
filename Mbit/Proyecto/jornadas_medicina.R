

pacientes <- rbind(diab, sanos)
pacientes <- pacientes[pacientes$CRG != 9010, c("Id", "CRG", "Anyo")]
pacientes$CRG <- ifelse(pacientes$CRG==0, 10000, pacientes$CRG)


###########
##### Gráfica de distribución de pacientes
###########

#Sacamos la gráfica de la diferencia solo
par(mfrow=c(2, 1), mar=c(3,4,2,2))
CRGs <- sqldf("select CRG, Anyo, count(*) as Num_CRG from pacientes group by CRG, Anyo")

for (anyo in CRGs$Anyo) {
  d <- subset(CRGs, CRGs$Anyo == anyo)
  d$CRG <- as.factor(d$CRG)
  max <- 41600
  barplot(d$Num_CRG, las = 2, cex.names=0.7, cex.axis=0.7, col=colorCRGConSanos, ylim=c(0, max),
          ylab=paste("Número de Pacientes en ", anyo, sep=""), names=as.character(factor(d$CRG)))
  s <- sum(d[d$CRG != 10000, "Num_CRG"])
    
  for (i in 1:nrow(d)) {
    text(i+(i-1)*0.21-0.5, d[i, "Num_CRG"] + 2600, 
         labels=c(d[i, "Num_CRG"]), 
         adj=0.5, font=4, cex=0.6)
    p <- ifelse(i == nrow(d), 100, round(100*d[i,"Num_CRG"]/s,1) )
    text(i+(i-1)*0.21-0.5, d[i, "Num_CRG"] + 1000, 
         labels=c(paste(p,"%", sep="")), 
         adj=0.5, font=4, cex=0.6)
  }
}

rm(i)
rm(max)
rm(d)
rm(p)
rm(s)
rm(CRGs)
rm(anyo)
rm(pacientes)

#############
###### Gráfica de distribución de ATCs por CRG
#############

todos <- rbind(diab, sanos)
todos$CRG <- ifelse(todos$CRG==0, 10000, todos$CRG)
todos <- todos[todos$CRG != 9010, ]

todosn11_ocur <- pasaAExistencia( subset(todos, todos$Anyo==2011) )
todosn12_ocur <- pasaAExistencia( subset(todos, todos$Anyo==2012) )

todosn11_ocur <- todosn11_ocur[,1:(length(todosn11_ocur)-3)]
todosn12_ocur <- todosn12_ocur[,1:(length(todosn12_ocur)-3)]

todos11 <- subset(todos, todos$Anyo==2011, 7:(length(todos)-N))
todos12 <- subset(todos, todos$Anyo==2012, 7:(length(todos)-N))

todos11_suma <- apply(todos11, 1, sum)
todos12_suma <- apply(todos12, 1, sum)

todos11_suma_ocur <- apply(todosn11_ocur, 1, sum)
todos12_suma_ocur <- apply(todosn12_ocur, 1, sum)

rm(todosn11_ocur)
rm(todosn12_ocur)
rm(todos11)
rm(todos12)

todos11_suma <- as.data.frame(cbind(subset(todos, todos$Anyo==2011, c("CRG")), todos11_suma))
todos12_suma <- as.data.frame(cbind(subset(todos, todos$Anyo==2012, c("CRG")), todos12_suma))

todos11_suma_ocur <- as.data.frame(cbind(subset(todos, todos$Anyo==2011, c("CRG")), todos11_suma_ocur))
todos12_suma_ocur <- as.data.frame(cbind(subset(todos, todos$Anyo==2012, c("CRG")), todos12_suma_ocur))
o <- FALSE
max <- ifelse(o == FALSE, 2100, 3000)
max_pre <- ifelse(o == FALSE, 60, 80)

par(mfrow=c(1,2), mar=c(4,4,3,3))
boxplot(todos11_suma$todos11_suma ~ todos11_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, outline=o,
        ylab="Total dispensaciones en 2011", cex.main=0.7, cex.lab=0.7, ylim=c(0,max))
boxplot(todos12_suma$todos12_suma ~ todos12_suma$CRG, col=colorCRG, cex.axis=0.7, las=2, outline=o,
        ylab="Total dispensaciones en 2012", cex.main=0.7, cex.lab=0.7, ylim=c(0,max))

boxplot(todos11_suma_ocur$todos11_suma ~ todos11_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, outline=o,
        ylab="Familias ATCs distintas dispensadas en 2011", cex.main=0.7, cex.lab=0.7, ylim=c(0,max_pre))
boxplot(todos12_suma_ocur$todos12_suma ~ todos12_suma_ocur$CRG, col=colorCRG, cex.axis=0.7, las=2, outline=o,
        ylab="Familias ATCs distintas dispensadas en 2012", cex.main=0.7, cex.lab=0.7, ylim=c(0,max_pre))


rm(max)
rm(max_pre)
rm(todos)
rm(todos11_suma_ocur)
rm(todos11_suma)
rm(todos12_suma_ocur)
rm(todos12_suma)
rm(o)
