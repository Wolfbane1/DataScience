
colorCRG <- c("thistle", 
              "darkblue", "chocolate", "green", "darkgrey", "firebrick4", "floralwhite", "firebrick", 
              "lemonchiffon", "red",       
              "slateblue", "saddlebrown", "aquamarine", "purple", "wheat", "orchid", "violetred", "saddlebrown", 
              "pink", "yellow", "olivedrab", "navajowhite")


#Pintamos el gráfico.
g <- ggplot(df_crg,  
            aes(x=Edad, y=`Total %`)) + geom_bar(stat="identity", aes(fill=df_crg$CRG)) + 
  scale_fill_manual("CRG-base", values=colorCRG) +
  theme(axis.text.x = element_text(size = 8, colour = "black")) + 
  theme(title = element_text(size = 8, colour = "black")) + 
  theme(legend.text = element_text(size = 6)) + 
  theme(legend.key.height = unit (0.4, "cm")) + 
  labs(x="Edad", y="%", title="% de distribución del CRG por Edad")
print(g)


######Probabilidad. 


#Gráfico 7 - CRG comparando los que toman medicamentos de Diabeticos vs el Total.
cat("\n\nGráfica 7.- % de distribución del CRG por Edad\n\n")
  anyo = 2011
  cat(paste("\n\nAño", anyo, "\n\n"))
  dx <- subset(csv, csv$Anyo == anyo)
  
  pacientes <- as.data.frame(cbind(dx$Id, dx$CRG, 
                                   dx$totalATCTodos, dx$totalATCDiabeticos, dx$totalATCOtros,
                                   rep(1.01, length(dx$Id)), rep(1.01, length(dx$Id)), rep(1.01, length(dx$Id))))
  colnames(pacientes) <- c("Id", "CRG", "TotalATCs", "TotalATCDiab", "TotalATCOtros", 
                           "%TotalATc", "%TotalATCDiab", "%TotalATCOtros")
  
    
  #Calculamos los totales por edad y por edad y crg para que poder calcular el %.
  df <- as.data.frame(sqldf("select CRG, sum(totalATCTodos) as TotalATCs, 
                              sum(totalATCDiabeticos) as TotalATCDiab, 
                              sum(totalATCOtros) as TotalATCOtros
                              from dx group by CRG"))
  
  #Calculamos el %
  for (i in 1:nrow(pacientes)) {
    pacientes[i, "%TotalATc"] <- as.numeric(round(pacientes[i, "TotalATCs"] / df[df$CRG == pacientes$CRG[i], "TotalATCs"], 4))
    pacientes[i, "%TotalATCDiab"] <- as.numeric(round(pacientes[i, "TotalATCDiab"] / df[df$CRG == pacientes$CRG[i], "TotalATCDiab"], 4))
    pacientes[i, "%TotalATCOtros"] <- as.numeric(round(pacientes[i, "TotalATCOtros"] / df[df$CRG == pacientes$CRG[i], "TotalATCOtros"], 4))
  }
  
  pacientes$CRG <- factor(pacientes$CRG)
    crg = 5424  
    i <- 1
    par(mfrow=c(1,1))

    for (crg in unique(dx$CRG)) {
      cat(paste("\n\nGráfica ATC.CRG.", i, ".", anyo, ": ", crg, 
                " - Distribución de Pacientes por toma de ATCs",
                ", Número de pacientes: ", length(pacientes[pacientes$CRG==crg, "CRG"]), "\n\n", sep=""))
     
      p1 <- cbind(as.data.frame(rep("Total ATC", 
                                    length(pacientes[pacientes$CRG==crg, "%TotalATc"]))), 
                     pacientes[pacientes$CRG==crg, "%TotalATc"])
      p2 <- cbind(as.data.frame(rep("Total ATC Diabeticos", 
                                    length(pacientes[pacientes$CRG==crg, "%TotalATCDiab"]))),
                     pacientes[pacientes$CRG==crg, "%TotalATCDiab"])
      colnames(p1) <- c("Tipo", "%Toma")
      colnames(p2) <- c("Tipo", "%Toma")
      p <- rbind(p1, p2)
     
      #Pintamos el gráfico de total.
      plot(p$`%Toma` ~ p$Tipo, type = "l", cex.axis = 0.7,  
           col=c(colorCRG[i], "black"),
           xlab = "Número de pacientes", ylab="% de ATCs (total)")
      title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
            cex.main=0.7)
      
      #Pintamos el gráfico de total de ATCs
      plot(pacientes[pacientes$CRG==crg, "%TotalATCDiab"], type = "l", cex.axis = 0.7,  
           col=colorCRG[i],
           xlab = "Número de pacientes", ylab="% de ATCs (Diabéticos)")
      title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
            cex.main=0.7)
      
      i <- i + 1
    }#fin bucle CRG.    
    
    
    print(g)

rm(anyo)
rm(dx)
rm(df)
rm(pacientes)
rm(g)
rm(p1)
rm(p2)
rm(p)

p_crg <- pacientes[pacientes$CRG==crg, c("%TotalATc", "%TotalATCDiab")]
p1 <- sqldf("select `%TotalATc` as `%ATC`, count(*) as NumATC, 0 as NumDiab from p_crg group by `%TotalATc`")
p2 <- sqldf("select `%TotalATCDiab` as `%ATC`, 0 as NumATC, count(*) as NumDiab from p_crg group by `%TotalATCDiab`")
p1$`%ATC` <- round(p1$`%ATC`, 2)
p2$`%ATC` <- round(p2$`%ATC`, 2)
p <- rbind(p1, p2)
p <- sqldf("select `%ATC`, sum(NumATC) as NumATC, sum(NumDiab) as NumDiab from p group by `%ATC`")

#Pintamos el gráfico de total.
plot(p$NumATC, p$`%ATC`, type="l",col="red")
lines(p$NumDiab, p$`%ATC`, col="green")

#Pintamos el gráfico de total.
plot(pacientes[pacientes$CRG==crg, "%TotalATc"], type = "l", cex.axis = 0.7,  
     col="red",
     xlab = "Número de pacientes", ylab="% de ATCs (total)")
lines(pacientes[pacientes$CRG==crg, "%TotalATCDiab"], col="blue")
title(main=paste("CRG:", crg, ", Año:", anyo, " - Distribución de probabilidad de ATC", sep=""), 
      cex.main=0.7)

