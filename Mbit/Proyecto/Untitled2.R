
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

myplot + 
  scale_x_discrete(breaks=unique(df_M$variable), 
                   labels=addline_format(c("Ambystoma mexicanum", 
                                           "Daubentonia madagascariensis", "Psychrolutes marcidus")))


#Gráfico 7 - CRG comparando los que toman medicamentos de Diabeticos vs el Total.
cat("\n\nGráfica 7.- % de distribución del CRG por Edad\n\n")
for (anyo in unique(csv$Anyo)) {
  cat(paste("\n\nAño", anyo, "\n\n"))
  dx <- subset(csv, csv$Anyo == anyo)
  
  #CALCULO TOTAL
  #Calculamos los totales por edad y por edad y crg para que poder calcular el %.
  df <- as.data.frame(sqldf("select Edad, count(*) as Total from dx group by Edad"))
  df_crg <- as.data.frame(sqldf("select Edad, CRG, count(*) as Total from dx group by Edad, CRG"))
  
  #Añadimos el %
  df_crg <- cbind(df_crg, rep(1.01, nrow(df_crg)))
  df_crg$CRG <- factor(df_crg$CRG)
  colnames(df_crg)[4] <- "Total %"
  for (i in 1:nrow(df_crg)) {
    df_crg[i, "Total %"] <- as.numeric(round(df_crg[i, "Total"] / df[df$Edad == df_crg$Edad[i], "Total"], 4))
  }
  
  #Calculamos las gráficos que vamos a poner en el eje de Edad. 
  pos1 <- as.integer(floor(quantile(df$Edad, 0.25)))
  pos3 <- as.integer(floor(quantile(df$Edad, 0.5)))
  pos5 <- as.integer(ceiling(quantile(df$Edad, 0.75)))
  pos2 = as.integer((pos3-pos1)/2+pos1)
  pos4 = as.integer((pos5-pos3)/2+pos3)

  #Pintamos el gráfico.
  g <- ggplot(df_crg,  
              aes(x=Edad, y=`Total %`)) + geom_bar(stat="identity", aes(fill=df_crg$CRG)) + 
    scale_fill_manual("CRG-base", values=colorCRG) +
    scale_x_continuous(name = "Edad", breaks=c(5, 15, pos1, pos2, pos3, pos4, pos5, 85, 95)) +
    theme(axis.text.x = element_text(size = 8, 
                colour = c("black", "black", "red", "red", "red", "red", "red", "black", "black"))) + 
    theme(title = element_text(size = 8, colour = "black")) + 
    theme(legend.text = element_text(size = 6)) + 
    theme(legend.key.height = unit (0.4, "cm")) + 
    annotate("text", x = as.integer(rownames(df_crg[df_crg$Edad == pos1,])[1])
             , y = 0.02, colour="red", size=2, label = "<------ Percentiles 25% y 75% ------->") + 
    labs(y="%", title="Porcentaje de distribución del CRG por Edad")
  print(g)
}  
rm(anyo)
rm(dx)
rm(df)
rm(df_crg)
rm(g)
rm(pos1)
rm(pos2)
rm(pos3)
rm(pos4)
rm(pos5)
rm(i)

