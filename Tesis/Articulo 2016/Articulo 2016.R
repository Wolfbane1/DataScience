
#1.- SELECCION DE PACIENTES PARA EL ARTICULO

## Clasificación de pacientes para el análisis.
crgDiabetico <- c(5424)
crgHipertenso <- c(5192)
crgDiabeticoHipertenso <- c(6144)
crgDiabeticoHipertensoYMas <- c(7070, 7071)

crgDiabeticosNivel2 <- c(6111, 6120, 6130, 6140, 6141, 6142, 6143, 6145)
crgDHipertensosNivel2 <- c(6124, 6242)
crgDiabeticosNivel3 <- c(7001, 7010, 7011, 7012, 7020, 7021, 7022, 7023)

## Creamos variable y columna para poder filtrar los pacientes. 
p_articulo <- pacientes
p_articulo$CRG4_2012 <- substr(p_articulo$CRG_2012, 1, 4)
p_articulo[p_articulo$CRG4_2012 %in% crgDiabetico, "CRG_Analisis"] <- "Diab"
p_articulo[p_articulo$CRG4_2012 %in% crgHipertenso, "CRG_Analisis"] <- "Hyper"
p_articulo[p_articulo$CRG4_2012 %in% crgDiabeticoHipertenso, "CRG_Analisis"] <- "Diab_Hyper"
p_articulo[p_articulo$CRG4_2012 %in% crgDiabeticoHipertensoYMas, "CRG_Analisis"] <- "Diab_Hyper_Other"
p_articulo[p_articulo$CRG4_2012 %in% crgDiabeticosNivel2, "CRG_Analisis"] <- "Diab_L2"
p_articulo[p_articulo$CRG4_2012 %in% crgDHipertensosNivel2, "CRG_Analisis"] <- "Hiper_L2"
p_articulo[p_articulo$CRG4_2012 %in% crgDiabeticosNivel3, "CRG_Analisis"] <- "Diab_L3"
p_articulo <- p_articulo[!is.na(p_articulo$CRG_Analisis), ]

table(p_articulo$CRG_Analisis)

rm(crgDiabetico)
rm(crgHipertenso)
rm(crgDiabeticoHipertenso)
rm(crgDiabeticoHipertensoYMas)
rm(crgDiabeticosNivel2) 
rm(crgDHipertensosNivel2) 
rm(crgDiabeticosNivel3)

## Añadimos la variable del tipo de Diabético. Para ello miramos si de 2010 a 2012 tiene algún diagnóstico
## como diabético. 
i_p_2012 <- interacciones[interacciones$Id %in% p_articulo$Id, ]
i_p_2012 <- i_p_2012[i_p_2012$Anyo %in% c(2010,2011,2012) & i_p_2012$Tipo == "D", ]
procDiabeticos <- diagnosticos[diagnosticos$DIABETES != "", ]
i_p_2012 <- i_p_2012[i_p_2012$Codigo %in% procDiabeticos$CODIGO, ]
i_p_2012 <- unique(i_p_2012[,c("Id", "Codigo")])

d_diabeticos <- diagnosticos[diagnosticos$DIABETES != "" & diagnosticos$DIABETES != "DMT1", ]
i_p_2012[i_p_2012$Codigo %in% d_diabeticos$CODIGO, "TipoDiabetico"] <- "DMT2"
d_diabeticos <- diagnosticos[diagnosticos$DIABETES == "DMT1", ]
i_p_2012[i_p_2012$Codigo %in% d_diabeticos$CODIGO, "TipoDiabetico"] <- "DMT1"

rm(procDiabeticos)
rm(d_diabeticos)

i_p_2012 <- sqldf("SELECT ID, MIN(TipoDiabetico) AS TipoDiabetico FROM i_p_2012 GROUP BY ID")
p_articulo[p_articulo$Id %in% i_p_2012$Id, "TipoDiabetes"] <- i_p_2012[i_p_2012$Id %in% p_articulo$Id, "TipoDiabetico"]

crgAnalisisDiabeticos <- c("Diab", "Diab_Hyper", "Diab_Hyper_Other", "Diab_L2", "Diab_L3")
p_articulo[is.na(p_articulo$TipoDiabetes) & p_articulo$CRG_Analisis %in% crgAnalisisDiabeticos, "TipoDiabetes" ] <- "DM"

rm(i_p_2012)
rm(crgAnalisisDiabeticos)

#2.- SELECCION DE VARIABLE DE ANTIDEPRESIVOS 
#a) Analgésicos       --> Subgrupo N02	Analgésicos	Completada	02/06/2010
#     OPIOIDES: N02A (N02AA, N02AB, N02AC, N02AD, N02AE, N02AF, N02AX)
#     OTROS ANALGÉSICOS Y ANTIPIRÉTICOS: N02B (N02BA, N02BB, N02BE, N02BG)
#b) Ansiolíticos
#     ANSIOLÍTICOS: N05B (N05BA a N05BX)
#c) Antidepresivos
#     HIPNÓTICOS Y SEDANTES: N05C (N05CA a N05CX)
#     ANTIDEPRESIVOS: N06A (N06AA a N06AX)
#d) Antipsicóticos
#     ANTIPSICÓTICOS: N05A (N05AA a N05AX) 
#
# ¿Qué hacemos con estos?
#     PREPARADOS ANTIMIGRAÑOSOS: N02C (N02CA, N02CC, N02CX)
#     PSICOESTIMULANTES, AGENTES UTILIZADOS PARA LA ADHD Y NOOTRÓPICOS: N06B (N06BA a N06BX)
#     FÁRMACOS ANTI-DEMENCIA: N06D (N06DA a N06DX)

atcAnalgesicos <- c("N02AA", "N02AB", "N02AC", "N02AD", "N02AE", "N02AF", "N02AX", "N02BA", "N02BB", "N02BE", "N02BG")
atcAnsioliticos <- c("N05BA", "N05BB", "N05BC", "N05BE", "N05BX")
atcAntidepresivos <- c("N05CA", "N05CB", "N05CC", "N05CD", "N05CE", "N05CF", "N05CH", "N05CM", "N05CX",
                       "N06AA", "N06AB", "N06AF", "N06AG", "N06AX")
atcAntipsicoticos <- c("N05AA", "N05AB", "N05AC", "N05AD", "N05AE", "N05AF", "N05AG", "N05AH", "N05AL", "N05AN", "N05AX")

i_p_2012 <- interacciones[interacciones$Id %in% p_articulo$Id, ]
i_p_2012 <- i_p_2012[i_p_2012$Ambito == 3 & i_p_2012$Anyo == 2012, ]

#Analgesicos
atcs <- dimATC[dimATC$ATC5 %in% atcAnalgesicos, ]
i_p_2012_anal <- i_p_2012[i_p_2012$Codigo %in% atcs$ATC, ]
p_2012_anal <- sqldf("SELECT Id, Tipo, count(distinct Mes_Atencion) as Analgesicos_numMeses, count(distinct Codigo) as Analgesicos_numAtcDiferentes, count(*) as Analgesicos_numAtcs FROM i_p_2012_anal GROUP BY Id, Tipo")

p_2012_com <- p_2012_anal[p_2012_anal$Tipo == "ATC_COM", ]
p_2012_hos <- p_2012_anal[p_2012_anal$Tipo == "ATC_HOS", ]

p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Anal_numMeses"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Analgesicos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Anal_numAtcDif"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Analgesicos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Anal_numTotalAtc"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Analgesicos_numAtcs"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Anal_numMeses"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Analgesicos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Anal_numAtcDif"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Analgesicos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Anal_numTotalAtc"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Analgesicos_numAtcs"]

rm(p_2012_com)
rm(p_2012_hos)

#Ansioliticos
atcs <- dimATC[dimATC$ATC5 %in% atcAnsioliticos, ]
i_p_2012_ansi <- i_p_2012[i_p_2012$Codigo %in% atcs$ATC, ]
p_2012_ansi <- sqldf("SELECT Id, Tipo, count(distinct Mes_Atencion) as Ansioliticos_numMeses, count(distinct Codigo) as Ansioliticos_numAtcDiferentes, count(*) as Ansioliticos_numAtcs FROM i_p_2012_ansi GROUP BY Id, Tipo")

p_2012_com <- p_2012_ansi[p_2012_ansi$Tipo == "ATC_COM", ]
p_2012_hos <- p_2012_ansi[p_2012_ansi$Tipo == "ATC_HOS", ]

p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Ansi_numMeses"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Ansioliticos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Ansi_numAtcDif"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Ansioliticos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Ansi_numTotalAtc"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Ansioliticos_numAtcs"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Ansi_numMeses"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Ansioliticos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Ansi_numAtcDif"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Ansioliticos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Ansi_numTotalAtc"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Ansioliticos_numAtcs"]

rm(p_2012_com)
rm(p_2012_hos)

#Antidepresivos
atcs <- dimATC[dimATC$ATC5 %in% atcAntidepresivos, ]
i_p_2012_antide <- i_p_2012[i_p_2012$Codigo %in% atcs$ATC, ]
p_2012_antide <- sqldf("SELECT Id, Tipo, count(distinct Mes_Atencion) as Antidepresivos_numMeses, count(distinct Codigo) as Antidepresivos_numAtcDiferentes, count(*) as Antidepresivos_numAtcs FROM i_p_2012_antide GROUP BY Id, Tipo")

p_2012_com <- p_2012_antide[p_2012_antide$Tipo == "ATC_COM", ]
p_2012_hos <- p_2012_antide[p_2012_antide$Tipo == "ATC_HOS", ]

p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antide_numMeses"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antidepresivos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antide_numAtcDif"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antidepresivos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antide_numTotalAtc"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antidepresivos_numAtcs"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antide_numMeses"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antidepresivos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antide_numAtcDif"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antidepresivos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antide_numTotalAtc"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antidepresivos_numAtcs"]

rm(p_2012_com)
rm(p_2012_hos)

#Antipsicoticos
atcs <- dimATC[dimATC$ATC5 %in% atcAntipsicoticos, ]
i_p_2012_antips <- i_p_2012[i_p_2012$Codigo %in% atcs$ATC, ]
p_2012_antips <- sqldf("SELECT Id, Tipo, count(distinct Mes_Atencion) as Antipsicoticos_numMeses, count(distinct Codigo) as Antipsicoticos_numAtcDiferentes, count(*) as Antipsicoticos_numAtcs FROM i_p_2012_antips GROUP BY Id, Tipo")

p_2012_com <- p_2012_antips[p_2012_antips$Tipo == "ATC_COM", ]
p_2012_hos <- p_2012_antips[p_2012_antips$Tipo == "ATC_HOS", ]

p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antips_numMeses"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antipsicoticos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antips_numAtcDif"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antipsicoticos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Antips_numTotalAtc"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Antipsicoticos_numAtcs"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antips_numMeses"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antipsicoticos_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antips_numAtcDif"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antipsicoticos_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Antips_numTotalAtc"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Antipsicoticos_numAtcs"]


#Resto de Medicamento
atcTodos <- c(atcAnalgesicos, atcAnsioliticos, atcAntidepresivos, atcAntipsicoticos)

atcs <- dimATC[! dimATC$ATC5 %in% atcTodos, ]
i_p_2012_resto <- i_p_2012[i_p_2012$Codigo %in% atcs$ATC, ]
p_2012_resto <- sqldf("SELECT Id, Tipo, count(distinct Mes_Atencion) as Resto_numMeses, count(distinct Codigo) as Resto_numAtcDiferentes, count(*) as Resto_numAtcs FROM i_p_2012_resto GROUP BY Id, Tipo")

p_2012_com <- p_2012_resto[p_2012_resto$Tipo == "ATC_COM", ]
p_2012_hos <- p_2012_resto[p_2012_resto$Tipo == "ATC_HOS", ]

p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Resto_numMeses"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Resto_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Resto_numAtcDif"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Resto_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_com$Id, "Com_Resto_numTotalAtc"] <- p_2012_com[p_2012_com$Id %in% p_articulo$Id, "Resto_numAtcs"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Resto_numMeses"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Resto_numMeses"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Resto_numAtcDif"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Resto_numAtcDiferentes"]
p_articulo[p_articulo$Id %in% p_2012_hos$Id, "Hos_Resto_numTotalAtc"] <- p_2012_hos[p_2012_hos$Id %in% p_articulo$Id, "Resto_numAtcs"]


rm(p_2012_hos)
rm(p_2012_com)

rm(atcAnalgesicos)
rm(atcAnsioliticos)
rm(atcAntidepresivos)
rm(atcAntipsicoticos)
rm(atcs)
rm(atcTodos)

rm(i_p_2012_anal)
rm(i_p_2012_ansi)
rm(i_p_2012_antips)
rm(i_p_2012_antide)
rm(i_p_2012_resto)
rm(i_p_2012)

rm(p_2012_anal)
rm(p_2012_ansi)
rm(p_2012_antide)
rm(p_2012_antips)
rm(p_2012_resto)

# 3.- VARIABLE DE HOSPITALIZACIÓN

# 4.- Juego de Datos de Comunidad y de Hospital
camposCom <- c("Id", "Sexo", "FechaNacimiento", "CRG4_2012", "CRG_Analisis", "TipoDiabetes", 
            "ATC_2012_Com", "Com_Anal_numTotalAtc", "Com_Ansi_numTotalAtc", "Com_Antide_numTotalAtc",
            "Com_Antips_numTotalAtc", "Com_Resto_numTotalAtc")
p_articulo_com <- p_articulo[, camposCom]
p_articulo_com$Com_Anal_numTotalAtc <- ifelse(is.na(p_articulo_com$Com_Anal_numTotalAtc),0,p_articulo_com$Com_Anal_numTotalAtc)
p_articulo_com$Com_Ansi_numTotalAtc <- ifelse(is.na(p_articulo_com$Com_Ansi_numTotalAtc),0,p_articulo_com$Com_Ansi_numTotalAtc)
p_articulo_com$Com_Antide_numTotalAtc <- ifelse(is.na(p_articulo_com$Com_Antide_numTotalAtc),0,p_articulo_com$Com_Antide_numTotalAtc)
p_articulo_com$Com_Antips_numTotalAtc <- ifelse(is.na(p_articulo_com$Com_Antips_numTotalAtc),0,p_articulo_com$Com_Antips_numTotalAtc)
p_articulo_com$Com_Resto_numTotalAtc <- ifelse(is.na(p_articulo_com$Com_Resto_numTotalAtc),0,p_articulo_com$Com_Resto_numTotalAtc)
p_articulo_com$AtcTotalAnyo <- p_articulo_com$Com_Anal_numTotalAtc + p_articulo_com$Com_Ansi_numTotalAtc +
                          p_articulo_com$Com_Antide_numTotalAtc + p_articulo_com$Com_Antips_numTotalAtc +
                          p_articulo_com$Com_Resto_numTotalAtc

camposHos <- c("Id", "Sexo", "FechaNacimiento", "CRG4_2012", "CRG_Analisis", "TipoDiabetes", 
            "ATC_2012_Hosp", "Hos_Anal_numTotalAtc", "Hos_Ansi_numTotalAtc", "Hos_Antide_numTotalAtc",
            "Hos_Antips_numTotalAtc", "Hos_Resto_numTotalAtc")
p_articulo_hos <- p_articulo[, camposHos]
p_articulo_hos$Hos_Anal_numTotalAtc <- ifelse(is.na(p_articulo_hos$Hos_Anal_numTotalAtc),0,p_articulo_hos$Hos_Anal_numTotalAtc)
p_articulo_hos$Hos_Ansi_numTotalAtc <- ifelse(is.na(p_articulo_hos$Hos_Ansi_numTotalAtc),0,p_articulo_hos$Hos_Ansi_numTotalAtc)
p_articulo_hos$Hos_Antide_numTotalAtc <- ifelse(is.na(p_articulo_hos$Hos_Antide_numTotalAtc),0,p_articulo_hos$Hos_Antide_numTotalAtc)
p_articulo_hos$Hos_Antips_numTotalAtc <- ifelse(is.na(p_articulo_hos$Hos_Antips_numTotalAtc),0,p_articulo_hos$Hos_Antips_numTotalAtc)
p_articulo_hos$Hos_Resto_numTotalAtc <- ifelse(is.na(p_articulo_hos$Hos_Resto_numTotalAtc),0,p_articulo_hos$Hos_Resto_numTotalAtc)
p_articulo_hos$AtcTotalAnyo <- p_articulo_hos$Hos_Anal_numTotalAtc + p_articulo_hos$Hos_Ansi_numTotalAtc +
                          p_articulo_hos$Hos_Antide_numTotalAtc + p_articulo_hos$Hos_Antips_numTotalAtc +
                          p_articulo_hos$Hos_Resto_numTotalAtc

rm(camposCom)
rm(camposHos)

# 9.- GRABAMOS EL FICHERO DE DATOS.

wd <- getwd()
setwd("/Users/zzddfge/Desktop/Compartida/Tesis/Datos/Articulo 2016")

#Todos los pacientes
write.table(p_articulo, file="pacientes_articulo.csv", quote=FALSE, col.names = TRUE, row.names = FALSE, sep=";")

#Pacientes con ATCs de comunidad
write.table(p_articulo_com, file="pacientes_articulo_com.csv", quote=FALSE, col.names = TRUE, row.names = FALSE, sep=";")

#Pacientes con ATCs de Hospital
write.table(p_articulo_hos, file="pacientes_articulo_hos.csv", quote=FALSE, col.names = TRUE, row.names = FALSE, sep=";")

setwd(wd)
rm(wd)
