
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
