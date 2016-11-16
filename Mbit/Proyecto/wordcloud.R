library(tm)
library(RColorBrewer)
library(wordcloud)
library(sqldf)


setwd("/Users/zzddfge/Desktop/Compartida/")
pal2 <- brewer.pal(7,"Set2")
personas <- as.data.frame(read.csv("personas.csv"))
words <- sqldf("Select palabra, count(*) from personas group by palabra")
colnames(words) <- c("palabra", "cuenta")
png("mentores.png", width=1280,height=800)
wordcloud(words$palabra, words$cuenta, scale=c(4,.1), min.freq = 1,
                max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()
