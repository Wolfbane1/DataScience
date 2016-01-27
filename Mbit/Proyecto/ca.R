
#Carga de librerías.
suppressPackageStartupMessages(library(ca))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(vcd))

par(mfrow=c(1,2), mar=c(4,4,3,3))

# read data from choosen table
mydata <- as.data.frame(m)

# get some details about the input table
grandtotal <- sum(mydata)
nrows <- nrow(mydata)
ncols <- ncol(mydata)
numb.dim.cols<-ncol(mydata)-1
numb.dim.rows<-nrow(mydata)-1
a <- min(numb.dim.cols, numb.dim.rows) #dimensionality of the table
labs<-c(1:a) #set the numbers that will be used as x-axis' labels on the Malinvaud's test scatterplot

# contingency table as matrix
mydataasmatrix<-as.matrix(mydata)

# contingency table w/ row and columns profiles
data.w.rowsum<-addmargins(mydataasmatrix,1)
data.w.colsum<-addmargins(mydataasmatrix,2)

# Number of dimensions according to the average rule
c.dim<-round(100/(ncols-1), digits=1)
r.dim<-round(100/(nrows-1), digits=1)
thresh.sig.dim<-(max(c.dim, r.dim))
dataframe.after.ca<- summary(ca(mydata))
n.dim.average.rule <- length(which(dataframe.after.ca$scree[,3]>=thresh.sig.dim))

# Malinvaud's Test
malinv.ca<-CA(mydata, ncp=a, graph=FALSE)
malinv.test.rows <- a
malinv.test.cols <- 6
malinvt.output <-matrix(ncol= malinv.test.cols, nrow=malinv.test.rows)
colnames(malinvt.output) <- c("K", "Dimension", "Eigen value", "Chi-square", "df", "p value")
malinvt.output[,1] <- c(0:(a-1))
malinvt.output[,2] <- c(1:a)
for(i in 1:malinv.test.rows){
  k <- -1+i
  malinvt.output[i,3] <- malinv.ca$eig[i,1]
  malinvt.output[i,5] <- (nrows-k-1)*(ncols-k-1)
}
malinvt.output[,4] <- rev(cumsum(rev(malinvt.output[,3])))*grandtotal
malinvt.output[,6] <- round(pchisq(malinvt.output[,4], malinvt.output[,5], lower.tail=FALSE), digits=6)
optimal.dimensionality <- length(which(malinvt.output[,6]<=0.05))

# plot bar chart of correlation between rows and columns, and add reference line
perf.corr<-(1.0)
sqr.trace<-round(sqrt(sum(dataframe.after.ca$scree[,2])), digits=3)
barplot(c(perf.corr, sqr.trace), main="Correlation coefficient between rows & columns (=square root of the inertia)", sub="reference line: threshold of important correlation ", ylab="correlation coeff.", names.arg=c("correlation coeff. range", "correlation coeff. bt rows & cols"), cex.main=0.80, cex.sub=0.80, cex.lab=0.80)
abline(h=0.20)

# plot bar chart of inertia explained by the dimensions, and add reference line corresponding to the Average Rule threshold
barplot(dataframe.after.ca$scree[,3], xlab="Dimensions", ylab="% of Inertia", names.arg=dataframe.after.ca$scree[,1])
abline(h=thresh.sig.dim)
title (main="Percentage of inertia explained by the dimensions", sub="reference line: threshold of an optimal dimensionality of the solution, according to the average rule (see also the Malinvaud's test Plot)", cex.main=0.80, cex.sub=0.80)

# Malinvaud's test Plot
plot(malinvt.output[,6], , xaxt="n", xlim=c(1, a), xlab="Dimensions", ylab="p value")
axis(1, at=labs, labels=sprintf("%.0f",labs))
title(main="Malinvaud's test Plot", sub="dashed line: alpha 0.05 threshold", col.sub="RED", cex.sub=0.80)
abline(h=0.05, lty=2, col="RED")

# prompt to the user and selection of the number of dimensions to be plotted
cat(paste("-The input Table has", nrows, "Rows and", ncols, "Columns"))
cat(" \n")
cat(paste("-The Correlation Coefficient btw Rows and Columns is", sqr.trace))
cat(" \n")
cat(paste("-The total number of dimensions is", a))
cat(" \n")
cat(paste("-The Average Rule indicates that the number of relevant dimensions is", n.dim.average.rule))
cat(" \n")
cat(paste("-The Malinvaud's Test indicates that the number of relevant dimensions is", optimal.dimensionality))
cat(" \n")
cat("-The Malinvaud's Test details are the following:")
cat(" \n")
print(malinvt.output)
user.dimensionality <- as.numeric(readline(prompt="How many dimensions do you want to analyze (min=2)? "))
dims.to.be.plotted <- user.dimensionality 

# CA analysis by Greenacre's package to be used later on for the Standard Biplots
res.ca <- ca(mydata, nd=dims.to.be.plotted)

# CA output as dataframe to be used for the some graphs to come
cadataframe<-summary(ca(mydata, nd=dims.to.be.plotted))

# plot the quality of the display of categories on successive pairs of dimensions
#row categories
counter <- 1
for(i in seq(9, ncol(cadataframe$rows), 3)){    
  counter <- counter +1
  quality.rows <- (cadataframe$rows[,6]+cadataframe$rows[,i])/10
  barplot(quality.rows, ylim=c(0,100), xlab="Row categories", ylab=paste("Quality of the display (% of inertia) on Dim. 1+", counter), names.arg=cadataframe$rows[,1], cex.lab=0.80)
}

#column categories
counter <- 1
for(i in seq(9, ncol(cadataframe$columns), 3)){    
  counter <- counter +1
  quality.cols <- (cadataframe$columns[,6]+cadataframe$columns[,i])/10
  barplot(quality.cols, ylim=c(0,100), xlab="Column categories", ylab=paste("Quality of the display (% of inertia) on Dim. 1+", counter), names.arg=cadataframe$columns[,1], cex.lab=0.80)
}

# charts of categories contribution
# plot bar charts of contribution of row categories to the axes, and add a reference line
counter <- 0
for(i in seq(7, ncol(cadataframe$rows), 3)){    
  counter <- counter +1
  barplot(cadataframe$rows[,i], ylim=c(0,1000), xlab="Row categories", ylab=paste("Contribution to Dim. ",counter," (in permills)"), names.arg=cadataframe$rows[,1], cex.lab=0.80)
  abline(h=round(((100/nrows)*10), digits=0))
}

# plot bar charts of contribution of column categories to the axes, and add a reference line
counter <- 0
for(i in seq(7, ncol(cadataframe$columns), 3)){    
  counter <- counter +1
  barplot(cadataframe$columns[,i], ylim=c(0,1000), xlab="Column categories", ylab=paste("Contribution to Dim. ",counter," (in permills)"), names.arg=cadataframe$columns[,1], cex.lab=0.80)
  abline(h=round(((100/ncols)*10), digits=0))
}

# correlation of categories to dimensions
# row categories
counter <- 0
for(i in seq(6, ncol(cadataframe$rows), 3)){    
  counter <- counter +1
  correl.rows <- round(sqrt((cadataframe$rows[,i]/1000)), digits=3)
  barplot(correl.rows, ylim=c(0,1), xlab="Row categories", ylab=paste("Correlation with Dim. ", counter), names.arg=cadataframe$rows[,1], cex.lab=0.80)
}

#column categories
counter <- 0
for(i in seq(6, ncol(cadataframe$columns), 3)){    
  counter <- counter +1
  correl.cols <- round(sqrt((cadataframe$columns[,i]/1000)), digits=3)
  barplot(correl.cols, ylim=c(0,1), xlab="Column categories", ylab=paste("Correlation with Dim. ", counter), names.arg=cadataframe$columns[,1], cex.lab=0.80)
}

## CA graphical outputs ##:
# symmetric plots from FactoMineR package
par(mfrow=c(1,3), mar=c(4,4,2,2))
counter <- 1
for(i in 2:dims.to.be.plotted){    
  counter <- counter +1
  plot(malinv.ca, axes=c(1,i), shadow=TRUE, cex=0.80, invisible="none", title = paste("Correspondence Analysis-symmetric map: Dim. 1 +", counter), cex.main=0.8)
  plot(malinv.ca, axes=c(1,i), shadow=TRUE, cex=0.80, invisible="col", title = paste("Correspondence Analysis-symmetric rows map: Dim. 1 +", counter), cex.main=0.8)
  plot(malinv.ca, axes=c(1,i), shadow=TRUE, cex=0.80, invisible="row", title = paste("Correspondence Analysis-symmetric cols map: Dim. 1 +", counter), cex.main=0.8)
}

# asymmetric biplots (Standard Biplots) from Greenacre's package: rows in principal coordinates and columns in standard coordinates times square root of the mass (Greenacre 2007, pp. 102, 234, 268, 270). NOTE: The lenght of each arrow joining the column points to the origin is proportional to the contribution that each column category makes to the principal axes; colour intensity proportional to the absolute contribution to the total inertia
dev.new()
counter <- 1
for(i in 2:dims.to.be.plotted){    
  counter <- counter +1
  plot(res.ca, mass = FALSE, dim=c(1,i), contrib = "none", col=c("black", "red"), map ="rowgreen", arrows = c(FALSE, TRUE), main = paste("Correspondence Analysis-standard biplot: Dim. 1 +", counter))
  plot(res.ca, mass = FALSE, dim=c(1,i), contrib = "none", col=c("black", "red"), map ="colgreen", arrows = c(TRUE, FALSE), main = paste("Correspondence Analysis-standard biplot: Dim. 1 +", counter))
  plot(res.ca, mass = FALSE, dim=c(1,i), contrib = "absolute", col=c("black", "red"), map ="rowgreen", arrows = c(FALSE, TRUE), main = paste("Correspondence Analysis-standard biplot: Dim. 1 +", counter), sub="colour intensity proportional to the absolute contribution to the inertia", cex.sub=0.70)
  plot(res.ca, mass = FALSE, dim=c(1,i), contrib = "absolute", col=c("black", "red"), map ="colgreen", arrows = c(TRUE, FALSE), main = paste("Correspondence Analysis-standard biplot: Dim. 1 +", counter), sub="colour intensity proportional to the absolute contribution to the inertia", cex.sub=0.70)
}

## clustering after FactoMiner package:
ca.factom <- CA(mydata, ncp=dims.to.be.plotted, graph= FALSE)
resclust.rows<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="rows")
resclust.cols<-HCPC(ca.factom, nb.clust=-1, metric="euclidean", method="ward", order=TRUE, graph.scale="inertia", graph=FALSE, cluster.CA="columns")


rm(ncols)
rm(nrows)
rm(numb.dim.rows)
rm(numb.dim.cols)
rm(optimal.dimensionality)
rm(perf.corr)
rm(quality.cols)
rm(quality.rows)
rm(r.dim)
rm(res.ca)
rm(resclust.cols)
rm(user.dimensionality)
rm(thresh.sig.dim)
rm(sqr.trace)
rm(resclust.rows)
rm(n.dim.average.rule)
rm(malinvt.output)
rm(malinv.test.cols)
rm(malinv.test.rows)
rm(malinv.ca)
rm(labs)
rm(k)
rm(i)
rm(grandtotal)
rm(dims.to.be.plotted)
rm(dataframe.after.ca)
rm(counter)
rm(correl.cols)
rm(correl.rows)
rm(cadataframe)
rm(ca.factom)
rm(a)
rm(c.dim)
rm(mydata)
rm(mydataasmatrix)
rm(data.w.colsum)
rm(data.w.rowsum)

