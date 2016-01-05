library("plotly")

plot_ly(z = volcano, type = "surface")


informe <- function(compania) {
  
  cat("# Anális de: ")  #La # implica hacer un título en smd
  cat(compania)
  cat("\n\n")   #Dos intros porque es para saltar de párrafos. 
}