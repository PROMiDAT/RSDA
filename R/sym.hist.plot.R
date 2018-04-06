#' sym.hist.plot
#' @keywords internal
sym.hist.plot <- function(info, col=c("blue"), border=FALSE, show.type = TRUE){
  if(info$sym.var.types != "$H")#El tipo de dato es el incorrecto
    stop("The data type is wrong, only $H are accepted")

  dataset <- as.matrix(info$data) #obtenemos los datos
  # namesC <- colnames(dataset) #obtenemos los nombres
  # matches <- regmatches(namesC, gregexpr("[[:digit:]]+", namesC)) #obtenemos los numeros de los nombres
  # dataTemp <- c() #creamos array temp

  # for (index in 1:length(matches)) { #genera un array representativo
  #   interval <- as.numeric(matches[[index]])
  #   if(dataset[1,index]!=0)
  #     dataTemp <- c(dataTemp, runif(100*dataset[1,index], interval[1], interval[2]))
  # }

  # breaks <- unique(as.numeric(unlist(matches))) # los breaks son los nombres sin repeticiones

  #grafica el histograma
  # hist(dataTemp, breaks = breaks,
  #      freq = T, xlim = c(min(breaks), max(breaks))+c(-1,1),ylim = c(0,100),
  #      border = "black",col = col, yaxt="n",xlab = "", ylab="",
  #      main = paste(info$sym.var.names,ifelse(show.type," (Histogram)","")))
  # graphics::axis(2, at=seq(0, 100, 20), labels=sprintf(round(seq(0, 100, 20)), fmt="%2.f%%"))#los y labels con %

  plt <- barplot(dataset,ylim = c(0,1), names.arg = colnames(dataset), col = col,
                 yaxt="n",xaxt="n",xlab = "", ylab="",
                 main = paste(info$sym.var.names,ifelse(show.type," (Histogram)","")))
  graphics::axis(2, at=seq(0, 1, 0.2), labels=sprintf(round(seq(0, 100, 20)), fmt="%2.f%%"), las=1)
  text(plt, par("usr")[3], labels = colnames(dataset), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.7)

  if(border) #se pone el borde en negro
    box("figure", col="black")
}
