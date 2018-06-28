#' sym.hist.plot
#' @keywords internal
sym.hist.plot <- function(info, col=c("blue"), border=FALSE, show.type = TRUE, angle = 60){
  if(info$sym.var.types != "$H")#El tipo de dato es el incorrecto
    stop("The data type is wrong, only $H are accepted")

  dataset <- as.matrix(info$data) #obtenemos los datos
  namesC <- colnames(dataset) #obtenemos los nombres
  matches <- regmatches(namesC, gregexpr("0|([[:digit:]]+\\.?[[:digit:]]+)", namesC)) #obtenemos los numeros de los nombres
  breaks <- as.numeric(unlist(matches))
  i <- seq(1,length(breaks),2)
  j <- seq(2,length(breaks),2)
  bins <- gsub("\\s","",paste(format(breaks[i], scientific=F, format="f", digits=2),format(breaks[j], scientific=F,format="f", digits=2),sep = ","))
  names <- c(paste0("[",bins[-length(bins)],")"),paste0("[",bins[length(bins)],"]"))

  plt <- barplot(dataset,ylim = c(0,1), names.arg = names, col = col,
                 yaxt="n",xaxt="n",xlab = "", ylab="",
                 main = paste(info$sym.var.names,ifelse(show.type," (Histogram)","")))
  graphics::axis(2, at=seq(0, 1, 0.2), labels=sprintf(round(seq(0, 100, 20)), fmt="%2.f%%"), las=1)
  text(plt, par("usr")[3], labels = names, srt = angle, adj = 1.1, xpd = T)

  if(border) #se pone el borde en negro
    box("figure", col="black")
}
