#' sym.interval.plot
#' @keywords internal
sym.interval.plot <- function(info, col=c("blue"), border=FALSE, show.type = TRUE ){
  if(info$sym.var.types != "$I")#El tipo de dato es el incorrecto
    stop("The data type is wrong, only $I are accepted")

  interval <- as.numeric(info$data[1,]) #sacamos el intervalo
  name <- paste("[",interval[1],",",interval[2],"]") #El label que va en el centro del grafico

  #grafica el plano
  plot(interval+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Interval)","")), yaxt='n')
  rect(interval[1],-1,interval[2],3.5,col=col) #rectangulo del intervalo
  center <- c(mean(c(interval[1], interval[2])), mean(c(-1, 3.5))) #encuentra el centro del rectangulo
  text(center[1], center[2], labels = name,cex=ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)) #pone el label con el intevalo en el centro
  if(border) #se pone el borde en negro
    box("figure", col="black")
}
