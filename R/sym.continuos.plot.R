#' sym.continuos.plot
#' @keywords internal
sym.continuos.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE){
  if(info$sym.var.types != "$C")
    stop("The data type is wrong, only $C are accepted")
  continuos <- as.numeric(info$data)
  plot(continuos+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Continuos)","")),yaxt='n')
  abline(v=continuos, col=col, lty=2, lwd=2)
  size.font <- ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)
  text(continuos, 2, labels = as.character(continuos),cex=size.font)
  if(border)
    box("figure", col="black")
}
