#' Title
#'
#' @param info
#' @param col
#' @param border
#' @param show.type
#'
#' @return
#' @export
#'
#' @examples
sym.continuos.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE){
  continuos <- as.numeric(info$data)
  plot(continuos+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Continuos)","")),yaxt='n')
  abline(v=continuos, col=col, lty=2, lwd=2)
  size.font <- ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)
  text(continuos, 2, labels = as.character(continuos),cex=size.font)
  if(border)
    box("figure", col="black")
}
