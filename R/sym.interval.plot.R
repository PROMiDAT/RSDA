#' sym.interval.plot
#' @keywords internal
sym.interval.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE){
  if(info$sym.var.types != "$I")
    stop("The data type is wrong, only $I are accepted")
  interval <- as.numeric(info$data[1,])
  name <- paste("[",interval[1],",",interval[2],"]")
  plot(interval+c(-0.5,0.5), c(0,4.1), type= "n", xlab = "", ylab = "",main = paste(info$sym.var.names,ifelse(show.type," (Interval)","")),yaxt='n')
  rect(interval[1],-1,interval[2],3.5,col=col)
  center <- c(mean(c(interval[1], interval[2])), mean(c(-1, 3.5)))
  size.font <- ifelse(par()$pin[1]<=1.5,par()$pin[1],1.5)
  text(center[1], center[2], labels = name,cex=size.font)
  if(border)
    box("figure", col="black")
}
