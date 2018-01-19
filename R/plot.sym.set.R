#' Title
#'
#' @param info
#' @param col
#' @param border
#' @param show.type
#' @param reduce
#'
#' @return
#' @export
#'
#' @examples
plot.sym.set <- function(info,col=c("blue"),border=FALSE,show.type = TRUE,reduce=FALSE){
  mt <- info$data
  names <- colnames(info$data)
  if(reduce){
    select <- colSums(mt) != 0
    mt <- cbind(mt[,select],0)
    names <- c(names[select],"...")
    colnames(mt)<-names
  }
  mt <- as.matrix(mt)
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Set)","")), xlab="", ylab="",
          names.arg=names,ylim = c(0,1), beside=TRUE, col=col,cex.names=.8)
  if(border)
    box("figure", col="black")
}
