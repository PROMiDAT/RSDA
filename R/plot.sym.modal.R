#' Title
#'
#' @param info
#' @param col
#' @param border
#' @param ylab
#' @param show.type
#' @param reduce
#'
#' @return
#' @export
#'
#' @examples
plot.sym.modal <- function(info,col=c("blue"),border=FALSE,ylab=TRUE,show.type = TRUE,reduce=FALSE){
  mt <- info$data
  names <- colnames(info$data)
  if(reduce){
    select <- colSums(mt) != 0
    mt <- cbind(mt[,select],0)
    names <- c(names[select],"...")
    colnames(mt)<-names
  }
  mt <- as.matrix(mt)
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Modal)","")), xlab="", ylab= ifelse(ylab,"% Percentage",""),
          col = col,beside=TRUE, names.arg=names,ylim = c(0,1),cex.names=.8)
  if(border)
    box("figure", col="black")
}
