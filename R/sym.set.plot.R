#' sym.set.plot
#' @keywords internal
sym.set.plot <- function(info,col=c("blue"),border=FALSE,show.type = TRUE,reduce=FALSE){
  if(info$sym.var.types != "$S")
    stop("The data type is wrong, only $S are accepted")
  mt <- info$data
  mt[,mt > 0] <- 1/sum(mt > 0)
  names <- colnames(info$data)
  if(reduce){ #Si el modo reduce esta activado
    if(any(mt==0)){#Si alguna de las columnas tiene cero
      mt <- cbind(mt[,select <- colSums(mt)!=0],0)#Se crea "select" (las columnas con valores mayores a cero),
      #se seleccionan los valores mayores a cero y
      #se les agraga una columna extra en cero(representativa de los valores en cero)
      names <- c(names[select],"...")#Se seleccionan los nombres de columnas con valores distintos de cero y
      #se crea el nombre de la columna representativa
    }
  }
  mt <- as.matrix(mt)
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Set)","")), xlab="", ylab="",
          names.arg=names,ylim = c(0,ifelse(max(mt)<0.5,0.5,1)), beside=TRUE, col=col,cex.names=.8)
  if(border)
    box("figure", col="black")
}
