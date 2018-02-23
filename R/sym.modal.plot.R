#' sym.modal.plot
#' @keywords internal
sym.modal.plot <- function(info,col=c("blue"),border=FALSE, show.type = TRUE, reduce=FALSE){
  if(info$sym.var.types != "$M")
    stop("The data type is wrong, only $M are accepted")
  mt <- info$data
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
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Modal)","")), xlab="", ylab= "", yaxt="n",
          col = col,beside=TRUE, names.arg=names,ylim = c(0,1),cex.names=.8)
  graphics::axis(2, at=seq(0, 1, 0.2), labels=sprintf(round(seq(0, 100, 20)), fmt="%2.f%%"), las=1)

  if(border)
    box("figure", col="black")
}
