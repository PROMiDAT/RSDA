#' Function for plotting one "modal" type value from the symbolic data table
#'
#' @author Andres Navarro
#' @param info The information of one "modal" type value. Use data[num.r,num.col] to get info
#' @param col A specification for the default plotting color.
#' @param border A logical value indicating whether border should be plotted.
#' @param ylab A logical value indicating whether the label of y axis has to be plotted.
#' @param show.type A logical value indicating whether type should be plotted.
#' @param reduce A logical value indicating whether values different from zero should be plotted in modal and set graphics.
#'
#' @return A plot of one "modal" type value from the symbolic data table.
#' @export
#'
#' @examples
#' \dontrun{
#' data(ex1_db2so)
#' data.sym <- classic.to.sym(ex1_db2so, concept=c("state", "sex"),
#'                            variables=c("county", "group", "age","age","age","age"),
#'                            variables.types=c("$I", "$C", "$C", "$S", "$M","$H"))
#' sym.modal.plot(data.sym[1,5])
#' sym.modal.plot(data.sym[1,5], reduce = TRUE)
#' }
sym.modal.plot <- function(info,col=c("blue"),border=FALSE,ylab=TRUE,show.type = TRUE,reduce=FALSE){
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
  barplot(mt, main=paste(info$sym.var.names,ifelse(show.type," (Modal)","")), xlab="", ylab= ifelse(ylab,"% Percentage",""),
          col = col,beside=TRUE, names.arg=names,ylim = c(0,1),cex.names=.8)
  if(border)
    box("figure", col="black")
}
