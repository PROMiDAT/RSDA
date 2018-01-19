#' Title
#'
#' @param x
#' @param col
#' @param border
#' @param size
#' @param title
#' @param show.type
#' @param reduce
#'
#' @return
#' @keywords Plot Symbolic data table
#' @export
#' @exportMethod
#'
plot.sym.data.table <- function(x, col=NA,border=FALSE,size = 1,title=TRUE,show.type = FALSE,reduce =FALSE,...){
  #El tipo de dato es el correcto
  if(!("sym.data.table" %in% class(x))){
    stop("The data type is wrong, only sym.data.table are accepted")
  }

  # No se ingresaron colores
  if(any(is.na(col)))
    col <- brewer.pal(n = 12, name = "Set3")

  #obtener la cantidad de variables
  num.col  <- x$M

  #obtener la cantidad de individuos
  num.row  <- x$N

  matrix.form <- c(num.row,num.col)
  def.par <- par(no.readonly = TRUE)

  par(mfrow = matrix.form)
  size.factor <- ifelse(is.numeric(size),1.75*(1/size),1.75)
  par(mar=c(0,0,1,0))
  par(pin = (par()$din/(rep(max(matrix.form),2)*size.factor)) )
  par(omi = c(0,0,0.2,0))


  #Grafica las variables
  for (index.row in 1:num.row) {
    for (index.col in 1:num.col) {
      var.data <- x[index.row,index.col]
      switch (var.data$sym.var.types, "$I" = plot.sym.interval(var.data,col,border,show.type=show.type),
              "$C" = plot.sym.continuos(var.data,col,border,show.type=show.type),
              "$H" = plot.sym.hist(var.data,col,border,FALSE,show.type=show.type),
              "$M" = plot.sym.modal(var.data,col,border,FALSE,show.type=show.type,reduce =reduce),
              "$S" = plot.sym.set(var.data,col,border,show.type=show.type,reduce =reduce))
    }
  }

  if(title)
    mtext(paste("Symbolic Data Table:",num.row," x ",num.col),cex = 1.5, outer = TRUE,side=3)
  par(def.par)
}
