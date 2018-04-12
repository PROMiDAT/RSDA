gplot <- function(data, color=NA, matrix.form=NA, border=FALSE, show.type = FALSE, reduce =FALSE, fill=NA,font.size = 1, background.color = "white",plot = TRUE){
  if(!("sym.data.table" %in% class(data))){ #El tipo de dato no es el correcto
    stop("The data type is wrong, only sym.data.table are accepted")
  }

  # Si se tiene matrix.form, el numero de espacios tiene que ser igual o superior al de variables
  if(any(!is.na(matrix.form)) ){ #Si matrix.form no es NA
    if(!is.vector(matrix.form) || length(matrix.form)!=2) #Si no es un vector o si es de tamaño diferente de 2
      stop("Wrong format on matrix.form")
    if(prod(matrix.form) < data$M * data$N) # Si el tamaño de matrix.form no contiene todos los datos
      stop("Wrong dimensions on matrix.form")
  }else{ #Si no hay matriz se crea una, segun la orientacion de la fila
    matrix.form <- c(data$N,data$M)
  }

  if(any(is.na(color))) #Si no se ingresaron colores para el borde
    color <- c("black")

  if(any(is.na(fill))) # No se ingresaron colores
    fill <- distinctColorPalette(max(data$sym.var.length)) #Cantidad de colores correspondiente a la cantidad maxima de variables

  size.factor <- 1.75 #Determina un tamaño por defecto de los graficos (proporcion agregada)
  par(pin = (par()$din/(rep(max(matrix.form),2)*size.factor)))


  # limit  <- data$M    #El recorrido
  #Si se intenta imprimir una columna
  # if(byColm <- data$N > 1 && data$M ==1){ # En forma de columna
  #   limit <- data$N   #Para recorrer se usa el numero de filas
  #   height <- unit(1/data$N, "npc")
  #   width  <- unit(.8/data$N, "npc")
  # }else{
  #   height <- unit(2/data$M, "npc")
  #   width  <- unit(1/data$M, "npc")
  # }
  #
  # if(length(matrix.form)==1){ #Si no hay matrix.form se crea una
  #   matrix.form <- c(data$N,data$M)
  # }else{
  #   height <- width <- NULL
  # }

  size.text <- 1 - 0.05*prod(matrix.form)
  size.text <- (ifelse(size.text < 0.35,0.35,size.text))*font.size

  grid.newpage()#nueva pagina

  layout <- matrix(1:prod(matrix.form),nrow = matrix.form[1], matrix.form[2], byrow = T) #se crean los campos del grafico
  # if(is.null(height) && is.null(width))
  #pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
  fun.ajuste <- if(matrix.form[1] == 1) mean else max
  pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), heights = unit(1/fun.ajuste(matrix.form),"native"), widths = unit(1/max(matrix.form),"native"))))
  # else
  #   pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout), heights = height, widths = width)))
  graphics.results <- list()
  for (index.row in 1:data$N) {#Grafica las variables
    # se obtiene por fila o por columna segun byColm
    for(index.col in 1:data$M){
      var.data <- data[index.row,index.col] #else  data[,index]
      pos <- as.data.frame(which(layout == ((index.row-1)*data$M) + index.col, arr.ind = TRUE)) #obtiene la posicion en viewport
      switch (var.data$sym.var.types,
              "$I" = graphics.results[[length(graphics.results)+1]] <- view.gplot(sym.interval.gplot(var.data, color, border, show.type,fill, size.text, background.color), pos$row, pos$col, plot),
              "$C" = graphics.results[[length(graphics.results)+1]] <- view.gplot(sym.continuos.gplot(var.data, color, border, show.type, size.text, background.color), pos$row,pos$col,plot),
              "$H" = graphics.results[[length(graphics.results)+1]] <- view.gplot(sym.hist.gplot(var.data, color, border, show.type,fill,size.text, background.color), pos$row,pos$col,plot),
              "$M" = graphics.results[[length(graphics.results)+1]] <- view.gplot(sym.modal.gplot(var.data, color, border, show.type, reduce, fill,size.text, background.color), pos$row,pos$col,plot),
              "$S" = graphics.results[[length(graphics.results)+1]] <- view.gplot(sym.set.gplot(var.data, color, border, show.type, reduce, fill, background.color), pos$row,pos$col,plot)
      )
    }
  }
  if(plot != TRUE)
    return(graphics.results)
}
