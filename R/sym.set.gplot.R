#' sym.set.gplot
#' @keywords internal
sym.set.gplot <- function(info, color=c("black"), border=FALSE, show.type = TRUE, reduce=FALSE, fill = "white",font.size = 1,background.color = "gray"){
  if(info$sym.var.types != "$S")  #Si el tipo no es el correcto
    stop("The data type is wrong, only $S are accepted")

  mt <- as.numeric(info$data) #Los datos de la tabla simbolica (solo set)
  values <- 1/sum(mt > 0) #Se arreglan los valores del set
  mt[mt > 0] <- values #Se arreglan los valores del set
  names <- colnames(info$data) #Los nombres de las variables

  if(reduce){ #Si el modo reduce esta activado
    if(any(mt==0)){#Si alguna de las columnas tiene cero
      mt <- mt[select <- mt != 0] #Se crea "select" (las columnas con valores mayores a cero),
      #se seleccionan los valores mayores a cero y
      mt[length(mt)+1] <- 0 #se les agraga una columna extra en cero(representativa de los valores en cero)
      names <- c(names[select],"...")#Se seleccionan los nombres de columnas con valores distintos de cero y
      #se crea el nombre de la columna representativa
      fill <- fill[select] # selecciona los colores corespondientes
    }
  }

  if(length(fill) != 1) #Si fill no es de tamaño 1
    if(length(mt) != length(fill)) #Si el tamaño es diferente al numero de variables
      fill <- rep(fill,length(mt))[1:length(mt)] #Repite fill hasta tener la misma cantidad de datos

  if(length(color) != 1) #Si color no es de tamaño 1
    if(length(mt) != length(color)) #Si el tamaño es diferente al numero de variables
      color <- rep(color,length(mt))[1:length(mt)] #Repite color hasta tener la misma cantidad de datos

  ggplot(data = data.frame(x=names,y=mt), aes(x,y))  +
    geom_bar(stat="identity", color=color, fill=fill) + #Crea las barras
    ggtitle(paste(toupper(info$sym.var.names),ifelse(show.type," (Set)",""))) + #Titulo del plot
    scale_y_continuous(limits = c(0,values), labels = scales::percent)+ #Limites del eje y (se reduce a 0.5 para que se vean mejor los datos)
    theme(plot.title = element_text(hjust = 0.5, colour = color), #Configuracion del titulo
          axis.title.x=element_blank(),#Quita elementos de los ejes
          axis.title.y=element_blank(),#Quita elementos de los ejes
          panel.border = element_rect(fill = NA, color = color), #Borde del plot
          plot.background = element_rect(fill = NA,size = ifelse(border,1,0),color="black"), #Borde de la figura
          panel.background = element_rect(fill = background.color, colour = NA), #color del fondo
          panel.grid.major = element_blank(), #elimina las lineas del fondo
          panel.grid.minor = element_blank(),#elimina las lineas del fondo
          axis.text = element_text(colour = color), #Color del texto
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 1, size = 10*font.size)) #rota los labels del eje y
}
