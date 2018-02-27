#' sym.interval.gplot
#' @keywords internal
sym.interval.gplot <- function(info, color=c("black"), border=FALSE, show.type = TRUE, fill="white", font.size = 1, background.color = "gray"){
  if(info$sym.var.types != "$I")#Si el tipo no es el correcto
    stop("The data type is wrong, only $I are accepted")

  if( length(fill) > 1 ) #El color del rectangulo tiene que ser de tamaño 1
    fill <- fill[1]

  if( length(color) > 1 ) #El color de los bordes tienen que ser de tamaño 1
    color <- color[1]

  d <- data.frame(x1 = as.numeric(info$data[1]), #El primer valor del intervalo
                  x2 = as.numeric(info$data[2]), #El segundo valor del intervalo
                  y1=0, y2=4,                    #Valores del rectangulo
                  r=paste0("[", as.character(info$data[1]),",", as.character(info$data[2]),"]")) #Leyenda dentro del rectangulo
  ggplot() +
    ggtitle(paste(info$sym.var.names,ifelse(show.type," (Interval)",""))) + #Titulo
    scale_y_continuous(limits = c(0,4.2),expand = c(0,0)) + #Valores del eje y
    geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), color=color, alpha=0.95,fill=fill)+ #Rectangulo que representa el intervalo
    geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=10*font.size) + #Dibuja la leyenda dentro del rectangulo
    theme(plot.title = element_text(hjust = 0.5,color = color), #Titulo del plot
          axis.title.x=element_blank(),  #Quita elementos de los ejes
          axis.title.y=element_blank(),  #Quita elementos de los ejes
          axis.ticks.y =element_blank(), #Quita elementos de los ejes
          axis.text.y = element_blank(), #Quita elementos de los ejes
          panel.border = element_rect(linetype = "dashed", fill = NA,color = color), #Borde del plot
          plot.background = element_rect(fill = NA,size = ifelse(border,1,0),color="black"), #Borde de la figura
          panel.background = element_rect(fill = background.color, colour = NA), #color del fondo
          panel.grid.major = element_blank(), #elimina las lineas del fondo
          panel.grid.minor = element_blank(),#elimina las lineas del fondo
          axis.text = element_text(colour = color), #Color del texto
          legend.position="none") #Quita el legend
}
