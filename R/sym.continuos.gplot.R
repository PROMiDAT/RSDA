#' sym.continuos.gplot
#' @keywords internal
sym.continuos.gplot <- function(info, color=c("black"), border=FALSE, show.type = TRUE, font.size = 1, background.color = "gray"){
  if(info$sym.var.types != "$C") #Si el tipo no es el correcto
    stop("The data type is wrong, only $C are accepted")

  if( length(color) > 1 ) #El color de los bordes tienen que ser de tamaño 1
    color <- color[1]

  continuos <- as.numeric(info$data) #El valor continuo
  ggplot()  +
    geom_vline(xintercept = continuos, linetype="dashed", color = color)+ # La linea vertical en el eje x
    geom_text(aes(continuos,2,label = continuos), size=8*font.size) + # El numero continuo como leyenda
    ggtitle(paste(toupper(info$sym.var.names),ifelse(show.type," (Continuos)",""))) + # Titulo del plot
    theme(plot.title = element_text(hjust = 0.5,color = color),#Configuracion del titulo
          axis.title.x=element_blank(),#Quita elementos de los ejes
          axis.title.y=element_blank(),#Quita elementos de los ejes
          axis.ticks.y =element_blank(), #Quita elementos de los ejes
          axis.text.y = element_blank(), #Quita elementos de los ejes
          panel.border=element_rect(linetype = "dashed", fill = NA, colour = color), #Borde del plot
          plot.background = element_rect(fill = NA,size = ifelse(border,1,0),color="black"), #Borde de la figura
          panel.background = element_rect(fill = background.color, colour = NA), #color del fondo
          panel.grid.major = element_blank(), #elimina las lineas del fondo
          panel.grid.minor = element_blank(), #elimina las lineas del fondo
          axis.text = element_text(colour = color)) #Color del texto
}
