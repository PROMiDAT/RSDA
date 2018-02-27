#' sym.hist.gplot
#' @keywords internal
sym.hist.gplot <- function(info,color=c("black"), border=FALSE, show.type = TRUE, fill = "white", background.color = "gray"){
  if(info$sym.var.types != "$H") #Si el tipo no es el correcto
    stop("The data type is wrong, only $H are accepted")

  dataset <- info$data        #Los datos de la tabla simbolica (solo hist)
  names <- colnames(dataset)  #Los nombres de las variables
  matches <- regmatches(names, gregexpr("[[:digit:]]+", names)) #Descompone los nombre en numeros (en intervalos)

  dataTemp <- c() #tabla de datos
  for (index in 1:length(matches)) { #se recorren los intervalos (se dan en el mismo orden y cantidad que las variables)
    interval <- as.numeric(matches[[index]]) ##se comvierten en vectores numericos
    if(dataset[1,index]!=0) #Si esa variable no esta en cero, se representa en los datos temporales
      dataTemp <- c(dataTemp, runif(100*dataset[1,index], interval[1], interval[2]))# se crean numeros aleactorios entre el intervalo de la variable,
    #segun el porcentaje que tenga la variable
  }
  breaks <- unique(as.numeric(unlist(matches))) # se crean los intervalos para el grafico

  if(length(fill) != 1) #Si fill no es de tamaño 1
    if(length(info$data) != length(fill)) #Si el tamaño es diferente al numero de variables
      fill <- rep(fill,length(info$data))[1:length(info$data)] #Repite fill hasta tener la misma cantidad de datos

  if(length(color) != 1) #Si color no es de tamaño 1
    if(length(info$data) != length(color)) #Si el tamaño es diferente al numero de variables
      color <- rep(color,length(info$data))[1:length(info$data)] #Repite color hasta tener la misma cantidad de datos

  ggplot(data.frame(dt = dataTemp), aes(x=dt)) +
    geom_histogram(bins = length(breaks), color=color, fill=fill, breaks=breaks) +
    scale_x_continuous(limits = c(min(breaks),max(breaks))+c(-0.1,0.1),breaks = breaks) + #Limites y breaks (interalos) del eje x
    scale_y_continuous(limits = c(0,100), labels = scales::percent) + #Limites del eje y
    ggtitle(paste(info$sym.var.names,ifelse(show.type," (Histogram)",""))) + #Titulo del plot
    theme(plot.title = element_text(hjust = 0.5, color = color), #Configuracion del titulo
          axis.title.x=element_blank(), #Quita elementos de los ejes
          axis.title.y=element_blank(),  #Quita elementos de los ejes
          panel.border = element_rect(linetype = "dashed", fill = NA, color = color),#Borde del plot
          plot.background = element_rect(fill = background.color,size = ifelse(border,1,0),color="black"),#Borde de la figura
          panel.background = element_rect(fill = background.color, colour = NA), #color del fondo
          panel.grid.major = element_blank(), #elimina las lineas del fondo
          panel.grid.minor = element_blank(), #elimina las lineas del fondo
          axis.text = element_text(colour = color), #Color del texto
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 1)) #rota los labels del eje y
}
