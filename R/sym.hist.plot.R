#' sym.hist.plot
#' @keywords internal
sym.hist.plot <- function(info,col=c("blue"),border=FALSE, ylab=TRUE,show.type = TRUE){
  if(info$sym.var.types != "$H")
    stop("The data type is wrong, only $H are accepted")
  dataset <- info$data
  namesC <- colnames(dataset)
  matches <- regmatches(namesC, gregexpr("[[:digit:]]+", namesC))
  dataTemp <- c()
  for (index in 1:length(matches)) {
    interval <- as.numeric(matches[[index]])
    if(dataset[1,index]!=0)
      dataTemp <- c(dataTemp, runif(100*dataset[1,index], interval[1], interval[2]))
  }

  breaks <- unique(as.numeric(unlist(matches)))
  hist(dataTemp, breaks = breaks,
       freq = T, xlim = c(min(breaks),max(breaks))+c(-1,1),ylim = c(0,100),
       border = "black",col = col,
       ylab = ifelse(ylab,"% Percentage",""),xlab = "",
       main = paste(info$sym.var.names,ifelse(show.type," (Histogram)","")))
  if(border)
    box("figure", col="black")
}
