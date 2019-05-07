library(RSDA)
library(ggplot2)
x <- classic_to_sym(x = diamonds,
                    concept = cut,
                    variables = c(cut,depth,clarity,carat,color),
                    depth = sym_histogram(depth),
                    clarity = sym_set(clarity))
x


to_rsdav2 <- function(x) {
  out <- list()
  out$concept = unname(attr(x,"concept"))
  out$N <- nrow(x)
  out$M <- ncol(x) - 1L
  out$sym.obj.names <- x$concept
  out$sym.var.names <- colnames(x[,-1])
  out$sym.var.types <- unname(sapply(x[,-1], function(x) reduce_class_name(class(x)[1])))
  out$sym.var.length <- unname(sapply(x[,-1], get_length))
  meta <- do.call("cbind",lapply(x[,-1], get_meta))
  rownames(meta) <- out$sym.obj.names
  out$sym.var.starts <- calculate_starts(meta)
  out$meta <- meta
  out$data <- do.call("cbind",lapply(x[,-1], get_data))
  class(out) <- "sym.data.table"
  return(out)
}

calculate_starts <- function(meta){
  colnames. <- colnames(meta)
  i <- which(stringr::str_detect(colnames., "\\$\\w{1}"))
  types <- na.omit(stringr::str_extract(colnames.,"\\$\\w{1}"))
  ifelse(types != "$I", i+2,i+1)
}

reduce_class_name <- function(x) {
  switch(x,
         symbolic_interval={
           return("$I")
         },
         symbolic_histogram = {
           return("$H")
         },
         symbolic_set = {
           return("$S")
         },
         symbolic_modal = {
           return("$M")
         },
         {
           print('default')
         }
  )
}

get_data <- function(x,...) UseMethod("get_data")
get_data.default <- function(x,...) return(x)

get_data.symbolic_interval <- function(x,...){
  data.frame(v1 =sapply(x, min), v2 =sapply(x, max))
}

get_data.symbolic_histogram<- function(x,...){
  breaks <- unlist(lapply(x, function(x)x$breaks))
  cuts <- hist(breaks,plot = F)$breaks
  c1 <- stats::na.omit(dplyr::lag(cuts))
  c2 <- stats::na.omit(dplyr::lead(cuts))
  cats <- paste0("(",scales::comma(c1, accuracy = 0.1)," : ",scales::comma(c2, accuracy = 0.1), "]")
  cats <- factor(cats,levels = cats,ordered = T)
  probs <- round(x[[1]]$f(c1,c2),2)

  out <- lapply(x,function(x) t(data.frame(x$f(c1,c2))))
  out <- do.call("rbind",out)
  rownames(out) <- NULL
  colnames(out) <- as.character(cats)
  return(out)
}

get_data.symbolic_set <- function(x,...){
  all_cats <- unique(do.call("c",x))
  out <- do.call("rbind",lapply(x, function(x) as.integer(all_cats %in% x)))
  colnames(out) <- all_cats
  out <- as.data.frame(out)
  return(out)
}

get_data.symbolic_modal <- function(x,...){
  cats <- lapply(x,function(x) prop.table(table(x)))
  cats <- do.call("rbind",cats)
  cats <- as.data.frame(cats)
  return(cats)
}

get_meta <- function(x,...) UseMethod("get_meta")
get_meta.default <- function(x,...) return(x)

get_meta.symbolic_interval <- function(x,...){
  data.frame("$I" = "$I", v1 =sapply(x, min), v2 =sapply(x, max),stringsAsFactors = F)
}

get_meta.symbolic_histogram <- function(x,...){
  .data <- get_data(x)
  out <- data.frame("$H" = "$H", v1 = ncol(.data),stringsAsFactors = F)
  out <- cbind(out,.data)
  colnames(out)[1:2] <- c("$H","var")
  return(out)
}

get_meta.symbolic_set<- function(x,...){
  .data <- get_data(x)
  out <- data.frame(v1 = "$S", v2 = ncol(.data),stringsAsFactors = F)
  out <- cbind(out,.data)
  colnames(out)[1:2] <- c("$S","var")
  return(out)
}

get_meta.symbolic_interval <- function(x,...){
  .data <- get_data(x)
  out <- data.frame("$I" = "$I", .data,stringsAsFactors = F)
  colnames(out)[1] <- c("$I")
  return(out)
}

get_meta.symbolic_modal <- function(x,...){
  .data <- get_data(x)
  out <- data.frame("$M" = "$M", v1 = ncol(.data), .data,stringsAsFactors = F)
  colnames(out)[1] <- c("$M")
  return(out)
}

get_length <- function(x,...) UseMethod("get_length")
get_length.default <- function(x,...) return(x)

get_length.symbolic_interval <- function(x,...){
  return(2)
}

get_length.symbolic_histogram<- function(x,...){
  breaks <- unlist(lapply(x, function(x)x$breaks))
  cuts <- hist(breaks,plot = F)$breaks
  return(length(cuts)-1)
}

get_length.symbolic_set<- function(x,...){
  length(unique(do.call("c",x)))
}

get_length.symbolic_modal<- function(x,...){
  length(levels(x[[1]]))
}
new_x <- to_rsdav2(x)


x <- classic_to_sym(x = iris,
                    concept = Species)
x

new_x <- to_rsdav2(x)
RSDAPlus::print.sym.data.table(new_x)
RSDAPlus::sym.radar.plot(new_x)

RSDAPlus::sym.interval.pca(new_x, method = "classic")
res <- RSDAPlus::sym.interval.pca(new_x, method = "tops")
RSDAPlus::sym.scatterplot(res$Sym.Components[,1],res$Sym.Components[,2],
                labels=TRUE,col='red', main='iris')

RSDAPlus::sym.circle.plot(res$Sym.Prin.Correlations)
RSDAPlus::sym.hclust(new_x)


