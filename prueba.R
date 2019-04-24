library(RSDA)
library(ggplot2)
x <- classic_to_sym(x = diamonds,
                    concept = cut,
                    variables = c(cut,depth,clarity,carat,color),
                    depth = sym_histogram(depth),
                    clarity = sym_set(clarity))
x
x2 <- RSDAPlus::classic.to.sym(diamonds, concept = "cut",
                               col.names = c(carat,color,depth, clarity),
                               col.types = c(
                                 depth = RSDAPlus::type.histogram(),
                                 clarity = RSDAPlus::type.set()
                               ))
x2

to_rsdav2 <- function(x) {
  out <- list()
  out$concept = unname(attr(x,"concept"))
  out$N <- nrow(x)
  out$M <- ncol(x) - 1L
  out$sym.obj.names <- x$concept
  out$sym.var.names <- colnames(x[,-1])
  out$sym.var.types <- unname(sapply(x[,-1], function(x) reduce_class_name(class(x)[1])))
  out$sym.var.length <- unname(sapply(x[,-1], get_length))
  out$sym.var.starts <- c(2,5,8,11)
  out$meta <- do.call("cbind",lapply(x[,-1], get_meta))
  out$data <- do.call("cbind",lapply(x[,-1], get_data))
  class(out) <- "sym.data.table"
  return(out)
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
  probs <- x[[1]]$f(c1,c2)

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

RSDAPlus::sym.interval.pca(new_x, method = "classic")

