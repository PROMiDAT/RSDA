#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_set)
symbolic_dist.symbolic_set <- Vectorize(
  FUN = function(x = sym_set(""), y = sym_set("")) {
    sum(x %in% y) / length(base::union(x,y))
  },
  vectorize.args = c("x","y"))
