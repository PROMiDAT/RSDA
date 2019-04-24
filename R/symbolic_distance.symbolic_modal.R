#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_modal)
symbolic_dist.symbolic_modal <- Vectorize(
  FUN = function(x = sym_modal(""), y = sym_modal(""), ...) {
    sum((prop.table(table(x)) - prop.table(table(y)))^2)
  },
  vectorize.args = c("x","y"))


