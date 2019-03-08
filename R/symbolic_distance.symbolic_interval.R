#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_interval)
symbolic_dist.symbolic_interval <- Vectorize(
  FUN = function(x = sym_interval(0), y = sym_interval(0)) {
    min_values <- c(min(x), min(y))
    max_values <- c(max(x), max(y))
    max(abs(c(diff(min_values), diff(max_values))))
  },
  vectorize.args = c("x","y"))







