#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_histogram)
symbolic_dist.symbolic_histogram <- Vectorize(
  FUN = function(x = sym_histogram(0), y = sym_histogram(0), ...) {
    xy <- c(x$breaks, y$breaks)
    breaks <- pretty.default(xy, 10)
    a <- na.omit(dplyr::lag(pretty.default(breaks, n = 10)))
    b <- na.omit(dplyr::lead(pretty.default(breaks, n = 10)))

    sum((x$f(a,b) - y$f(a,b))^2)
  },
  vectorize.args = c("x","y"))

