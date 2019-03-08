#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_modal)
symbolic_dist.symbolic_modal <- Vectorize(
  FUN = function(x = sym_modal(""), y = sym_modal("")) {
    sum((prop.table(table(x)) - prop.table(table(y)))^2)
  },
  vectorize.args = c("x","y"))


df <- classic_to_sym(iris, concept = Species,.default_numeric = sym_histogram)
df

fx <- df$Sepal.Length[[1]]$f
fy <- df$Sepal.Length[[2]]$f

lower <- -Inf
upper <- Inf
g <- function(z) (fx(z) - fy(z))^2
v <- stats::integrate(g, lower, upper,subdivisions=2000)
v$value

