#' Distance for symbolic vectors
#' @rdname symbolic_dist
#' @param x a symbolic vector or a symbolic_tbl
#' @param y a symbolic vector
#' @param alpha \itemize{
#'   \item For alpha = 1, city-block distance
#'   \item For alpha = 2, euclidean distance
#'   \item For alpha = ∞ , Chebychev distence
#' }
#' @param  w Weight
#' @param ... s3
#' @param parallel If False the future strategy ir sequential, otherwise is multiprocess
#' @importFrom stats as.dist
#' @importFrom future plan multiprocess
#' @importFrom furrr future_map
#' @export  symbolic_dist
symbolic_dist <- function(x,...) UseMethod("symbolic_dist")

#' @rdname symbolic_dist
#' @rawNamespace S3method(symbolic_dist, symbolic_tbl)
symbolic_dist.symbolic_tbl <- function(x = NULL, alpha = 2, w = 1, parallel = F, ...) {

  colnames(x) <- make.names(colnames(x), unique = T)
  names <- x$concept
  x <- dplyr::select(x, -concept)
  p <- purrr::cross_df(list(a = seq_len(nrow(x)),
                            b = seq_len(nrow(x))),
                       .filter = function(x, y) x == y | x < y)
  out <- matrix(0, nrow = nrow(x), ncol = nrow(x))

  if(parallel) {
    future::plan(strategy = future::multiprocess)
  }else {
    future::plan(strategy = future::sequential)
  }

  distances <- furrr::future_map(x, ~symbolic_dist(x = .[p$a], y = .[p$b]))
  distances <- purrr::reduce(distances, function(x, y) w*(x^alpha + y^alpha)^(1/alpha))

  for (i in seq_len(nrow(p))) {
    out[p$a[i],p$b[i]] <- distances[i]
  }
  rownames(out) <- names
  colnames(out) <- names
  return(stats::as.dist(out))
}
