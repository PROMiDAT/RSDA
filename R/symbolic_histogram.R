#' Create an symbolic_histogram type object
#' @importFrom vctrs vec_assert new_vctr
#' @importFrom rlang abort
#' @importFrom graphics hist
#' @importFrom stats ecdf
#' @keywords internal
#'
new_sym_histogram <- function(x = double(), breaks = NA_real_){
  vctrs::vec_assert(x, numeric())
  a <- na.omit(dplyr::lead(breaks))
  b <- na.omit(dplyr::lag(breaks))
  out <- list(
    breaks = breaks,
    props = sapply(seq_along(a), function(i) sum(x>=b[i] & x<a[i])/length(x))
  )
  new_vctr(list(out), class = "symbolic_histogram")
}


#' Create an symbolic_histogram type object
#'
#' @param x character vector
#'
#' @return a symbolic histogram
#' @export
#'
#' @examples
#' sym_histogram(iris$Sepal.Length)
#' @importFrom vctrs vec_cast
#'
sym_histogram <- function(x = double(), breaks = NA_real_){
  x <- vec_cast(x, double())
  new_sym_histogram(x, breaks)
}

#' Symbolic histogram
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_histogram and FALSE otherwise.
#'
#' @examples
#' x <- sym_histogram(iris$Sepal.Length)
#' is_sym_histogram(x)
#' @export
is_sym_histogram <- function(x){
  inherits(x, "symbolic_histogram")
}

#' abbr for symbolic modal
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_histogram <- function(x) {
  "hist"
}

#' full name for symbolic modal
#' @keywords internal
#' @export
vec_ptype_full.symbolic_histogram <- function(x) {
  "symbolic_histogram"
}

#' Symbolic modal conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
format.symbolic_histogram <- function(x, ...) {
  out <- vector(mode = "character",length = length(x))
  for (i in seq_along(x)) {
    #mean. <- sprintf("%.2f",round(x[[i]]$mean,2))
    #sd. <- sprintf("%.2f",round(x[[i]]$sd,2))
    #out[i] <- stringr::str_glue("mean:{mean.} sd:{sd.}")

    breaks <- x[[i]]$breaks
    min. <- min(breaks)
    max. <- max(breaks)
    out[i] <- stringr::str_glue("breaks:{length(breaks)} min: {min.} max: {max.}")
  }
  out
}


#' Symbolic object plot with ggplot2
#' @param x a symbolic histogram
#' @param ... further arguments passed to or from other methods.
#' @export
#'
gplot <- function(x, ...) UseMethod("gplot")

#' Create a complete ggplot for symbolic histograms
#'
#' @param x A symbolic histogram
#' @param probability Logical; if TRUE, the histogram graphic is a representation of frequencies
#' @param breaks A vector giving the breakpoints between histogram cells
#' @param ... Further arguments to be passed from or to other methods.
#' @return a ggplot object
#' @importFrom dplyr lag lead
#' @importFrom scales comma
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal theme element_text
#' @rawNamespace S3method(gplot, symbolic_histogram)
#' @examples
#' h <- sym_histogram(iris$Sepal.Length)
#' gplot(h)
#'
gplot.symbolic_histogram <- function(x) {
  df <- data.frame(cats = x[[1]]$breaks, probs = x[[1]]$probs)
  ggplot(data =df,
         mapping = aes(x = cats, y = probs)) +
    geom_col(width = 1, color = "white") +
    labs(x = "",
         y = "Probability",
         caption = "Powerd by : RSDA ") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = .5))
}

#' $ operator for histograms
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_histogram` <- function(x, name){
  if(length(x) == 1L){
    return(x[[1]][[name]])
  }else {
    return(lapply(x, function(x) x[[name]]))
  }
}


#' a data.frame
#'
#' @param x .....
#' @param  ... ...
#' @export
#'
as.data.frame.symbolic_histogram <- function(x, ...) {
  df <- do.call("rbind", x$props)
  df <- as.data.frame(df)

  a <- na.omit(dplyr::lead(x$breaks[[1]]))
  b <- na.omit(dplyr::lag(x$breaks[[1]]))

  colnames(df) <- paste0("[",b," : ",a,"]")
  df
}
