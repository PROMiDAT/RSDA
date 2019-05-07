#' Create an symbolic_histogram type object
#' @importFrom vctrs vec_assert new_vctr
#' @importFrom rlang abort
#' @importFrom graphics hist
#' @importFrom stats ecdf
#' @keywords internal
#'
new_sym_histogram <- function(x = double()){
  vctrs::vec_assert(x, numeric())
  h <- hist(x, plot = F)
  breaks <- h$breaks
  c1 <- stats::na.omit(dplyr::lag(breaks))
  c2 <- stats::na.omit(dplyr::lead(breaks))
  cats <- paste0("[",scales::comma(c1, accuracy = 0.1)," : ",scales::comma(c2, accuracy = 0.1), "]")
  cats <- factor(cats,levels = cats, ordered = T)

  new_vctr(list(
    list(
      probs = h$counts/length(x),
      length = length(x),
      min = min(x),
      max = max(x),
      breaks = cats
    )
  ),class = "symbolic_histogram")
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
sym_histogram <- function(x = double()){
  x <- vec_cast(x, double())
  new_sym_histogram(x)
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
    out[i] <- "<hist>"
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
