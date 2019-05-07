#' Create an symbolic_modal type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_sym_modal <- function(x = character()) {
  out <- list()
  out$cats <- levels(x)
  out$probs <- as.numeric(prop.table(table(x)))
  out$n <- as.numeric(table(x))
  vctrs::new_vctr(list(out), class = "symbolic_modal")
}

#' Create an symbolic_modal type object
#'
#' @param x character vector
#'
#' @return a symbolic modal
#' @export
#'
#' @examples
#' sym_modal(c("a","b","b","l"))
#' @importFrom vctrs vec_cast
#'
sym_modal <- function(x = character()){
  x <- vctrs::vec_cast(x, factor())
  new_sym_modal(x)
}

#' Symbolic modal
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_modal and FALSE otherwise.
#'
#' @examples
#' x <- sym_modal(c("a","b","b","l"))
#' is_sym_modal(x)
#' @export
is_sym_modal <- function(x){
  inherits(x, "symbolic_modal")
}

#' abbr for symbolic modal
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_modal <- function(x) {
  "modal"
}

#' full name for symbolic set
#' @keywords internal
#' @export
vec_ptype_full.symbolic_modal <- function(x) {
  "symbolic_modal"
}

#' Symbolic modal conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom  stringr str_trunc
format.symbolic_modal <- function(x, ...) {
  out <- vector(mode = "character", length = length(x))
  for (i in seq_along(x)) {
    cats <- abbreviate(x[[i]]$cats,3)
    probs <- x[[i]]$probs
    text <- paste0(stringr::str_trunc(paste0(cats,":",probs),width = 20, ellipsis = "..."),collapse = " ")
    out[i] <- text
  }
  out
}
