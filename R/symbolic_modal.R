#' Create an symbolic_modal type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_modal <- function(x = character()) {
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(list(factor(x)), class = "symbolic_modal")
}

#' Create an symbolic_modal type object
#'
#' @param x character vector
#'
#' @return a symbolic modal
#' @export
#'
#' @examples
#' modal(c("a","b","b","l"))
#' @importFrom vctrs vec_cast
#'
modal <- function(x = character()){
  x <- vctrs::vec_cast(x, character())
  new_modal(x)
}

#' Symbolic modal
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_modal and FALSE otherwise.
#'
#' @examples
#' x <- modal(c("a","b","b","l"))
#' is_modal(x)
#' @export
is_modal <- function(x){
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
  out <- vector(mode = "character",length = length(x))
  for (i in seq_along(x)) {
    p <- prop.table(table(x[[i]]))
    out[i] <- stringr::str_trunc(paste0(paste0(abbreviate(names(p),minlength = 2),":",
                                               round(p,2)),
                                        collapse = " "),
                                 width = 20, ellipsis = "...")
  }
  out
}
