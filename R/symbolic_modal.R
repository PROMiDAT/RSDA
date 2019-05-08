#' Create an symbolic_modal type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_sym_modal <- function(x = character()) {
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
#' sym_modal(c("a","b","b","l"))
#' @importFrom vctrs vec_cast
#'
sym_modal <- function(x = character()){
  x <- vctrs::vec_cast(x, character())
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
    cats <- abbreviate(levels(x[[i]]),3)
    probs <- sprintf("%.2f",round(prop.table(table(x[[i]])),2))
    text <- paste0(paste0(cats,":",probs),collapse = " ")
    text <- stringr::str_trunc(text, width = 30, ellipsis = "...")
    out[i] <- text
  }
  out
}


#' Extract categories
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export

get_cats <- function(x,...) UseMethod("get_cats")

#' @rawNamespace S3method(var, default)
get_cats.symbolic_modal <- function(x, ...){
  if(length(x) == 1){
    return(levels(x[[1]]))
  }else {
    return(lapply(x, levels))
  }
}

#' Extract prop
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export

get_props <- function(x,...) UseMethod("get_props")

#' @rawNamespace S3method(var, default)
get_props.symbolic_modal <- function(x, ...){
  if(length(x) == 1){
    out <- prop.table(table(x[[1]]))
    return(out)
  }else {
    .f <- function(x) prop.table(table(x))
    return(lapply(x,.f))
  }
}

#' Extract counts
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export

get_counts <- function(x,...) UseMethod("get_counts")

#' @rawNamespace S3method(var, default)
get_counts.symbolic_modal <- function(x, ...){
  if(length(x) == 1){
    out <- table(x[[1]])
    return(out)
  }else {
    .f <- function(x) table(x)
    return(lapply(x,.f))
  }
}
#' $ operator for modals
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_modal` <- function(x, name = c("cats","props","counts")){
  switch(name,
         cats = get_cats(x),
         props = get_props(x),
         counts = get_counts(x),
         NULL
  )
}

