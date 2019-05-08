#' Create an symbolic_set type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_sym_set <- function(x = character()) {
  vctrs::vec_assert(x, character())
  vctrs::new_vctr(list(factor(unique(x))), class = "symbolic_set")
}

#' Create an symbolic_set type object
#'
#' @param x character vector
#'
#' @return a symbolic set
#' @export
#'
#' @examples
#' sym_set(c("a","b","b","l"))
#' @importFrom vctrs vec_cast
#'
sym_set <- function(x = character()){
  x <- vctrs::vec_cast(x, character())
  new_sym_set(x)
}

#' Symbolic set
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_set and FALSE otherwise.
#'
#' @examples
#' x <- sym_set(c("a","b","b","l"))
#' is_sym_set(x)
#' @export
is_sym_set <- function(x){
  inherits(x, "symbolic_set")
}

#' abbr for symbolic set
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_set <- function(x) {
  "set"
}

#' full name for symbolic set
#' @keywords internal
#' @export
vec_ptype_full.symbolic_set <- function(x) {
  "symbolic_set"
}

#' Symbolic set conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom  stringr str_trunc
format.symbolic_set <- function(x, ...) {
  out <- vector(mode = "character",length = length(x))
  for (i in seq_along(x)) {
    cats <- levels(x[[i]])
    text <- paste0(cats, collapse = ",")
    text <- stringr::str_trunc(text,width = 30,ellipsis = "...")
    text <- paste0("{",text,"}")
    out[i] <- text
  }
  out
}


#' $ operator for set
#'
#' @param x .....
#' @param name ...
#' @export
`$.symbolic_set` <- function(x, name = c("levels","values")){
  switch(name,
         levels = {
           if(length(x) == 1){
             return(levels(x[[1]]))
           }else {
             return(lapply(x, levels))
           }
         },
         values = {
           if(length(x) == 1){
             return(table(x[[1]]))
           }else {
             return(lapply(x, table))
           }
         },
         NULL
  )
}
