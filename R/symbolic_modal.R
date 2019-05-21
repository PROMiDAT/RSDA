#' Create an symbolic_modal type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_sym_modal <- function(x = character()) {
  x <- prop.table(table(x))
  out <- list(var = names(x),
              prop = as.numeric(x))
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
  if(!class(x) == "factor"){
    stop("To create a variable of modal type, the data must be of type factor.")
  }
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

    cats <- abbreviate(x[[i]]$var,3)
    props <- sprintf("%.2f",round(x[[i]]$prop,2))
    text <- paste0(paste0(cats, ":", props) ,collapse = " ")
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
    return(x[[1]]$var)
  }else {
    return(lapply(x, function(x) x$var))
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
    out <- x[[1]]$prop
    return(out)
  }else {
    return(lapply(x, function(x) x$prop))
  }
}

#' Extract values
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#'
as.data.frame.symbolic_modal <- function(x ,...){
  out <- do.call("rbind", x$props)
  colnames(out) <- unique(do.call("c",x$cats))
  out <- as.data.frame(out)
  return(out)
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
         NULL
  )
}

