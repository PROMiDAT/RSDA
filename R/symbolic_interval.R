#' Create an symbolic_interval type object
#' @importFrom vctrs vec_assert new_vctr
#' @keywords internal
#'
new_sym_intreval <- function(min = numeric(), max = numeric()) {
  vctrs::vec_assert(min, numeric())
  vctrs::vec_assert(max, numeric())
  vctrs::new_vctr(complex(real = min, imaginary = max), class = "symbolic_interval")
}

#' Create an symbolic_interval type object
#'
#' @param x numeric vector
#' @param .min function that will be used to calculate the minimum interval
#' @param .max function that will be used to calculate the maximum interval
#'
#' @return a symbolic interval
#' @export
#'
#' @examples
#' sym_interval(c(1,2,4,5))
#' sym_interval(1:10)
#' @importFrom vctrs vec_cast
#'
sym_interval <- function(x = numeric(), .min = min, .max = max){
  x <- vctrs::vec_cast(x, numeric())
  new_sym_intreval(min = .min(x), max = .max(x))
}


#' Symbolic interval
#'
#' @param x an object to be tested
#'
#' @return returns TRUE if its argument's value is a symbolic_vector and FALSE otherwise.
#'
#' @examples
#' x <- sym_interval(1:10)
#' is_sym_interval(x)
#' is_sym_interval("d")
#' @export
is_sym_interval <- function(x){
  inherits(x,"symbolic_interval")
}

#' abbr for symbolic interval
#' @keywords internal
#' @export
vec_ptype_abbr.symbolic_interval <- function(x) {
  "interval"
}

#' full name for symbolic interval
#' @keywords internal
#' @export
vec_ptype_full.symbolic_interval <- function(x) {
  "symbolic_interval"
}

#' Symbolic interval conversion functions to and from Character
#'
#' @param x An object to be converted
#' @param ... Further arguments to be passed from or to other methods.
#'
#' @export
#' @importFrom scales comma
#' @importFrom vctrs vec_data
format.symbolic_interval <- function(x, ...) {
  min <- Re(vctrs::vec_data(x))
  max <- Im(vctrs::vec_data(x))
  paste0("[",scales::comma(min, accuracy = 0.01)," : ",
         scales::comma(max, accuracy = 0.01), "]")
}

#' Maxima and Minima
#' @rdname Maxima_and_Minima
#' @param x symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#'
#' @return a new symbolic interval with the minimum of the minima and the minimum of the maxima
#' @export
#' @importFrom vctrs vec_data
min.symbolic_interval <- function(x, ...) {
  min(Re(vctrs::vec_data(x)))
}

#' @rdname Maxima_and_Minima
#' @export
max.symbolic_interval <- function(x, ...) {
  max(Im(vctrs::vec_data(x)))
}

#' @rdname Maxima_and_Minima
#' @export
max_interval <- function(x) {
  new_sym_intreval(max(Re(vec_data(x))), max(Im(vec_data(x))))
}
#' @rdname Maxima_and_Minima
#' @export
min_interval <- function(x) {
  new_sym_intreval(min(Re(vec_data(x))), max(Im(vec_data(x))))
}

#' @rdname Maxima_and_Minima
#' @export
center_interval <- function(x) {
  (Re(vec_data(x)) + Im(vec_data(x)))/2
}
#' @rdname Maxima_and_Minima
#' @export
`$.symbolic_interval` <- function(x, name = c("min","max","mean","median")){
  switch(name,
    min = min(x),
    max = max(x),
    mean = mean(x),
    median = median(x),
    NULL
)
}


#' Symbolic interval mean
#'
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export
#' @importFrom vctrs vec_data
#'
mean.symbolic_interval <- function(x, ...) {
  new_sym_intreval(mean(vctrs::vec_data(Re(x))), mean(vctrs::vec_data(Im(x))))
}

#' Symbolic interval median
#'
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export
#' @importFrom vctrs vec_data
#' @importFrom stats median
#'
median.symbolic_interval <- function(x, ...) {
  new_sym_intreval(stats::median(vctrs::vec_data(Re(x))), stats::median(vctrs::vec_data(Im(x))))
}

#' Symbolic interval SD
#' @rdname Symbolic_interval_sd
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export
#' @importFrom vctrs vec_data
#' @importFrom stats sd
#'
sd <- function(x,...) UseMethod("sd")

#' @rdname Symbolic_interval_sd
#' @rawNamespace S3method(sd, default)
sd.default <- function(x,...) stats::var(x,...)

#' @rdname Symbolic_interval_sd
#' @rawNamespace S3method(sd, symbolic_interval)
sd.symbolic_interval <- function(x, ...) {
  new_sym_intreval(stats::sd(vctrs::vec_data(Re(x))), stats::sd(vctrs::vec_data(Im(x))))
}

#' Symbolic interval variance
#' @rdname Symbolic_interval_variance
#' @param x a symbolic interval vector
#' @param ... further arguments passed to or from other methods.
#' @export  var
#' @importFrom vctrs vec_data
var <- function(x,...) UseMethod("var")

#' @rawNamespace S3method(var, default)
var.default <- function(x,...) stats::var(x,...)

#' @rawNamespace S3method(var, symbolic_interval)
var.symbolic_interval <- function(x, ...) {
  new_sym_intreval(var(vctrs::vec_data(Re(x))), var(vctrs::vec_data(Im(x))))
}
