#' Check duplicated names in a quo
#' @keywords internal
#' @importFrom purrr map_chr
#' @importFrom rlang get_expr
check_quo_duplicated_names <- function(x){
  duplicated_names <- which(duplicated(names(x), fromLast= T))
  fs <- purrr::map_chr(x[duplicated_names],~ as.character(rlang::get_expr(.)[[1]]))
  names(x)[duplicated_names] <- paste0(names(x)[duplicated_names], "_", fs)
  return(x)
}


#' Generate a symbolic data frame
#' @description Generate a symbolic data table from a classic data table.
#' @param x A data.frame.
#' @param concept These are the variable that we are going to use a concepts.
#' @param variables These are the variables that we want to include in the symbolic data table.
#' @param .default_numeric function to use for numeric variables
#' @param .default_categorical function to use for categorical variables
#' @param ... A vector with names and the type of symbolic data to use, the available types are type_histogram (), type_continuous (), type.set (), type.modal (), by default type_histogram () is used for numeric variables and type_modal () for the categorical variables.
#' @return a [tibble][tibble::tibble-package]
#' @references Bock H-H. and Diday E. (eds.) (2000).
#' Analysis of Symbolic Data. Exploratory methods for extracting statistical information from
#' complex data. Springer, Germany.
#' @import tidyselect
#' @importFrom rlang quos !! !!! syms
#' @importFrom dplyr group_by summarise select summarise_all left_join ungroup mutate mutate_if '%>%'
#' @importFrom  purrr compose
#' @importFrom forcats fct_unify
#' @importFrom tibble tibble
#' @export
classic_to_sym <- function(x = NULL,
                        concept = NULL,
                        variables = tidyselect::everything(),
                        .default_numeric = sym_interval,
                        .default_categorical = sym_modal,
                        ...){
  concept. <- tidyselect::vars_select(colnames(x), !!enquo(concept))
  col.types <- rlang::quos(...)
  col.types <- check_quo_duplicated_names(col.types)
  var.names <- tidyselect::vars_select(colnames(x), !!rlang::enquo(variables))
  var.names <- var.names[!var.names %in% names(col.types)]

  out1 <- x %>% dplyr::group_by(!!!rlang::syms(concept.)) %>%
    dplyr::summarise(!!!col.types)

  default_function <- function(x) {
    if(is.numeric(x)) {
      return(.default_numeric(x))
    }
    return(.default_categorical(x))
  }


  out2 <- x %>% dplyr::select(var.names) %>%
    dplyr::group_by(!!!dplyr::syms(concept.)) %>%
    dplyr::summarise_all(default_function)

  out <- dplyr::left_join(out1, out2, by = concept.) %>%
    dplyr::ungroup()

  fct_unify_modal. <- purrr::compose(function(x) vec_cast(x, new_sym_modal()),
                                     forcats::fct_unify)

  #out <- dplyr::mutate_if(out,is_sym_modal, fct_unify_modal.)
  out <- dplyr::mutate(out, concept = apply(out[,concept.],1, function(x) paste0(x, collapse = ":"))) %>%
    dplyr::select(concept, tidyselect::everything(), -concept.)
  class(out) <- c("symbolic_tbl", class(out))
  attr(out,"concept") <- concept.
  return(out)
}
