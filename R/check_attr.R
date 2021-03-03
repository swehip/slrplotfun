#' Check if object has attribute "map"
#'
#' Useful with [dplyr::mutate_if()]
#' @param x Vector
#' @return `TRUE` or `FALSE`
#' @example man/examples/check_attr.R
#' @export
check_attr <- function(x){
  !is.null(attr(x, "map"))
}
