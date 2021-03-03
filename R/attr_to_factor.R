#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @return Factor
#' @example man/examples/attr_to_factor.R
#' @export
attr_to_factor <- function(x) {
  factor(x, attr(x, "map")$levels, attr(x, "map")$labels)
}
