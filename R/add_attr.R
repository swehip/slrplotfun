#' Add attributes from one data frame to another
#'
#' Add "map" attributes from df2 to df1.
#' @param df1 Data frame to receive attributes.
#' @param df2 Data frame to give attributes.
#' @return Data frame.
#' @example man/examples/add_attr.R
#' @export
add_attr <- function(df1, df2) {
  for (i in names(df1)) {
    attr(df1[[i]], "map") <- attr(df2[[i]], "map")
  }
  df1
}
