#' Colors used by the SLR color pallette
#'
#' @param n number of colors
#'
#' @return vector with color codes
#' @export
#'
#' @examples
#' slr_colors(2) # two colors (not yeallow and blue)
slr_colors <- function(n = NULL) {
  clrs <- c(
    yellow = "#FFC655",
    blue   = "#3E92AA",
    black  = "#000000",
    purple = "#965C96",
    orange = "#F0863C",
    green  = "#63BA97",
    red    = "#C90327",
    pink   = "#F290A9",
    grey   = "#CCCCCC"
  )

  choose <-
    if      (is.null(n) || n <= 1) c("blue")
    else if (n == 2)               c("yellow", "blue")
    else if (n <= 4)               c("yellow", "green", "black", "blue")
    else if (n <= length(clrs))    seq_along(clrs)
    else stop("SLR does not have that many colors!")

  unname(clrs[choose])
}
