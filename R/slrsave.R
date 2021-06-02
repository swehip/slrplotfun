#' ggsave function with default settings for SLR annual report
#' 
#'  @export
#' @import ggplot2
#' 
slrsave <- function(
  file = NULL,
  plot = NULL,
  width = 3.14961,
  height = 3.54331,
  units = "in",
  dpi = 600,
  device = "tiff",
  family = "Calibri Light"
) {
  ggplot2::ggsave(
    file, 
    plot = plot, 
    width = width, 
    height = height, 
    units = units, 
    dpi = dpi,
    type = "cairo",
    device = device,
    family = family
  )
}
