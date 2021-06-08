#' ggsave function with default settings for SLR annual report
#' 
#' @param file File name to create on disk.
#' @param plot Plot to save
#' @param width Width in units ("in", "cm", or "mm").
#' @param height Height in units ("in", "cm", or "mm").
#' @param units Units ("in", "cm", or "mm")
#' @param dpi Plot resolution.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param family Font family
#' @export
#' @import ggplot2
#' 
slrsave <- function(
  file = NULL,
  plot = ggplot2::last_plot(),
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
    family = family,
    compression = "lzw"
  )
}
