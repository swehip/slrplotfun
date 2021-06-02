#' The Swedish Arthroplasty Registert theme
#'
#' Implementation of graphical profile used by the annual report and more.
#'
#' @inheritParams ggplot2::theme
#' @param legend_title Include legend title (boolean)
#' @param subtitle is a subtitle used in the figure (boolean)?
#' @param title_hjust,title_margin passed to
#'   `plot.title = element_text(hjust = title_hjust, margin = margin(b = title_margin)`
#' @param axis_text_angle passed to
#'   `axis.text.x = element_text(angle = axis_text_angle)`
#' @param text_size,title_size,subtitle_size Text size for most text, title and subtitle
#' @param x_lab_exists Indicator if x label should be blank or not
#' @return Modified version of [theme_classic()]
#' @export
#' @import ggplot2
theme_slr <- function(
                      axis_text_angle      = NULL,
                      legend.position      = "bottom",
                      legend.justification = legend.position,
                      legend_title         = FALSE,
                      text_size            = 7,
                      subtitle_size        = 8,
                      title_size           = 9,
                      title_hjust          = 0.5,
                      subtitle             = FALSE,
                      x_lab_exists         = FALSE,
                      title_margin         = if (subtitle) 1 else title_size / 2
                      ) {

  theme_classic() %+replace%
  theme(

# General -----------------------------------------------------------------
    text                 = element_text(colour = "black", size = text_size),
    rect                 = element_rect(linetype = "blank"),

# Axis --------------------------------------------------------------------

    axis.line             = element_line(size = 0.2),
    axis.text             = element_text(size = rel(1)),
    axis.text.x           = element_text(angle = axis_text_angle),
    axis.ticks.x          = element_line(size = 0.2),
    axis.ticks.y          = element_blank(),
    axis.title.x          = if(x_lab_exists) {
      element_text(size = rel(1),
                   margin = margin(t = 10, r = 0, b = 0, l = 0))
      
    } else {element_blank()},


# Legend ------------------------------------------------------------------

    legend.background     = element_rect(fill = "transparent"),
    legend.justification  = legend.justification,
    legend.key.height     = unit(text_size, "pt"),
    legend.key.width      = unit(text_size, "pt"),
    legend.position       = legend.position,
    legend.text           = element_text(size = rel(1)),
    legend.title          = if (legend_title)
                              element_text(size = text_size)
                            else element_blank(),


# Panel -------------------------------------------------------------------

    panel.background      = element_rect(fill = "white"),
    panel.grid.major.y    = element_line(colour = "#ADAEAE", size = 0.2),


# Plot --------------------------------------------------------------------
    plot.margin           = margin(0.2, 0.4, 0.2, 0.4, unit = "cm"),
    plot.title            = element_text(
      hjust  = title_hjust,
      size   = title_size,
      margin = margin(b = title_margin)
    ),
    plot.subtitle         = element_text(hjust = 0.5, size = subtitle_size)
  )
}
