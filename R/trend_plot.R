#' Trend plot function
#'
#' Simple function to show trend in percent (y_var) between different years
#' (x_var) using [ggplot2::geom_smooth()].
#'
#' @param df               Data frame.
#' @param x_var,y_var      X and Y variable.
#' @param y_breaks         Length between each break on y-axis.
#' @param y_lim            Limit on y-axis.
#' @param percent_accuracy Set accuracy for [scales::percent_format()].
#' @param x_breaks         Length between each break on x-axis.
#' @param x_lab,y_lab      X and Y-axis labels, use `NULL` for no label.
#' @param title            Plot title, `NULL` for no title.
#' @param subtitle         Small text under title, `NULL` for no subtitle.
#' @param line_color       Color of the line.
#' @param fill_color       Fill color
#' @param line_size        Size of the line.
#' @param point_size       Size of the points.
#' @return                 ggplot object containing trend plot.
#' @example                man/examples/trend_plot.R
#' @export
trend_plot <- function(
                      df,
                      x_var,
                      y_var,
                      y_breaks         = 5,
                      y_lim            = c(54.9, 65.1),
                      x_breaks         = 5,
                      y_lab            = "Procent kvinnor",
                      x_lab            = "\u00E5r",
                      title            = NULL,
                      subtitle         = NULL,
                      line_color       = "#3E92AA",
                      fill_color       = "#CADBD5",
                      line_size        = 1,
                      point_size       = 1,
                      percent_accuracy = 1
                    ) {

    y_breaks <- y_breaks / 100

    if (is.vector(y_lim)) {
      y_lim <- y_lim / 100
    }

  ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]])) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
      breaks = seq(0, 1, by = y_breaks),
      limits = y_lim
    ) +
    ggplot2::scale_x_continuous(breaks = seq(1900, 2100, by = x_breaks)) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::geom_smooth(
      method = "loess",
      colour = line_color,
      size   = line_size,
      fill   = fill_color,
      alpha  = 0.9
    ) +
    ggplot2::geom_point(size = point_size) +
    theme_slr()
}
