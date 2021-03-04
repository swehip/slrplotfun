#' Line plot function
#'
#' Standard line plot using ggplot2. Y-variable not necessary.
#'
#' @param df               Data frame.
#' @param x_var            Variable for x-axis, use string name.
#'                           Recommended that x_var is
#'                           in character in df (not necessary).
#' @param color_var        Variable for the different colors in lines, use string
#'                           name. Use `NULL` if only one color for lines.
#' @param y_var            Variable for y axis, if `NULL`, count is used.
#' @param group_by_x_var   Boolean indicating if percentages should be for `x_var`
#'                           or `color_var`.
#' @param y_percent        If `TRUE`, y-axis is in percent form. Otherwise in count
#'                           form.
#' @param percent_accuracy Set accuracy for [scales::percent_format()].
#' @param y_lim            Limit on y-axis.
#' @param y_breaks         Length between each break on y-axis.
#' @param y_breaks_end     Break end, default for 100,000. Works for all count
#'                           values below that.
#' @param line_size        Size of the lines.
#' @param title            Plot title, `NULL` if no title.
#' @param subtitle         Small text under title, `NULL` if no subtitle.
#' @param y_lab            Y-axis label, use `NULL` for no label.
#' @param x_lab            X-axis label, use `NULL` for no label.
#' @param fill_colors      Colors of the different categories in color_var.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_row       How many rows for the legends.
#' @param legend_col       How many columns for the legends.
#' @param ...              Arguments passed to [theme_slr()]
#'
#' @return                 Ggplot object containing line-plot.
#' @example                man/examples/line_plot.R
#' @export
line_plot <-
  function(
    df,
    x_var,
    color_var         = NULL,
    y_var             = NULL,
    group_by_x_var    = TRUE,
    y_percent         = TRUE,
    percent_accuracy  = 1,
    y_lim             = NULL,
    y_breaks          = 2000,
    y_breaks_end      = 100000,
    line_size         = 1,
    title             = NULL,
    subtitle          = NULL,
    y_lab             = NULL,
    x_lab             = NULL,
    fill_colors       = NULL,
    legend_labels     = ggplot2::waiver(),
    label_breaks      = ggplot2::waiver(),
    legend_row        = NULL,
    legend_col        = NULL,
    ...
  ) {


    # Fill colours ------------------------------------------------------------
    # Different colours depending on number of categories.
    if (is.null(fill_colors) & !is.null(color_var)) {
      if (length(unique(df[[color_var]])) <= 2) {
        fill_colors <- c("#3E92AA", #Blue
                         "#FFC655") #Grey
      } else if (length(unique(df[[color_var]])) <= 4) {
        fill_colors <- c("#FFC655", #Yellow
                         "#63BA97", #Green
                         "#000000", #Black
                         "#3E92AA") #Blue
      } else {
        fill_colors <- c("#FFC655", #Yellow
                         "#3E92AA", #Blue
                         "#000000", #Black
                         "#965C96", #Purple
                         "#F0863C", #Orange
                         "#63BA97", #Green
                         "#C90327", #Red
                         "#F290A9", #Pink
                         "#CCCCCC") #Grey
      }

    } else if (is.null(fill_colors) & is.null(color_var)) {
      fill_colors <- "#3E92AA"
    }


    # If y_var != NULL, no summarise is needed. -------------------------------

    show_legend <- TRUE

    if (is.character(y_var)) {
      names(df)[names(df) == y_var] <- 'y'
      df$y2 <- 1

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
      }

    } else{
    # Only one fill variabel used means no legend needed  ---------------------

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
        df <-
          df %>%
          dplyr::group_by(.data[[x_var]]) %>%
          dplyr::summarise(y = dplyr::n()) %>%
          dplyr::mutate(y2 = sum(.data$y))
      } else{
    # Data transformations ----------------------------------------------------

        if (group_by_x_var) {
          df <-
            df %>%
            dplyr::group_by(.data[[x_var]], .data[[color_var]]) %>%
            dplyr::summarise(y = dplyr::n()) %>%
            dplyr::group_by(.data[[x_var]]) %>%
            dplyr::mutate(y2 = sum(.data$y))

        } else{
          df <-
            df %>%
            dplyr::group_by(.data[[x_var]], .data[[color_var]]) %>%
            dplyr::summarise(y = dplyr::n()) %>%
            dplyr::group_by(.data[[color_var]]) %>%
            dplyr::mutate(y2 = sum(.data$y))
        }
      }
    }

    # Ggplot ------------------------------------------------------------------

    lines <-
      ggplot2::ggplot(data = df) +
      ggplot2::scale_color_manual(
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
      )  +
      ggplot2::ylab(y_lab) +
      ggplot2::xlab(x_lab) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      theme_slr(...)

    if (y_percent) {
      y_breaks <- y_breaks / 100

      if (is.vector(y_lim)) {
        y_lim <- y_lim / 100
      }

      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y / .data$y2,
            color = if (utils::hasName(lines$data, color_var))
            .data[[color_var]] else color_var,
            group = if (utils::hasName(lines$data, color_var))
            .data[[color_var]] else color_var
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim
        )

    } else{
      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y,
            color = if (utils::hasName(lines$data, color_var))
              .data[[color_var]] else color_var,
            group = if (utils::hasName(lines$data, color_var))
            .data[[color_var]] else color_var
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim
        )
    }
  lines
}
