#' Bar plot function
#'
#' Plot a bar plot using ggplot2.
#'
#' @param df                 Data frame.
#' @param x_var              Variable for x axis, use string name.
#'                           Recommended that `x_var` is in character in df.
#' @param fill_var           Variable for the different colors in bars,
#'                             use string name.
#'                             Use `NULL` if only one color for bars.
#' @param y_var              Variable for y axis, if `NULL`, count is used.
#' @param style              3 different styles of bar plots,
#'                            "stack", "fill", or "dodge".
#'                            fill requires `y_percent = TRUE`.
#' @param group_by_x_var     Only relevant for style dodge. Boolean indicating
#'                             if percentages should be for `x_var` or `fill_var`.
#' @param y_percent          If `TRUE`, y axis is in percent form.
#'                             Otherwise in count form.
#' @param percent_accuracy   Set accuracy for [scales::percent_format()].
#' @param y_lim              Limit on y axis.
#' @param x_breaks,y_breaks  Length between each break on x/y axis.
#' @param y_breaks_end       Break end, default for 100,000. Works for all count
#'                             values below that.
#' @param expand             If `TRUE`, the margins around the data are kept.
#' @param flip               If `TRUE`, x and y axis changes positions making
#'                             the bars go horizontally instead of vertically.
#' @param title              Plot title, `NULL` if no title.
#' @param subtitle           Small text under title, `NULL` if no subtitle.
#' @param y_lab              Y-axis label, use `NULL` for no label.
#' @param x_lab              X-axis label, use `NULL` for no label.
#' @param fill_colors        Color of the different categories in `fill_var`.
#' @param legend_labels      Label for each legend key.
#' @param label_breaks       Order of the legend keys.
#' @param legend_row         How many rows for the legends.
#' @param legend_col         How many columns for the legends.
#' @param ...                arguments passed to [theme_slr()]
#'
#' @return                   ggplot object containing bar plot.
#' @example                  man/examples/bar_plot.R
#' @export
bar_plot <-
  function(df,
           x_var,
           fill_var          = NULL,
           y_var             = NULL,
           style             = c("stack", "fill", "dodge")[1],
           group_by_x_var    = TRUE,
           y_percent         = TRUE,
           percent_accuracy  = 1,
           y_lim             = NULL,
           y_breaks          = 2000,
           x_breaks          = NULL,
           y_breaks_end      = 100000,
           title             = NULL,
           subtitle          = NULL,
           y_lab             = NULL,
           x_lab             = NULL,
           fill_colors       = NULL,
           legend_labels     = ggplot2::waiver(),
           label_breaks      = ggplot2::waiver(),
           legend_row        = NULL,
           legend_col        = NULL,
           expand            = FALSE,
           flip              = FALSE,
           ...
           ) {


  # Fill colors ------------------------------------------------------------
  if (is.null(fill_colors)) {
    n <- if (!is.null(fill_var)) length(unique(df[[fill_var]])) else NULL
    fill_colors <- slr_colors(n)
  }
  # If y_var != NULL, no summarise is needed. -------------------------------

  show_legend <- TRUE

  if (is.character(y_var)) {
    names(df)[names(df) == y_var] <- 'y'

    if (!is.character(fill_var)) {
      df$y2 <- 1
      fill_var <- "fill_var"
      show_legend <- FALSE
    } else{
      # y2 used for style dodge ----------------------------------------------

      if (group_by_x_var) {
        df <-
          df %>%
          dplyr::group_by(.data[[x_var]]) %>%
          dplyr::mutate(y2 = sum(.data$y))

      } else{
        df <-
          df %>%
          dplyr::group_by(.data[[fill_var]]) %>%
          dplyr::mutate(y2 = sum(.data$y))
      }
    }

  } else{
    # Only one fill variabel used means no legend needed  --------------------

    if (!is.character(fill_var)) {
      fill_var <- "fill_var"
      show_legend <- FALSE
      df <-
        df %>%
        dplyr::group_by(.data[[x_var]]) %>%
        dplyr::summarise(y = dplyr::n())
      df$y2 <- 1
    } else{
      # Data transformations -------------------------------------------------

      if (group_by_x_var) {
        df <-
          df %>%
          dplyr::group_by(.data[[x_var]], .data[[fill_var]]) %>%
          dplyr::summarise(y = dplyr::n()) %>%
          dplyr::group_by(.data[[x_var]]) %>%
          dplyr::mutate(y2 = sum(.data$y))

      } else{
        df <-
          df %>%
          dplyr::group_by(.data[[x_var]], .data[[fill_var]]) %>%
          dplyr::summarise(y = dplyr::n()) %>%
          dplyr::group_by(.data[[fill_var]]) %>%
          dplyr::mutate(y2 = sum(.data$y))
      }
    }
  }

  # y2 used for style dodge

  # Ggplot ------------------------------------------------------------------

  bars <- ggplot2::ggplot(data = df) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      labels = legend_labels,
      breaks = label_breaks,
      guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
    ) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::labs(x = x_lab, y = y_lab) +
    theme_slr(subtitle = !is.null(subtitle),
              x_lab_exists = !is.null(x_lab),
              ...)

  if (y_percent) {
    y_breaks <- y_breaks / 100

    if (is.vector(y_lim)) {
      y_lim <- y_lim / 100
    }

    if (style == "dodge") {
      bars <-
        bars +
        ggplot2::geom_bar(
          width = 0.5,
          mapping = ggplot2::aes(x = .data[[x_var]], y = .data$y / .data$y2, fill = .data[[fill_var]]),
          stat = "identity",
          show.legend = show_legend,
          position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim,
          expand = if(expand) ggplot2::waiver() else c(0,0)
        )

    } else if (style == "fill") {
      bars <-
        bars +
        ggplot2::geom_bar(
          width = 0.5,
          mapping = ggplot2::aes(x = .data[[x_var]], y = .data$y / sum(.data$y), fill = .data[[fill_var]]),
          stat = "identity",
          show.legend = show_legend,
          position = ggplot2::position_fill(vjust = 0.5, reverse = TRUE)
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim,
          expand = if(expand) ggplot2::waiver() else c(0,0)
        )
    } else{
      bars <-
        bars +
        ggplot2::geom_bar(
          width       = 0.5,
          mapping     = ggplot2::aes(x = .data[[x_var]], y = .data$y / sum(.data$y), fill = .data[[fill_var]]),
          stat        = "identity",
          show.legend = show_legend,
          position    = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim,
          expand = if(expand) ggplot2::waiver() else c(0,0)
        )

    }

  } else if (style == "fill") {
    bars <-
      bars +
      ggplot2::geom_bar(
        width       = 0.5,
        mapping     = ggplot2::aes(
          x = .data[[x_var]],
          y = .data$y,
          fill = if (utils::hasName(bars$data, fill_var))
            .data[[fill_var]] else fill_var
        ),
        stat        = "identity",
        show.legend = show_legend,
        position    = ggplot2::position_fill(vjust = 0.5, reverse = TRUE)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, y_breaks_end, by = y_breaks),
        limits = y_lim,
        expand = if(expand) ggplot2::waiver() else c(0,0)
      )

  } else if (style == "dodge") {
    bars <-
      bars +
      ggplot2::geom_bar(
        width = 0.5,
        mapping = ggplot2::aes(
          x = .data[[x_var]],
          y = .data$y,
          fill = if (utils::hasName(bars$data, fill_var))
            .data[[fill_var]] else fill_var
        ),
        stat = "identity",
        show.legend = show_legend,
        position = ggplot2::position_dodge(width = 0.5)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, y_breaks_end, by = y_breaks),
        limits = y_lim,
        expand = if(expand) ggplot2::waiver() else c(0,0)
      )


  } else{
    bars <-
      bars +
      ggplot2::geom_bar(
        width = 0.5,
        mapping = ggplot2::aes(
          x = .data[[x_var]],
          y = .data$y,
          fill = if (utils::hasName(bars$data, fill_var))
            .data[[fill_var]] else fill_var
        ),
        stat = "identity",
        show.legend = show_legend,
        position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
      ) +
      ggplot2::scale_y_continuous(
        breaks = seq(0, y_breaks_end, by = y_breaks),
        limits = y_lim,
        expand = if(expand) ggplot2::waiver() else c(0,0),
      )
  }

  if (is.numeric(df[[x_var]]) & !is.null(x_breaks)) {
    bars <-
      bars +
      ggplot2::scale_x_continuous(
        breaks = seq(floor(min(df[[x_var]])),
                     ceiling(max(df[[x_var]])),
                     by = x_breaks)
        )
  }

  if(flip){
    bars <-
      bars +
      ggplot2::coord_flip()
  }

  bars
}
