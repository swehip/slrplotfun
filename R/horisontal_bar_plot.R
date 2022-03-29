#' Helper functions for work-around with guides on discrete scale.
#' This solution is presented in issue #3171 at
#' https://github.com/tidyverse/ggplot2/issues/3171
#' Credit to user paleolimbot for supplying this solution
#' Helper for right-hand side label in figure
#' @param label_trans labels for axis_guide$label_trans
#' @param ...         arguments passed to guide_axis
#'
#' @export
guide_axis_label_trans <- function(label_trans = identity, ...){
  axis_guide <- ggplot2::guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}
#' Helper for right-hand side label in figure
#' @param x key$.label for NextMethod inherited
#'          from guide_train ... or something
#' @param ... arguments passed to NextMethod
#'
#' @export
guide_train.guide_axis_trans <- function(x, ...){
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}


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
#' @param title              Plot title, `NULL` if no title.
#' @param subtitle           Small text under title, `NULL` if no subtitle.
#' @param y_lab              Y-axis label, use `NULL` for no label.
#' @param x_lab              X-axis label, use `NULL` for no label.
#' @param fill_colors        Color of the different categories in `fill_var`.
#' @param percent_accuracy   Set accuracy for [scales::percent_format()].
#' @param ...                arguments passed to [theme_slr()]
#'
#' @return                   ggplot object containing bar plot.
#' @export horisontal_bar_plot
#'
#' @import ggplot2
#' @import forcats
#' @import dplyr
#' @importFrom rlang .data

horisontal_bar_plot <-
  function(df,
           x_var,
           fill_var,
           title             = NULL,
           subtitle          = NULL,
           y_lab             = NULL,
           x_lab             = NULL,
           fill_colors       = NULL,
           percent_accuracy  = 1,
           ...
  )
  {

    # Fill colors ------------------------------------------------------------
    if (is.null(fill_colors)) {
      n <- if (!is.null(fill_var)) length(unique(df[[fill_var]])) else NULL
      fill_colors <- slr_colors(n)
    }
    if(!is.factor(df[[fill_var]])){
      df[[fill_var]] <- as.factor(df[[fill_var]])
    }
    if(!is.factor(df[[x_var]])){
      df[[x_var]] <- as.factor(df[[x_var]])
    }
    # Calculations by group
    df <-
      df %>%
      # TODO: replace with across()
      dplyr::group_by_at(
        c(x_var, fill_var), .drop = FALSE
      ) %>%
      dplyr::summarise(y = dplyr::n()) %>%
      dplyr::group_by_at(x_var) %>%
      dplyr::mutate(
        n = sum(.data$y),
        y2 = .data$y/.data$n
      ) %>%
      dplyr::ungroup()


    # Sort left-hand labels by y2
    df[[x_var]] <- forcats::fct_reorder2(df[[x_var]], df[[fill_var]], df$y2)

    # Construct right-hand labels sorted by y2
    right_lab <-
      df[df[[fill_var]] == levels(as.factor(df[[fill_var]]))[1], ] %>%
      dplyr::mutate(
          rlab = paste0(y, "/", n, " (", round(.data$y2*100,0), ")")
      ) %>%
      dplyr::arrange(.data$y2) %>%
      dplyr::select(.data$rlab)

    #### create ggplot object ####
    bars <-
      ggplot2::ggplot(
        df,
        aes(x = .data[[x_var]],
            y = .data$y2,
            fill = .data[[fill_var]]
        )
      ) +
      ggplot2::scale_fill_manual(
        values = fill_colors
      ) +
      ggplot2::ggtitle(
        label = title,
        subtitle = subtitle
      ) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab
      ) +
      theme_slr(
        subtitle = !is.null(subtitle),
        x_lab_exists = !is.null(x_lab)#, ...
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_fill(vjust = 0.5,
                                          reverse = TRUE)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(
          accuracy = percent_accuracy,
          suffix = " %"),
        expand = c(0,0)
      ) +
      ggplot2::guides(
        y.sec = guide_axis_label_trans(~right_lab)
      ) +
      ggplot2::coord_flip()

  return(bars)
  }

