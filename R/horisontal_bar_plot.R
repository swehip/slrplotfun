# Function to make stacked horisontal bar diagram

#' Bar plot function
#'
#' Plot a bar plot using ggplot2.
#'
#' @param df                 Data frame.
#' @param ...                arguments passed to [theme_slr()]
#' @param title              Plot title, `NULL` if no title.
#' @param subtitle           Small text under title, `NULL` if no subtitle.
#' @param y_lab              Y-axis label, use `NULL` for no label.
#' @param x_lab              X-axis label, use `NULL` for no label.
#'
#' @return                   ggplot object containing bar plot.
#'

#' @export
# Helper functions for work-around with guides on discrete scale
guide_axis_label_trans <- function(label_trans = identity, ...){
  axis_guide <- ggplot2::guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

#' @export
guide_train.guide_axis_trans <- function(x, ...){
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}

#' @export
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
    #vars <- c(x_var, fill_var)
    df <-
      df %>%
      dplyr::group_by_at(
        c(x_var, fill_var)
      ) %>%
      dplyr::summarise(y = dplyr::n()) %>%
      dplyr::group_by_at(x_var) %>%
      dplyr::mutate(
        n = sum(y),
        y2 = y/n
      ) %>%
      dplyr::ungroup()

    df$lab <- forcats::fct_reorder2(df[[x_var]], df[[fill_var]], df$y2)


    labe <-
      df[df[[fill_var]] == levels(as.factor(df[[fill_var]]))[1], ] %>%
      dplyr::mutate(
          rlab = paste0(n, " (", round(y2,2)*100, "%)")
      ) %>%
      arrange(y2) %>%
      select(rlab)

    #### create ggplot object ####
    bars <-
      ggplot2::ggplot(
        df,
        aes(x = .data[[x_var]],
            y = y2,
            fill = .data[[fill_var]]
        )
      ) +
      ggplot2::scale_fill_manual(
        values = fill_colors
      ) +
      ggplot2::ggtitle(
        title,
        subtitle = subtitle
      ) +
      ggplot2::labs(
        x = x_lab,
        y = y_lab
      ) +
      theme_slr(
        subtitle = !is.null(subtitle),
        x_lab_exists = !is.null(x_lab),
        ...
      ) +
      ggplot2::geom_bar(
        stat = "identity",
        position = ggplot2::position_fill(vjust = 0.5,
                                          reverse = TRUE)
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
        expand = c(0,0)
      ) +
      ggplot2::guides(
        y.sec = guide_axis_label_trans(~labe)
      ) +
      ggplot2::coord_flip()

  return(bars)
  }

