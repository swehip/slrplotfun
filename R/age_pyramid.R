#' Age-pyramid plot function
#'
#' Plot an age pyramid using ggplot2.
#'
#' @param df               Data frame.
#' @param age_var          Name of age variable.
#' @param gender_var       Name of gender variable.
#' @param man_level        Name of man level, probably "Man" or "Män".
#' @param age_breaks       Each age observation in an interval.
#' @param age_labels       Label of the interval.
#' @param percent          If `TRUE`, x-axis is in percent form.
#'                           Otherwise in count form.
#' @param x_breaks         Length between each break on x-axis.
#' @param x_breaks_end     Break end, default for 100,000. Works for all count
#'                           values below that.
#' @param title,subtitle   Plot title/subtitle, `NULL` for no title.
#' @param x_lab,y_lab      X/Y-axis labels, use `NULL` for no label.
#' @param fill_colors      Colors of the genders.
#' @param legend.position  Position of the legend in plot,
#'                           if `c(1,1)`, `c(1,0)` etc, legend inside plot.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_row,legend_col How many rows/columns for the legends.
#' @param ...             arguments passed to [theme_slr()]
#'
#' @return                ggplot object containing age pyramid plot.
#' @example               man/examples/age_pyramid.R
#' @export
age_pyramid <-
  function(df,
           age_var           = "Alder",
           gender_var        = "Kon",
           man_level         = "Man",
           age_breaks        = c(0, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
           age_labels        = c("0-39", "40-44", "45-49", "50-54", "55-59",
                                 "60-64", "65-69", "70-74", "75-79", "80-84",
                                 "85+" ),
           percent           = TRUE,
           x_breaks          = 5,
           x_breaks_end      = x_breaks * 1e5,
           title             = "",
           subtitle          = NULL,
           y_lab             = NULL,
           x_lab             = NULL,
           fill_colors       = c("#FFC655", "#3E92AA"),
           legend.position   = c(0, 0),
           legend_labels     = ggplot2::waiver(),
           label_breaks      = ggplot2::waiver(),
           legend_row        = NULL,
           legend_col        = NULL,
           ...
           ) {


  # Data transformation -----------------------------------------------------

  df[, age_var] <- cut(df[, age_var], breaks = age_breaks, labels = age_labels)

  df <-
    df %>%
    dplyr::group_by(.data[[gender_var]], .data[[age_var]]) %>%
    dplyr::summarise(Population = dplyr::n())

  df      <- stats::na.omit(df)
  n_man   <- sum(df$Population[df[, gender_var] == man_level])
  n_woman <- sum(df$Population[df[, gender_var] != man_level])
  x_lim   <- max(df$Population)

  df$Population[df[, gender_var] == man_level] <-
    -1 * df$Population[df[, gender_var] == man_level]

  # Breaks and labels for x-axis (which is currently y-axis before coordflip)
  br <- seq(0, x_breaks_end, x_breaks)
  x_breaks <- unique(c(-br, 0, br))
  x_labels <- abs(x_breaks)

  if (percent) {
    df$Population[df[, gender_var] == man_level] <-
      df$Population[df[, gender_var] == man_level] / n_man * 100
    df$Population[df[, gender_var] != man_level] <-
      df$Population[df[, gender_var] != man_level] / n_woman * 100

    x_lim <- max(abs(df$Population))
    x_labels <- paste0(x_labels, " %")
  }

  # Ggplot ------------------------------------------------------------------

  ggplot2::ggplot(df,
    ggplot2::aes(.data[[age_var]], .data$Population, fill = .data[[gender_var]])
  ) +

  ggplot2::geom_bar(
    stat = "identity"
  ) +

  ggplot2::xlab(x_lab) +
  ggplot2::ylab(y_lab) +
  ggplot2::ggtitle(title, subtitle = subtitle) +

  ggplot2::scale_y_continuous(
    breaks = x_breaks,
    labels = x_labels,
    limits = c(-x_lim, x_lim)
  ) +

  ggplot2::scale_fill_manual(
    paste0("n = ", as.character(n_woman + n_man)),
    values = fill_colors,
    labels = legend_labels,
    breaks = label_breaks,
    guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
  ) +

  ggplot2::coord_flip() +
  theme_slr(
    legend.position = legend.position,
    legend_title = TRUE
  )
}
