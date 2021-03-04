#' PROM-trends plot function
#'
#' Visualizing PROM-trends for each clinic using ggplot2.
#'
#' @param eq_vas_exp       Data frame with expected EQ VAS data.
#' @param eq_vas_obs       Data frame with observed EQ VAS data.
#' @param eq_vas_riket     Data frame with Swedish average EQ VAS data.

#' @param pain_exp         Data frame with expected pain VAS data.
#' @param pain_obs         Data frame with observed pain VAS data.
#' @param pain_riket       Data frame with Swedish average pain VAS data.
#'
#' @param satis_exp        Data frame with expected satisfaction VAS data.
#' @param satis_obs        Data frame with observed satisfaction VAS data.
#' @param satis_riket      Data frame with Swedish average satisfaction VAS data.
#' @param riket_name       Character to find what row is Swedish average,
#'                           usually named "Riket".
#' @param y_breaks         Y breaks in the three plots.
#' @param y_labs           Labels for y-axis
#' @param year             X-axis years, character vector works.
#'                           The year variables will be renamed as they appear
#'                           in the data set.
#' @param subset           Which plots should be generated, `subset = 1`
#'                           corresponds to the first plot in alphabetical order.
#' @param legend_labels    Labels for the legends in the plot.
#' @param line_colors      Colors of the lines.
#' @param line_size        Line thickness of the lines in plot.
#' @param point_size       Point sizes in plot.
#' @param legend.position       Position of the legend in plot, matrix where each row
#'                           corresponds to a legend position is recommended,
#'                           if `length(subset) > 1`.
#' @param n_row,n_col      Number of rows/columns for the three plots.
#' @param ...              arguments passed to [theme_slr()]
#'
#' @return List of several gtable objects where each gtable object is one clinic
#' @example man/examples/prom_trends.R
#' @export
prom_trends <-
  function(eq_vas_exp,
           eq_vas_obs,
           eq_vas_riket,
           pain_exp,
           pain_obs,
           pain_riket,
           satis_exp,
           satis_obs,
           satis_riket,
           riket_name       = "Riket",
           y_labs           = c("EQ VAS", "Sm\u00E4rt VAS",
                                "Tillfredst\u00E4llelse"),
           y_breaks         = c(5, 5, 5),
           year             = c('2008/09', '2010/11', '2012/13', '2014/15'),
           subset           = 1,
           legend_labels    = c("F\u00F6rv\u00E4ntat", "Observerat", "Riket"),
           line_colors      = c("#3E92AA", "#C90327", "black"),
           line_size        = 0.5,
           point_size       = 1.5,
           legend.position  = c(0, 0),
           n_row            = 1,
           n_col            = 3,
           ...
           ) {

    # Transforming data to be suitable for ggplot -----------------------------

    ggp_suit <- function(df, year, sheet) {
      df$hospital <- ordered(df$hospital, levels = df$hospital)
      names(df) <- c("hospital", year)

      y <- as.vector(apply(df[, year], 1, as.numeric))
      years <- rep(year, dim(df)[1])
      hospital <- sort(rep(df$hospital, dim(df)[2] - 1))

      df <- dplyr::tibble(hospital, years, y, sheet)

      df$years <- ordered(df$years, levels = year)
      df
    }

    ggsuit_df <- function(df1, df2, df3) {
      rbind(
        ggp_suit(df1, year = year, sheet = legend_labels[1]),
        ggp_suit(df2, year = year, sheet = legend_labels[2]),
        ggp_suit(df3, year = year, sheet = legend_labels[3])
      )
    }

    # Data transformation -----------------------------------------------------

    eq_vas       <- ggsuit_df(eq_vas_exp, eq_vas_obs, eq_vas_riket)
    pain         <- ggsuit_df(pain_exp,   pain_obs,   pain_riket  )
    satis        <- ggsuit_df(satis_exp,  satis_obs,  satis_riket )

    eq_vas$sheet <- ordered(eq_vas$sheet, levels = legend_labels)
    pain$sheet   <- ordered(pain$sheet,   levels = legend_labels)
    satis$sheet  <- ordered(satis$sheet,  levels = legend_labels)
    hospitals    <- unique(satis$hospital)

    # Get right ylim function -------------------------------------------------

    get_ylim <- function(y_vec, y_break) {
      top <- ((max(y_vec, na.rm = TRUE) + y_break) %/% y_break) * y_break

      bottom <- (min(y_vec, na.rm = TRUE) %/% y_break) * y_break

      if (top - bottom < 3 * y_break) {
        top <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE)) /
                  2 + 2 * y_break) %/% y_break) * y_break

        bottom <- (((max(y_vec, na.rm = TRUE) + min(y_vec, na.rm = TRUE)) /
                      2 - y_break) %/% y_break) * y_break

      }
      return(c(bottom, top))
    }

    # Ggplots -----------------------------------------------------------------

    plot_list <- list()

    count <- 1

    if (is.vector(legend.position)) {
      legend.position <-
        matrix(rep(legend.position, length(subset)), nrow = length(subset))
    }

     for (i in subset) {
      eq_vas_data <- dplyr::filter(
          eq_vas, .data$hospital == hospitals[i] | .data$hospital == riket_name)
      pain_data <- dplyr::filter(
          pain, .data$hospital == hospitals[i] | .data$hospital == riket_name)
      satis_data <- dplyr::filter(
          satis, .data$hospital == hospitals[i] | .data$hospital == riket_name)

      ylim_eq <- get_ylim(eq_vas_data$y, y_breaks[1])
      ylim_sm <- get_ylim(pain_data$y, y_breaks[2])
      ylim_tf <- get_ylim(satis_data$y, y_breaks[3])

      eq_vas_plot <-
        ggplot2::ggplot(
          eq_vas_data,
          ggplot2::aes(.data$years, .data$y, color = .data$sheet, group = .data$sheet)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_eq[1], ylim_eq[2], by = y_breaks[1]),
          limits = ylim_eq) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::geom_line(size = line_size, show.legend = TRUE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = TRUE) +
        ggplot2::ylab(y_labs[1]) +
        ggplot2::ggtitle(hospitals[i]) +
        theme_slr(
          title_hjust = 0,
          legend.position      = legend.position[count, ],
          legend.justification = legend.position[count, ]
        ) +
        ggplot2::theme(
          legend.margin        = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
          legend.box.margin    = ggplot2::margin(0, 0, 0, 0, unit = "mm"),
        )

      pain_plot <-
        ggplot2::ggplot(pain_data,
               ggplot2::aes(.data$years, .data$y, color = .data$sheet, group = .data$sheet)) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_sm[1], ylim_sm[2], by = y_breaks[2]),
          limits = ylim_sm) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::geom_line(size = line_size, show.legend = FALSE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = FALSE) +
        ggplot2::ggtitle("") +
        ggplot2::ylab(y_labs[2]) +
        theme_slr()

      satis_plot <-
        ggplot2::ggplot(satis_data,
               ggplot2::aes(.data$years, .data$y, color = .data$sheet, group = .data$sheet)) +
        ggplot2::scale_y_continuous(
          breaks = seq(ylim_tf[1], ylim_tf[2], by = y_breaks[3]),
          limits = ylim_tf) +
        ggplot2::scale_colour_manual(values = line_colors) +
        ggplot2::geom_line(size = line_size, show.legend = FALSE) +
        ggplot2::geom_point(shape = 18,
                   size = point_size,
                   show.legend = FALSE) +
        ggplot2::ggtitle("") +
        ggplot2::ylab(y_labs[3]) +
        theme_slr()

      plot_list[[count]] <-
        gridExtra::arrangeGrob(
          eq_vas_plot,
          pain_plot,
          satis_plot,
          nrow = n_row,
          ncol = n_col)

      count <- count + 1
    }

    if (length(subset) < 2) {
      return(gridExtra::arrangeGrob(
        eq_vas_plot,
        pain_plot,
        satis_plot,
        nrow = n_row,
        ncol = n_col
      ))

    } else{
      return(plot_list)
    }
  }
