#' Kaplan-Meier plot function using ggplot2.
#'
#' By default it plots 1-KM. Set cum_inc = `FALSE` to plot the survival function.
#'
#' @param survfit_obj             Object returned from [survival::survfit()].
#'                                  Also works with data frame
#'                                  if year and surv variable exist. lower
#'                                  and upper variable needed if `show_ci = TRUE`.
#'                                  Specify strata variable needed
#'                                  if several curves wanted.
#' @param cum_inc                 If `TRUE`, 1-KM is plotted.
#' @param make_step               If `TRUE`, step data will be created.
#' @param first_point             If `make_step = TRUE`, `first_point` for KM is
#'                                  1 and for competing risk 0.
#' @param one_level               Boolean indicating if there is only one
#'                                  level in the strata.
#' @param y_lim                   Limit on y-axis.
#' @param percent_accuracy        Set accuracy for [scales::percent_format()].
#' @param y_breaks                Length between each break on y-axis.
#' @param x_lim                   Limit on x-axis.
#' @param x_breaks                Length between each break on x-axis.
#' @param n_points                Number of points to be plotted,
#'                                  useful to change if file is
#'                                  large because of too many points!
#' @param title                   Plot title, `NULL` for no title.
#' @param subtitle                Small text under title, `NULL` for no subtitle.
#' @param x_lab,y_lab             X- and Y-axis labels.
#' @param line_size               Size of the head lines.
#' @param show_ci                 If `TRUE`, show confidence interval lines.
#' @param ci_line_size            Size of the confidence interval lines.
#' @param line_colors             Color of the different curves.
#' @param legend.position              Position of the legend in plot.
#' @param legend_labels           Label for each legend key, default order as
#'                                they appear in `names(survfit_obj$strata)`.
#' @param label_breaks            Order of the legend keys.
#' @param n_risk_break            Minimum number at risk to include
#' @param ribbon_ci               Show confidence interval
#' @param ribbon_alpha            Degree of transparency for confidence interval
#' @param ...                     arguments passed to [theme_slr()]
#'
#' @return                        ggplot object containing Kaplan-Meier plot.
#'
#' @example                       man/examples/km_plot.R
#' @export
km_plot <- function(
  survfit_obj,
  make_step              = NULL,
  cum_inc                = TRUE,
  first_point            = 1,
  one_level              = FALSE,
  y_lim                  = NULL,
  percent_accuracy       = 1,
  y_breaks               = 5,
  x_lim                  = NULL,
  x_breaks               = 1,
  n_points               = NULL,
  n_risk_break           = 50,
  title                  = NULL,
  subtitle               = NULL,
  y_lab                  = NULL,
  x_lab                  = NULL,
  line_size              = 0.5,
  show_ci                = TRUE,
  ribbon_ci              = TRUE,
  ribbon_alpha           = 0.5,
  ci_line_size           = 0.2,
  line_colors            = NULL,
  legend.position        = c(0, 1),
  legend_labels          = ggplot2::waiver(),
  label_breaks           = ggplot2::waiver(),
  ...
) {

  # Data suitable for ggplot ------------------------------------------------

  if (is.null(line_colors))
    line_colors <- slr_colors(dplyr::n_distinct(survfit_obj$strata))

  if (!is.data.frame(survfit_obj)) {
    if (is.null(x_lim)) {
      # range of x axis, used if n_points is specified
      time_range <- c(0, max(survfit_obj$time))
      ret_times  <- c(0, survfit_obj$time)

    } else{
      time_range <- c(0, x_lim[2])
      ret_times <- c(0, survfit_obj$time[survfit_obj$time < x_lim[2]], x_lim[2])
    }
    # For one level strata, strata is created otherwise rest of
    # code does not work
    if (one_level) {
      strata <- "1"
    } else{
      if (is.numeric(n_points)) {
        # if n_points specified, time range is used
        strata <-
          summary(
            survfit_obj,
            times = seq(time_range[1], time_range[2], length.out = n_points),
            extend = TRUE
          )$strata

      } else{
        # otherwise all points are used, included the censored points
        strata <- summary(survfit_obj, times = ret_times, extend = TRUE)$strata
      }
    }

    if (is.numeric(n_points)) {
      sf <-
        summary(
          survfit_obj,
          times = seq(time_range[1], time_range[2], length.out = n_points),
          extend = TRUE
        )

      df <-
        data.frame(
          surv    = sf$surv,
          year    = sf$time,
          lower   = sf$lower,
          upper   = sf$upper,
          strata  = strata,
          n.risk  = sf$n.risk,
          n.event = sf$n.event
        ) %>%
        dplyr::filter(.data$n.risk >= n_risk_break) %>%
        dplyr::group_by(strata) %>%
        dplyr::filter(.data$n.event != 0 | .data$n.risk == min(.data$n.risk) | .data$year == 0) %>%
        dplyr::ungroup()

    } else{
      # Creates data frame for ggplot

      sf <- summary(survfit_obj, times = ret_times, extend = TRUE)
      df <-
        data.frame(
          surv    = sf$surv,
          year    = sf$time,
          lower   = sf$lower,
          upper   = sf$upper,
          strata  = strata,
          n.event = sf$n.event,
          n.risk  = sf$n.risk
        ) %>%
        # removes points where number at risk are less than break
        dplyr::filter(.data$n.risk >= n_risk_break) %>%
        dplyr::group_by(strata) %>%
        # removes all points where there are no events
        dplyr::filter(.data$n.event != 0 | .data$n.risk == min(.data$n.risk) | .data$year == 0) %>%
        # but adds the last point for each curve
        dplyr::ungroup()

    }




    # make step function data by adding all points from data but
    # with all the next values in year
    df2 <-
      dplyr::group_by(df, strata) %>%
      dplyr::mutate(year = dplyr::lead(.data$year)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(.data$year))

    df <- rbind(df, df2) %>%
      # bind together with original data
      dplyr::arrange(.data$strata, .data$year, dplyr::desc(.data$surv), dplyr::desc(.data$n.risk))

    # remove ugly strata=level and only keep level in legend

    if(!is.null(levels(df$strata))){
      strata <- strsplit(levels(df$strata), ", ") %>%
        lapply(gsub, pattern = ".*=", replacement = "") %>%
        lapply(paste0, collapse = ", ")

      df$strata <-
        factor(df$strata,
               levels = levels(df$strata),
               labels = strata)
    }

  } else{
    df <- survfit_obj
    if (one_level) {df$strata <- "1"}
    if (make_step) {
      df2 <- data.frame(
        year   = 0,
        surv   = first_point,
        lower  = first_point,
        upper  = first_point,
        strata = unique(df$strata)
      )

      df <-
        dplyr::bind_rows(df, df2) %>%
        dplyr::arrange(.data$strata, .data$year, dplyr::desc(.data$surv))

      df2 <- dplyr::group_by(df, strata) %>%
        dplyr::mutate(year = dplyr::lead(.data$year)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(!is.na(.data$year))

      if (first_point == 1) {
        df <- rbind(df, df2) %>%
          # bind together with original data
          dplyr::arrange(.data$strata, .data$year, dplyr::desc(.data$surv))
      } else{
        df <- rbind(df, df2) %>%
          # bind together with original data
          dplyr::arrange(.data$strata, .data$year, .data$surv)
      }
    }
  }


  # If cumulative incidence = TRUE: 1-KM
  if (cum_inc) {

    dfxx <- df

    df$surv <- 1 - dfxx$surv
    df$lower <- 1 - dfxx$upper
    df$upper <- 1 - dfxx$lower

  }
  # Ggplot ------------------------------------------------------------------

  y_breaks <- y_breaks / 100

  if (is.null(x_lim)) {x_lim <- range(df$year)}
  if (is.null(y_lim)) {
    y_lim <- c(min(df$surv) - min(df$surv) %% y_breaks, 1)
  } else{
    y_lim <- y_lim / 100
  }

  km <- ggplot2::ggplot(df, ggplot2::aes(x = .data$year, y = .data$surv)) +
    ggplot2::scale_colour_manual(
      values = line_colors,
      labels = legend_labels,
      breaks = label_breaks
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(y_lim[1], y_lim[2], by = y_breaks),
      limits = y_lim,
      labels = scales::percent_format(accuracy = percent_accuracy, suffix = " %"),
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(x_lim[1], x_lim[2], by = x_breaks),
      limits = x_lim
    ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = strata, group = strata), size = line_size
    ) +
    ggplot2::ggtitle(title, subtitle = subtitle) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    theme_slr(
      legend.position       = legend.position,
      legend.justification  = legend.position,
      subtitle              = !is.null(subtitle),
      x_lab_exist           = !is.null(x_lab)
    )

  if (show_ci) {
    if (ribbon_ci) {
      km <-
        km +
        ggplot2::geom_ribbon(
          ggplot2::aes(
            ymin  = .data$lower,
            ymax  = .data$upper,
            fill  = .data$strata,
            group = .data$strata
          ),
          alpha = ribbon_alpha
        ) +
        ggplot2::scale_fill_manual(
          values = line_colors,
          labels = legend_labels,
          breaks = label_breaks
        )

    } else{
      km <-
        km +
        ggplot2::geom_line(
          ggplot2::aes(
            y      = .data$upper,
            colour = .data$strata,
            group  = .data$strata
          ),
          size = ci_line_size
        ) +
        ggplot2::geom_line(
          ggplot2::aes(
            y      = .data$lower,
            colour = .data$strata,
            group  = .data$strata
          ),
          size = ci_line_size
        )
    }
  }
  km
}
