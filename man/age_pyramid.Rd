% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_pyramid.R
\name{age_pyramid}
\alias{age_pyramid}
\title{Age-pyramid plot function}
\usage{
age_pyramid(
  df,
  age_var = "Alder",
  gender_var = "Kon",
  man_level = "Man",
  age_breaks = c(0, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
  age_labels = c("0-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80-84", "85+"),
  percent = TRUE,
  x_breaks = 5,
  x_breaks_end = x_breaks * 1e+05,
  title = "",
  subtitle = NULL,
  y_lab = NULL,
  x_lab = NULL,
  fill_colors = slr_colors(2),
  legend.position = c(0, 0),
  legend_labels = ggplot2::waiver(),
  label_breaks = ggplot2::waiver(),
  legend_row = NULL,
  legend_col = NULL,
  ...
)
}
\arguments{
\item{df}{Data frame.}

\item{age_var}{Name of age variable.}

\item{gender_var}{Name of gender variable.}

\item{man_level}{Name of man level, probably "Man" or "Män".}

\item{age_breaks}{Each age observation in an interval.}

\item{age_labels}{Label of the interval.}

\item{percent}{If \code{TRUE}, x-axis is in percent form.
Otherwise in count form.}

\item{x_breaks}{Length between each break on x-axis.}

\item{x_breaks_end}{Break end, default for 100,000. Works for all count
values below that.}

\item{title, subtitle}{Plot title/subtitle, \code{NULL} for no title.}

\item{x_lab, y_lab}{X/Y-axis labels, use \code{NULL} for no label.}

\item{fill_colors}{Colors of the genders.}

\item{legend.position}{Position of the legend in plot,
if \code{c(1,1)}, \code{c(1,0)} etc, legend inside plot.}

\item{legend_labels}{Label for each legend key.}

\item{label_breaks}{Order of the legend keys.}

\item{legend_row, legend_col}{How many rows/columns for the legends.}

\item{...}{arguments passed to \code{\link[=theme_slr]{theme_slr()}}}
}
\value{
\preformatted{           ggplot object containing age pyramid plot.
}
}
\description{
Plot an age pyramid using ggplot2.
}
\examples{
# Creating data
set.seed(123)
df <- data.frame(age = rpois(100000, 65),
  gender = sample(c('Woman', 'Woman', 'Man'), 100000, replace = TRUE))

# Age pyramid
age_pyramid(df, age_var = 'age', gender_var = 'gender',
  man_level = 'Man', title = "This is an age pyramid")

# Age pyramid with percent = FALSE
age_pyramid(df, age_var = 'age', gender_var = 'gender',
  man_level = 'Man', percent = FALSE, x_breaks = 5000,
  title = "This is an age pyramid")
}
