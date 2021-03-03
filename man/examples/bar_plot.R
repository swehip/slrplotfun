# Example data
df <- ggplot2::diamonds

# Style stack
bar_plot(df, 'color', 'cut', y_breaks = 2)
bar_plot(df, 'color', 'cut', y_percent = FALSE, y_breaks = 2000)

# Style stack with y variable included
df2 <-
  dplyr::group_by(df, color, cut) %>%
  dplyr::summarise(y = dplyr::n(), .groups = "drop_last")
bar_plot(df2, 'color', 'cut', y_var = 'y', y_breaks = 2)

# Style fill
bar_plot(df, 'color', 'cut', y_breaks = 10, style = 'fill')

# Style dodge grouped by x_var (color in this case)
bar_plot(df, 'color', 'cut', style = 'dodge', y_breaks = 10)
bar_plot(df, 'color', 'cut', style = 'dodge', y_percent = FALSE, y_breaks = 2000)

# Style dodge grouped by fill_var (cut in this case)
bar_plot(df, 'color', 'cut', style = 'dodge', group_by_x_var = FALSE, y_breaks = 10)

# Since bar_plot() returns ggplot object, it is possible to add more features
# Here we zoom the plot using coord_cartesian():
df3 <- dplyr::filter(df, clarity %in% c('SI1', 'SI2', 'VS2'))
bar_plot(df3, 'clarity', style = 'dodge', y_percent = FALSE, y_breaks = 2000) +
  ggplot2::coord_cartesian(ylim = c(8000, 14000))
