# Example data
df <- ggplot2::diamonds

# y_percent = TRUE
line_plot(df, 'cut', 'color', y_breaks = 2)
line_plot(df, 'cut', 'color', group_by_x_var = FALSE, y_breaks = 2)

# y_percent = FALSE
line_plot(df, 'cut', 'color', y_percent = FALSE, y_breaks = 2000)

# y variable included
df2 <- dplyr::group_by(df, color, cut) %>%
  dplyr::summarise(y = dplyr::n(), .groups = "drop_last")

line_plot(df2, 'cut', 'color', y_var = 'y', y_percent = FALSE, y_breaks = 2000)
line_plot(df2[df2$color == 'D', ], 'cut', y_var = 'y', y_percent = FALSE,
          y_breaks = 500)
