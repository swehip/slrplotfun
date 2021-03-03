library(survival)

# KM-plot with 2 levels
survfit_obj <- survfit(Surv(time / 365.24, status) ~ sex, survival::colon)
km_plot(survfit_obj, y_lim = c(40, 100), y_breaks = 10, x_lim = c(0, 9))

# KM-plot with 6 levels
survfit_obj <- update(survfit_obj, . ~ . + differ)
km_plot(survfit_obj, y_lim = c(30,100), y_breaks = 10, x_lim = c(0,9),
  line_colors = c('dodgerblue', 'red', 'green', 'black', 'yellow', 'chocolate'))
