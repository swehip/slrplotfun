# Create trend plot for SU/Mölndal
# Look at the 9 data sets to see the required structure.
# Data sets are included in the package.
if (FALSE) { # See Issue # 2!
  example_plot <- prom_trends(
    eq_vas_exp, eq_vas_obs, eq_vas_riket, pain_exp,
    pain_obs, pain_riket, satis_exp, satis_obs, satis_riket,
    y_breaks = c(5, 0.1, 0.1), subset = 49)

  plot(example_plot)
}
