# Create trend plot for SU/Mölndal
# Look at 9 data sets to see the required structure.
p <-
  prom_trends(
  rcplot::eq_vas_exp,
  rcplot::eq_vas_obs,
  rcplot::eq_vas_riket,
  rcplot::pain_exp,
  rcplot::pain_obs,
  rcplot::pain_riket,
  rcplot::satis_exp,
  rcplot::satis_obs,
  rcplot::satis_riket,
  y_breaks = c(5, 0.1, 0.1),
  subset = 49
)

plot(p)
