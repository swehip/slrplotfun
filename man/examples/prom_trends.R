# Create trend plot for SU/Mölndal
# Look at 9 data sets to see the required structure.
p <-
  prom_trends(
  shprplotfun::eq_vas_exp,
  shprplotfun::eq_vas_obs,
  shprplotfun::eq_vas_riket,
  shprplotfun::pain_exp,
  shprplotfun::pain_obs,
  shprplotfun::pain_riket,
  shprplotfun::satis_exp,
  shprplotfun::satis_obs,
  shprplotfun::satis_riket,
  y_breaks = c(5, 0.1, 0.1),
  subset = 49
)

plot(p)
