# Creating data
set.seed(123)
df <- data.frame(year = 2000:2017, prob = rnorm(18, 0.5, 0.02))

# Trend
trend_plot(df, 'year', 'prob', y_breaks = 2, y_lim = range(df$prob) * 100)
