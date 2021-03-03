df <- data.frame(cars = 1:3)
attr(df$cars, "map") <-
  data.frame(levels = 1:3, labels = c("Volvo", "Saab", "Opel"))
  attr_to_factor(df$cars)
