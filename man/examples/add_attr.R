df1 <- df2 <- data.frame(cars = 1:3)
attr(df2$cars, "map") <- data.frame(levels = 1:3,
labels = c("Volvo", "Saab", "Opel"))
df1 <- add_attr(df1, df2)
df1$cars
