# install.packages("ggplot2")   
# install.packages("reshape2")  

library(ggplot2)

print("Scatter plor")
data(iris)
ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Sepal Length vs Petal Length",
    x = "Sepal Length",
    y = "Petal Length"
  ) +
  theme_minimal()



print("Pie chart")
data(airquality)
month_data <- as.data.frame(table(airquality$Month))
colnames(month_data) <- c("Month", "Count")

ggplot(month_data, aes(x = "", y = Count, fill = Month)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Pie Chart of Observations by Month") +
  theme_void()



print("High low chart")
data(mtcars)
ggplot(mtcars, aes(x = rownames(mtcars))) +
  geom_linerange(aes(ymin = mpg - 3, ymax = mpg + 3)) +
  labs(
    title = "High-Low Chart of Mileage",
    x = "Car Models",
    y = "MPG Range"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

