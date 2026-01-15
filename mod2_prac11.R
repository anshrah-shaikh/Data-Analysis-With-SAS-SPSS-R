print("histiogram")
data(faithful)
ggplot(faithful, aes(x = eruptions)) +
  geom_histogram(bins = 10) +
  labs(
    title = "Histogram of Eruption Durations",
    x = "Eruption Duration",
    y = "Frequency"
  ) +
  theme_minimal()


print("box plot")
data(faithful)
ggplot(faithful, aes(y = waiting)) +
  geom_boxplot() +
  labs(
    title = "Box Plot of Waiting Time",
    y = "Waiting Time"
  ) +
  theme_minimal()
