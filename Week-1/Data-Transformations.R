library(tidyverse)
library(plotly)

list <- 1:10

data <- data.frame(
  log10 = log(list, base = 10),
  log8 = log(list, base = 8),
  log6 = log(list, base = 6),
  log4 = log(list, base = 4),
  log3 = log(list, base = 3),
  natural_log = log(list, base = 2.72),
  log2 = log(list, base = 2),
  numbers = list)

data_long <- data %>%
  pivot_longer(cols = log10:log2,
               values_to = "value",
               names_to = "kind")

ggplot(data_long, aes(x = factor(numbers), y = value, group = kind, color = kind)) +
  geom_line() +
  geom_point(aes(shape = kind)) +
  labs(title = "Figure 1. \nThe Effect of log base on the efficacy of transformation",
       x = "Base",
       y = "Value") +
  theme_linedraw()






