library(tidyverse)

d <- read_csv("results.csv")
print(d)
ggplot(data = d) +
  geom_bar(aes(x = benchmark, y = constraints, fill = compiler), stat = "identity", position = position_dodge()) +
  scale_y_continuous(trans="log2") +
  labs(x = "Benchmark",
       y = "Number of Rank-1 Constraints",
       title = "The Circom Compiler v. Circify")
ggsave("results.png", width = 6, height = 4, units = "in")