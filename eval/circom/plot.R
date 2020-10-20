library(tidyverse)

d <- read_csv("results.csv")
print(d)
ggplot(data = d) +
  geom_bar(aes(x = benchmark, y = constraints, fill = compiler), stat = "identity", position = position_dodge()) +
  scale_y_continuous(trans="log2") +
  labs(x = "Benchmark",
       y = "Rank-1 Constraints",
       title = "Circify v. The Circom Compiler") +
  theme(text = element_text(size=8),
        legend.key.size = unit(2,"mm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
       )
ggsave("results.png", width = 3, height = 2, units = "in")
