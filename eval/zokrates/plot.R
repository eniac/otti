library(tidyverse)
source("../theme.R")

d <- read_csv("results.csv")
dw <- d %>% select(-wall_time) %>% pivot_wider(values_from = constraints, names_from=compiler) %>%
    mutate(reduction = (zokrates - circify)/zokrates) %>%
    mutate(ratio = circify/zokrates)
ggplot(data = dw) +
  geom_point(aes(x = benchmark, y = ratio)) +
  labs(x = "Benchmark",
       y = "Constraint Ratio\nCirC/ZoKrates\n(lower is better)") +
  geom_hline(yintercept=1) +
  scale_y_continuous(trans="log2", limits=c(0.5,2), labels=c("1/2","1", "2"), breaks = c(.5,1,2)) +
  t
ggsave("results.png", width = 3, height = 2, units = "in")
