library(tidyverse)
source("../theme.R")

d <- read_csv("results.csv")
dw <- d %>% select(-wall_time) %>% pivot_wider(values_from = constraints, names_from=compiler) %>% mutate(reduction = (circom - circify)/circom)
ggplot(data = dw) +
  geom_point(aes(x = benchmark, y = 100 * reduction)) +
  geom_hline(yintercept=0) +
  labs(x = "Benchmark",
       y = "Constraint Reduction (%)") + t
ggsave("results.png", width = 3, height = 2, units = "in")
