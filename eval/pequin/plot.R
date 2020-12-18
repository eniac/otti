library(tidyverse)
source("../theme.R")

d <- read_csv("results.csv")
dw <- d %>% pivot_wider(values_from = constraints, names_from=compiler) %>%
    mutate(reduction = (pequin - circify)/pequin) %>%
    mutate(ratio = circify/pequin)
print(dw)
ggplot(data = dw) +
  geom_point(aes(x = benchmark, y = ratio)) +
  labs(x = "Benchmark",
       y = "Constraint Ratio\nCirC/Pequin\n(lower is better)") +
  geom_hline(yintercept=1) +
  scale_y_continuous(trans="log2", limits=c(0.0625,4), labels=c("1/16","1/4","1", "4"), breaks = c(.0625,.25,1,4)) +
  t
ggsave("results.png", width = 3, height = 2, units = "in")
