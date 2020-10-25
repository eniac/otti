library(tidyverse)
library(patchwork)
source("../theme.R")

d <- read_csv("results.csv") %>% mutate(optimized = ifelse(optimized, "Yes", "No"))


t <-   t + theme(legend.position = "bottom")
l <- labs(shape = "Array Eliminated", color = "Array Eliminated")

smt_plot <-
  ggplot(d) +
  geom_point(aes(x = size, y = 1000*z3_time, color = optimized, shape = optimized)) +
  labs(y = "Solver Time (ms)\n(lower is better)") + l + t + xlim(60,200)
ps_plot <-
  ggplot(d) +
  geom_point(aes(x = size, y = constraints/1000, color = optimized, shape=optimized)) +
  labs(y = "R1CS Constraints (k)\n(lower is better)") + l + t + xlim(60,200)
((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
