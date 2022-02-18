library(tidyverse)
library(patchwork)
source("../theme.R")

d <- read_csv("results.csv") %>% mutate(optimized = ifelse(optimized, "Yes", "No"))

print(d)

t <-   t + theme(legend.position = "bottom")
olab = "Constant Folding: "
l <- labs(x = "Input Length", shape = "Constant Folding", color = "Constant Folding")

smt_plot <-
  ggplot(d) +
  geom_point(aes(x = rounds, y = 1000*z3_time, color = optimized, shape = optimized)) +
  labs(y = "Solver Time (ms)\n(lower is better)") + l +
  scale_y_continuous(trans='log2') +
  t
ps_plot <-
  ggplot(d) +
  geom_point(aes(x = rounds, y = constraints/1000, color = optimized, shape=optimized)) +
  labs(y = "R1CS Constraints (k)\n(lower is better)") + l +
  scale_y_continuous(trans='log2') +
  t
((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
