library(tidyverse)
library(patchwork)
source("../theme.R")

d <- read_csv("results.csv") %>% filter(size >= 2)
d$size = as_factor(d$size)

t <-   t + theme(legend.position = "bottom")
s <- scale_y_continuous()
l <- labs(x = "Arrays Fused (%)", color = "Permutation Size", shape = "Permutation Size")

smt_plot <- ggplot(d, mapping = aes(x = 100 * largest_perm / perms, color=size, y = z3_time, shape = size)) +
  geom_point() +
  geom_line() +
  labs(y = "Solver Time (s)\n(lower is better)") + l + s + t
ps_plot <- ggplot(d, mapping = aes(x = 100 * largest_perm / perms, color=size, y = constraints, shape = size)) +
  geom_point() +
  geom_line() +
  labs(y = "R1CS Constraints\n(lower is better)") + l + s + t

((smt_plot | ps_plot) / guide_area()) + plot_layout(heights = c(5,1),guides = 'collect')
ggsave("results.png", width = 3, height = 2, units = "in")
