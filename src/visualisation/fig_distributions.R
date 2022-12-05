#
#
#
#

library(here)
library(gamlss.dist)
library(ggplot2)
library(viridis)

# average cluster size
N = 50L
MU = 0.2

df.BI = data.frame(x = 0:50, BI = dBI(x = x, mu = MU, bd = N))
df.BB = data.frame(x = 0:50, BB = dBB(x = x, mu = MU, sigma = 0.1, bd = N))

theme_set(theme_minimal())

plt = ggplot() +
  geom_bar(data = df.BB, mapping = aes(x, BB, fill = "betabinom"), stat="identity") +
  geom_bar(data = df.BI, mapping = aes(x, BI, fill = "binomial"), stat="identity") +
  scale_fill_manual(name = "Distribution",
                     values = c("betabinom" = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7)[1], "binomial" = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7)[2]),
                     labels = c("betabinom" = "Beta binomial BB(n = 50, mu = 0.2, sigma = 0.1)", "binomial" = "Binomia B(n = 50, mu = 0.2)")) +
  labs(x = expression(x), y = expression(p(x))) +
  theme(legend.position = "bottom")

ggsave(plot = plt, filename = "fig_distributions.png", path = here("results", "figures"), dpi = 600, width = 200, height = 100, units = "mm", device = png)




