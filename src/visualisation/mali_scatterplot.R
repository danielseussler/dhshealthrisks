#
#
#
#

library(here)
library(ggplot2)
library(viridis)

theme_set(theme_classic())

load(file = here("data", "processed", "mali", "surveydata.rda"))

plt = ggplot(data = cl, mapping = aes(x = n, y = npos, shape = urban, color = urban)) +
  geom_point() +
  geom_jitter(width = 0.4, height = 0.2) +
  geom_abline(intercept = 0, slope = 0.1, color = "gray") +
  geom_abline(intercept = 0, slope = 0.3, color = "gray") +
  labs(x = "Cluster size", y = "Tested positive") +
  scale_color_manual(values = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7), name = "Type") +
  scale_y_continuous(expand = expansion(add = 1)) +
  scale_x_continuous(limits = c(0, 110), expand = expansion(add = 0)) +
  scale_shape(name = "Type") + 
  theme(legend.position = c(.1, .9))

ggsave(plot = plt, filename = "fig_mli_scatterplot.png", path = here("results", "figures"), dpi = 600, width = 200, height = 100, units = "mm", device = png)
