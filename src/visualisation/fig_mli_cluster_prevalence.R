#
#
#
#

library(here)
library(ggplot2)
library(viridis)

theme_set(theme_minimal())

load(file = here("data", "cleaned", "mali", "surveydata.rda"))

plt.1 = ggplot(data = cl, mapping = aes(x = n, y = npos, shape = urban, color = urban)) +
  geom_point() +
  geom_jitter(width = 0.4, height = 0.1) +
  geom_abline(intercept = 0, slope = 0.1, color = "gray") +
  geom_abline(intercept = 0, slope = 0.3, color = "gray") +
  labs(x = "Cluster size", y = "Number tested positive") +
  scale_color_manual(values = viridis(n = 2, alpha = 0.8, begin = 0.3, end = 0.7), name = "Type") +
  scale_shape(name = "Type")

ggsave(plot = plt.1, filename = "fig_clusterprevdat_mli.png", path = here("figures"), dpi = 600, width = 200, height = 100, units = "mm", device = png)
