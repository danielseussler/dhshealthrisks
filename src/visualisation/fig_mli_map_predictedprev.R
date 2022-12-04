#
#
#
#

library(here)
library(h3)
library(ggplot2)
library(viridis)
library(patchwork)
library(gamlss.dist)

# predicted mu and sigma were computed in the file analysis.R
load(file = here("models", "9dkw7wyn.rda"))

# compute quantiles from conditional
N = 1000L
pred$mean = N * pred$mu
pred$lower = qBB(p = 0.1, mu = pred$mu, sigma = pred$sigma.V1, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)
pred$upper = qBB(p = 0.9, mu = pred$mu, sigma = pred$sigma.V1, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)

# get geometries of locations
sf = h3_to_geo_boundary_sf(pred$h3_index)
sf = cbind(sf, pred)

theme_set(theme_void())

# plots
plt.1 = ggplot(data = sf) +
  geom_sf(mapping = aes(fill = mean), color = NA) +
  scale_fill_continuous(
    type = "viridis", option = "cividis", name = "Estimated Prevalence per 1000", limits=c(0L, 1000L),
    guide = guide_colorbar(direction = "horizontal", barheight = unit(2L, units = "mm"),
                           barwidth = unit(50, units = "mm"), label.hjust = 0L,
                           title.position = "top", title.hjust = 0.5)
  ) +
  theme(legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")

plt.2 = ggplot(data = sf) +
  geom_sf(mapping = aes(fill = lower), color = NA) +
  scale_fill_viridis(option = "cividis", limits = c(0L, 1000L)) +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude")

plt.3 = ggplot(data = sf) +
  geom_sf(mapping = aes(fill = upper), color = NA) +
  scale_fill_viridis(option = "cividis", limits = c(0L, 1000L)) +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude")

patch = plt.2 + plt.1 + plt.3 + plot_annotation(tag_levels = "A")

ggsave(plot = plt.1, filename = "fig_mli_predictedprev.png", path = here("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
ggsave(plot = patch, filename = "fig_mli_predictedprev_ucq.png", path = here("results", "figures"), dpi = 600, width = 200, height = 100, units = "mm", device = png)
