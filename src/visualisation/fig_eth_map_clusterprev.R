#
#
#
#

library(here)
library(dplyr)
library(ggplot2)
library(viridis)
library(sf)


# data
load(file = here("data", "processed", "ethiopia", "surveydata.rda"))
shp = readRDS(file = here("data", "processed", "ethiopia", "dhsboundaries.rds"))

svs = aggregate(hazx ~ cluster + lon + lat, data = sv, mean) # FIXME
svs = st_as_sf(svs, coords = c("lon", "lat"), crs = st_crs(4326))


# num clusters
n_distinct(sv$cluster)
n_distinct(sv[, 1:2])


# admin regions and cluster level prevalence
theme_set(theme_void())

ggplot(data = shp) + geom_sf()

plt = ggplot(data = shp) + geom_sf() +
  geom_sf(data = svs, aes(color = hazx), shape = 20, size = 2) +
  scale_color_continuous(
    type = "viridis", option = "inferno", name = "Prevalence", limits = c(0, 1),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"), barwidth = unit(50, units = "mm"),
      label.hjust = 0, title.position = "top", title.hjust = 0.5
    )
  ) +
  theme(legend.position = "bottom")


ggsave(plot = plt, filename = "fig_eth_map_clusterprev.png", path = here("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
