#
#
#
#

library(sf)
library(terra)
library(ggplot2)

theme_set(theme_void())

load(file = file.path("data", "processed", "mali", "surveydata.rda"))
mali_shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))


# data
cl_sf = st_as_sf(cl, coords = c("lon", "lat"), crs = st_crs(4326))
cl_sf$prev = cl_sf$npos / cl_sf$n

climzone = rast(x = file.path("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = mali_shp)
climzone = mask(x = climzone, mask = mali_shp)
climzone = as.factor(climzone)

val = data.frame(ID = c(3, 4, 6), label = c("Tropical, savannah", "Arid, desert, hot", "Arid, steppe, hot"))
climzone = categories(climzone, value = val, index = 2)
climzone_df = as.data.frame(climzone, xy = T)


# plots
plt.1 = ggplot() +
  geom_sf(data = mali_shp) +
  geom_sf(data = cl_sf, aes(color = prev), shape = 20, size = 2) +
  scale_color_continuous(
    type = "viridis", option = "inferno", name = "Prevalence", limits = c(0, 1),
    guide = guide_colorbar(direction = "horizontal", barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"), label.hjust = 0,
      title.position = "top", title.hjust = 0.5)
  ) +
  theme(legend.position = "bottom")

plt.2 = ggplot() +
  geom_raster(data = climzone_df, mapping = aes(x = x, y = y, fill = label)) +
  scale_fill_manual(values = c("#46AAFA", "#FF0000", "#F5A500"), na.value = "transparent", na.translate = F, name = "") +
  # scale_fill_grey(na.value = "transparent", na.translate = F, name = "", start = 0.4, end = 0.8) +
  geom_sf(data = mali_shp, fill = NA) +
  geom_sf(data = cl_sf, size = 0.7) +
  theme(legend.position = "bottom")


ggsave(plot = plt.1, filename = "mali_clusterprevalence.png", path = file.path("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
ggsave(plot = plt.2, filename = "mali_climatezones.png", path = file.path("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
