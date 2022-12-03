#
#
#
#

library(here)
library(sf)
library(terra)
library(ggplot2)

# theme
theme_set(theme_void())

# data
shp = readRDS(file = here("data", "processed", "madagascar", "regions.rds"))

climzone = rast(x = here("data", "raw", "Beck_KG_V1", "Beck_KG_V1_present_0p083.tif"))
climzone = crop(x = climzone, y = shp$admin0)
climzone = mask(x = climzone, mask = shp$admin0)
climzone = as.factor(climzone)

# plot
plot(climzone)
levels(climzone)

