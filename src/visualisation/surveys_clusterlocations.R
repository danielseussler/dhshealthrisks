# country figures of admin1 (as designated by DHS) and DHS cluster locations
#
#
#

library(ggplot2)
library(viridis)
library(sf)


# data
shp_eth = readRDS(file = file.path("data", "processed", "ethiopia", "dhsboundaries.rds"))
clr_eth = readRDS(file = file.path("data", "raw", "rdhs", "ETGE81FL.rds")) |> st_as_sf()

shp_mdg = readRDS(file = file.path("data", "processed", "madagascar", "dhsboundaries.rds"))
clr_mdg = readRDS(file = file.path("data", "raw", "rdhs",  "MDGE81FL.rds")) |> st_as_sf()

shp_mli = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
clr_mli = readRDS(file = file.path("data", "raw", "rdhs",  "MLGE81FL.rds")) |> st_as_sf() |> subset(LONGNUM != 0)


# plots
theme_set(theme_void())

plt_eth = ggplot() +
  geom_sf(data = shp_eth) +
  geom_sf(data = clr_eth, size = 0.7)

plt_mdg = ggplot() +
  geom_sf(data = shp_mdg) +
  geom_sf(data = clr_mdg, size = 0.7)

plt_mli = ggplot() +
  geom_sf(data = shp_mli) +
  geom_sf(data = clr_mli, size = 0.7)


ggsave(plot = plt_eth, filename = "ethiopia_clusterlocations.png", path = file.path("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
ggsave(plot = plt_mdg, filename = "madagascar_clusterlocations.png", path = file.path("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
ggsave(plot = plt_mli, filename = "mali_clusterlocations.png", path = file.path("results", "figures"), dpi = 600, width = 200, height = 200, units = "mm", device = png)
