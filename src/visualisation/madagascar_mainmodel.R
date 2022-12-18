#
#
#
#

library(here)
library(ggplot2)
library(data.table)
library(Metrics)

source(file = here("src", "utils", "func_plot_partial_effects.R"))

# load data
load(file = here("models", "h9h7v919.rda"))
shp = readRDS(file = here("data", "processed", "madagascar", "dhsboundaries.rds"))

names(coef(mod))

# theme 
theme_set(theme_classic())
theme_update(
  legend.background = element_rect(fill = "transparent", colour = NA)
  , panel.background = element_rect(fill = "transparent", colour = NA)
  , plot.background = element_rect(fill = "transparent", colour = NA)
)


# plot partial effects panel
plts = vector(mode = "list")

plts$A = plt_numeric(.mod = mod, .data = sv, .var = "cage", .rugged = TRUE, .xlab = "cage")
plts$B = plt_numeric(.mod = mod, .data = sv, .var = "mage", .rugged = TRUE, .xlab = "Precipitation")
plts$C = plt_numeric(.mod = mod, .data = sv, .var = "ndvi", .rugged = TRUE, .xlab = "NDVI")



# theme 
theme_set(theme_void())
theme_update(
  legend.background = element_rect(fill = "transparent", colour = NA)
  , panel.background = element_rect(fill = "transparent", colour = NA)
  , plot.background = element_rect(fill = "transparent", colour = NA)
)

# plot spatial effect
plt.gmrf = plt_gmrf(mod, sv, .var = "dhsregion", .shp = shp, .limscol = c(-0.8, 0.8))
ggsave(plot = plt.gmrf, filename = "fig_mdg_main_effects_spatial.png", path = here::here("results", "figures"), dpi = 600L, scale = 1.3, width = 200L, height = 100L, units = "mm", device = png, bg = "transparent")
