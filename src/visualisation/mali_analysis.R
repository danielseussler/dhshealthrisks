#
#
#
#

library(ggplot2)
library(viridis)
library(patchwork)
library(sf)
library(gamboostLSS)
library(gamlss.dist)

theme_set(theme_classic())

theme_update(
  legend.background = element_rect(fill = "transparent", colour = NA)
  , panel.background = element_rect(fill = "transparent", colour = NA)
  , plot.background = element_rect(fill = "transparent", colour = NA)
)


# load custom plot functions
# plot partial effects from main model
load(file = file.path("models", "9dkw7wyn.rda"))
mali_shp = readRDS(file = file.path("data", "processed", "mali", "dhsboundaries.rds"))
source(file = file.path("src", "utils", "func_plot_partial_effects.R"))

table(mod$mu$xselect())
table(mod$sigma$xselect())

names(coef(mod, parameter = "mu"))
names(coef(mod, parameter = "sigma"))

table(selected(mod)$mu)
table(selected(mod)$sigma)



# plot partial effects panel
plts = vector(mode = "list")

plts$A = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "log_pop", .rugged = TRUE, .title = expression(mu), .xlab = "Population (log)")
plts$B = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "precip", .rugged = TRUE, .title = expression(mu), .xlab = "Precipitation")
plts$C = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "ndvi", .rugged = TRUE, .title = expression(mu), .xlab = "NDVI")
plts$D = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "evi", .rugged = TRUE, .title = expression(mu), .xlab = "EVI")
plts$E = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "elev", .rugged = TRUE, .title = expression(mu), .xlab = "Elevation")
plts$G = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "lstnight", .rugged = TRUE, .title = expression(mu), .xlab = "LST (night)")
plts$H = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "lstday", .rugged = TRUE, .title = expression(mu), .xlab = "LST (day)")

ggsave(
  plot = wrap_plots(plts, ncol = 2L) + plot_annotation(tag_levels = "A")
  , filename = "mali_maineffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 210L, height = 210L
  , units = "mm", device = png, bg = "transparent"
)


# coefficient table for categorical covariates
coef(mod, parameter = "mu", which = "urban")$`bols(urban)` |> round(digits = 3)
coef(mod, parameter = "mu", which = "climate")$`bols(climate)` |> round(digits = 3)
coef(mod, parameter = "sigma", which = "urban")$`bols(urban)` |> round(digits = 3)


# spatial effects plot
plts_spatial = vector(mode = "list")

plts_spatial$mu = plt_spatial_s(.mod = mod, .data = cl, .parameter = "mu", .shp = mali_shp, .limscol = c(-4.4, 4), .title = expression(mu))
plts_spatial$mu = plts_spatial$mu + theme_void() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

plts_spatial$sigma = plt_spatial_s(.mod = mod, .data = cl, .parameter = "sigma", .shp = mali_shp, .limscol = c(-4.4, 4), .title = expression(sigma))
plts_spatial$sigma = plts_spatial$sigma + theme_void() + theme(plot.title = element_text(hjust = 0.5))

ggsave(
  plot = wrap_plots(plts_spatial, ncol = 2L) + plot_annotation(tag_levels = "A")
  , filename = "mali_spatialeffects.png"
  , path = file.path("results", "figures")
  , dpi = 600L, scale = 1.3, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)


# plot the predicted parameter of the response distribution
pred_sf = h3_to_geo_boundary_sf(pred$h3_index)
pred_sf = cbind(pred_sf, pred)

map_mean = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = mu), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = expression(hat(mu))
    , limits = c(0L, 1)
  ) +
  theme_void()

ggsave(
  plot = map_mean
  , filename = "mali_predictedmean.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)


map_sigma = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = sigma), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "cividis"
    , name = expression(hat(sigma))
    , limits = c(0, 0.5)
  ) +
  theme_void()

ggsave(
  plot = map_sigma
  , filename = "mali_predictedsigma.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)



# plot the upper bounds for the matrix
plts_bounds = vector(mode = "list")

plts_bounds$lower = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = q010), color = NA) +
  scale_fill_viridis(option = "mako", limits = c(0L, 650L), direction = -1) +
  labs(title = "10%-Quantile") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

plts_bounds$upper = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = q090), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "mako"
    , direction = -1
    , name = "Estimated Prevalence"
    , limits = c(0L, 650L)
    , guide = guide_colorbar(
      direction = "horizontal"
      , barheight = unit(2L, units = "mm")
      , barwidth = unit(50, units = "mm")
      , label.hjust = 0L
      , title.position = "top"
      , title.hjust = 0.5
    )
  ) +
  labs(title = "90%-Quantile") +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

ggsave(
  plot = wrap_plots(plts_bounds, ncol = 1L) + plot_annotation(tag_levels = "A")
  , filename = "mali_predictedbounds.png"
  , path = file.path("results", "figures")
  , dpi = 600L, scale = 1, width = 200L, height = 260L
  , units = "mm", device = png, bg = "transparent"
)
