# figures
#
#
#

library(ggplot2)
library(viridis)
library(patchwork)
library(sf)
library(gamboostLSS)
library(gamlss.dist)


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


# plot cross validation
png(filename = file.path("results", "figures", "mali_cvrisk.png"),
    width = 200L, height = 80L, units = "mm", res = 96L)
plot(cv_risk, main = "10-fold cross-validation\nstratified by survey strata")
dev.off()


# plot partial effects panel
plts = vector(mode = "list")

plts$A = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "log_pop", .rugged = TRUE, .title = expression(mu), .xlab = "Population (log)") + theme_classic()
plts$B = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "precip", .rugged = TRUE, .title = expression(mu), .xlab = "Precipitation") + theme_classic()
plts$C = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "ndvi", .rugged = TRUE, .title = expression(mu), .xlab = "NDVI") + theme_classic()
plts$D = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "evi", .rugged = TRUE, .title = expression(mu), .xlab = "EVI") + theme_classic()
plts$E = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "elev", .rugged = TRUE, .title = expression(mu), .xlab = "Elevation") + theme_classic()
plts$G = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "lstnight", .rugged = TRUE, .title = expression(mu), .xlab = "LST (night)") + theme_classic()
plts$H = plt_smooth(.mod = mod, .data = cl, .parameter = "mu", .var = "lstday", .rugged = TRUE, .title = expression(mu), .xlab = "LST (day)") + theme_classic()

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
  , dpi = 600L, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)


# plot the predicted parameter of the response distribution
pred_sf = h3_to_geo_boundary_sf(pred$h3_index)
pred_sf = cbind(pred_sf, pred)

plt = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = mu), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = expression(hat(mu))
    , limits = c(0, 1)
  ) +
  theme_void()

ggsave(
  plot = plt
  , filename = "mali_predictedmean.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)


plt = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = sigma), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "cividis"
    , name = expression(hat(sigma))
    , limits = c(0, 0.5)
  ) +
  theme_void()

ggsave(
  plot = plt
  , filename = "mali_predictedsigma.png"
  , path = file.path("results", "figures")
  , dpi = 600L, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)
