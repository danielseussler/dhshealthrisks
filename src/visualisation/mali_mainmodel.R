#
#
#
#

library(here)
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
source(file = here("src", "utils", "func_plot_partial_effects.R"))


# plot partial effects from main model
load(file = here("models", "9dkw7wyn.rda"))
shp = readRDS(file = here("data", "processed", "mali", "dhsboundaries.rds"))

table(mod$mu$xselect())
table(mod$sigma$xselect())

names(coef(mod, parameter = "mu"))
names(coef(mod, parameter = "sigma"))

table(selected(mod)$mu)
table(selected(mod)$sigma)



# plot partial effects panel
plts = vector(mode = "list")

plts$A = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "pop", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "Population (log)")
plts$B = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "precip", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "Precipitation")
plts$C = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "ndvi", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "NDVI")
plts$D = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "evi", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "EVI")
plts$E = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "elev", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "Elevation")
plts$G = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "lstnight", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "LST (night)")
plts$H = plt_numeric(.mod = mod, .data = cl, .parameter = "mu", .var = "lstday", .rugged = TRUE, .title = expression(logit(mu)), .xlab = "LST (day)")

patch = wrap_plots(plts, ncol = 2L) + plot_annotation(tag_levels = "A")
ggsave(plot = patch, filename = "fig_mli_main_effects.png", path = here("results", "figures"), dpi = 600L, width = 210L, height = 210L, units = "mm", device = png, bg = "transparent")



# coefficient table for categorical covariates
coef(mod, parameter = "mu", which = "urban")$`bols(urban)`
coef(mod, parameter = "mu", which = "climate")$`bols(climate)`
coef(mod, parameter = "sigma", which = "urban")$`bols(urban)`



# do the spatial partial effect plots
plts.spatial = vector(mode = "list")

plts.spatial$mu = plt_spatial_s(.mod = mod, .data = cl, .parameter = "mu", .shp = shp, .limscol = c(-4.4, 4), .title = expression(logit(mu)))
plts.spatial$mu = plts.spatial$mu + theme_void() + theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

plts.spatial$sigma = plt_spatial_s(.mod = mod, .data = cl, .parameter = "sigma", .shp = shp, .limscol = c(-4.4, 4), .title = expression(log(sigma)))
plts.spatial$sigma = plts.spatial$sigma + theme_void() + theme(plot.title = element_text(hjust = 0.5))

patch.spatial = wrap_plots(plts.spatial, ncol = 2L) + plot_annotation(tag_levels = "A")

ggsave(plot = patch.spatial, filename = "fig_mli_main_effects_spatial.png", path = here("results", "figures"), dpi = 600L, scale = 1.3, width = 200L, height = 100L, units = "mm", device = png, bg = "transparent")


# plot the maps for the predicted params of the response distribution
pred_sf = h3_to_geo_boundary_sf(pred$h3_index)
pred_sf = cbind(pred_sf, pred)

plts.preds.mean = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = mean), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "viridis"
    , name = "Estimated Prevalence per 1000"
    , limits = c(0L, 800L)
    , guide = guide_colorbar(
      direction = "horizontal"
      , barheight = unit(2L, units = "mm")
      , barwidth = unit(50, units = "mm")
      , label.hjust = 0L
      , title.position = "top"
      , title.hjust = 0.5
    )
  ) + 
  theme_void() +
  theme(legend.position = "bottom")

ggsave(plot = plts.preds.mean, filename = "fig_mli_main_predicted.png", path = here("results", "figures"), dpi = 600L, width = 200L, height = 200L, units = "mm", device = png, bg = "transparent")


plts.preds.sigma = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = sigma), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "cividis"
    , name = expression(hat(sigma))
    , limits = c(0, 0.5)
  ) + 
  theme_void()  

ggsave(plot = plts.preds.sigma, filename = "fig_mli_main_predicted_sigma.png", path = here("results", "figures"), dpi = 600L, width = 200L, height = 100L, units = "mm", device = png, bg = "transparent")




# do the uncertainty plots for the appendix
plts.ucq = vector(mode = "list")

plts.ucq$lower = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = lower), color = NA) +
  scale_fill_viridis(option = "cividis", limits = c(0L, 1000L)) +
  labs(title = "5%-Quantile") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) 

plts.ucq$upper = ggplot(data = pred_sf) +
  geom_sf(mapping = aes(fill = upper), color = NA) +
  scale_fill_continuous(
    type = "viridis"
    , option = "cividis"
    , name = "Estimated Prevalence per 1000"
    , limits = c(0L, 1000L)
    , guide = guide_colorbar(
      direction = "horizontal"
      , barheight = unit(2L, units = "mm")
      , barwidth = unit(50, units = "mm")
      , label.hjust = 0L
      , title.position = "top"
      , title.hjust = 0.5
    )
  ) + 
  labs(title = "95%-Quantile") +  
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))
 

patch.ucq = wrap_plots(plts.ucq, ncol = 1L) + plot_annotation(tag_levels = "A")

ggsave(plot = patch.ucq, filename = "fig_mli_main_uncertainty.png", path = here("results", "figures"), dpi = 600L, scale = 1.3, width = 200L, height = 260L, units = "mm", device = png, bg = "transparent")
