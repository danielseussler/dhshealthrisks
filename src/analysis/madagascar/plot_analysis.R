#
#
#
#

library(here)
library(mboost)
library(ggplot2)
library(patchwork)
library(data.table)
library(Metrics)

theme_set(theme_classic())

theme_update(
  legend.background = element_rect(fill = "transparent", colour = NA)
  , panel.background = element_rect(fill = "transparent", colour = NA)
  , plot.background = element_rect(fill = "transparent", colour = NA)
)

# load shapes for gmrf effect plot
shp = readRDS(file = here("data", "processed", "madagascar", "dhsboundaries.rds"))

# load custom plot functions
source(file = here("src", "utils", "func_plot_partial_effects.R"))

# load fitted models
load(file = here("models", "h9h7v919.rda"))


# plot comparison of cv results
png(file = here("results", "figures", "fig_mdg_cvresults.png"), units = "mm", width = 210L, height = 80L, res = 96L, pointsize = 10)
par(mfrow = c(1, 2))
plot(cv, main = "25-fold survey stratified cluster sampling")
plot(cv.kfold)
dev.off()


# plot variable importance before and after Deselection of learners that attributet low to the 
# complete risk reduction 
plot(varimp(mod), type = "blearner", nbars = 20L)
plot(varimp(mod.ds.atr), type = "blearner", nbars = 20L)

plts = vector(mode = "list")

plts$A = ggplot(data.frame(varimp(mod)), aes(variable, reduction, fill = blearner)) + 
  geom_bar(stat = "identity") + coord_flip() 

plts$B = ggplot(data.frame(varimp(mod.ds.atr)), aes(variable, reduction, fill = blearner)) + 
  geom_bar(stat = "identity") + coord_flip() 

patch.plts = wrap_plots(plts, ncol = 1L) +  plot_annotation(tag_levels = "A")

ggsave(plot = patch.plts, filename = "fig_mdg_variableimportance.png", path = here::here("results", "figures"), dpi = 600L, scale = 2, width = 200L, height = 200L, units = "mm", device = png, bg = "transparent")


# plot main effects
names(coef(mod.ds.atr))

plts = vector(mode = "list")
plts$A = plt_numeric(.mod = mod.ds.atr, .data = sv, .var = "cage", .rugged = FALSE, .xlab = "cage")
plts$B = plt_numeric(.mod = mod.ds.atr, .data = sv, .var = "medu", .rugged = FALSE, .xlab = "medu")
patch.plts = wrap_plots(plts, ncol = 2) +  plot_annotation(tag_levels = "A")

ggsave(plot = patch.plts, filename = "fig_mdg_main_effects_smooth.png", path = here::here("results", "figures"), dpi = 600L, scale = 1.3, width = 200L, height = 100L, units = "mm", device = png, bg = "transparent")


# plot spatial effect
theme_set(theme_void())
plt.spatial = plt_gmrf(.mod = mod.ds.atr, .data = sv, .var = "dhsregion", .shp = shp, .limscol = c(-0.8, 0.8))

ggsave(plot = plt.spatial, filename = "fig_mdg_main_effects_spatial.png", path = here::here("results", "figures"), dpi = 600L, scale = 1.3, width = 200L, height = 100L, units = "mm", device = png, bg = "transparent")

