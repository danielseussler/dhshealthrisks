# figures for the main models
#
#
#

library(mboost)
library(ggplot2)
library(patchwork)
library(data.table)
library(Metrics)

# theme_update(
#   legend.background = element_rect(fill = "transparent", colour = NA)
#   , panel.background = element_rect(fill = "transparent", colour = NA)
#   , plot.background = element_rect(fill = "transparent", colour = NA)
# )

# load fitted models, dhs boundaries for spatial gmrf effect and helper functions
dhs_shp = readRDS(file = file.path("data", "processed", "madagascar", "dhsboundaries.rds"))
load(file = file.path("models", "h9h7v919.rda"))
load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "utils", "func_plot_partial_effects.R"))

sv$moderatelyx = with(sv, ifelse(haz < -200, 1L, 0L))
sv$moderatelyf = factor(sv$moderatelyx, levels = c(0L, 1L), labels = c("no", "yes"))

sv$severelyx = with(sv, ifelse(haz < -300, 1L, 0L))
sv$severelyf = factor(sv$severelyx, levels = c(0L, 1L), labels = c("no", "yes"))


# plot comparison of cv results
png(
  file = file.path("results", "figures", "fig_mdg_cvresults.png"),
  units = "mm", width = 210L, height = 80L, res = 96L, pointsize = 10
)
par(mfrow = c(1, 2))
plot(cv, main = "25-fold survey stratified cluster sampling")
plot(cv.kfold)
dev.off()


# plot variable importance before and after Deselection of learners that attributet low to the
# complete risk reduction
plot(varimp(mod), type = "blearner", nbars = 20L)
plot(varimp(mod.deselect), type = "blearner", nbars = 20L)

plts = vector(mode = "list")

plts$A = ggplot(data.frame(varimp(mod)), aes(variable, reduction, fill = blearner)) +
  geom_bar(stat = "identity") + coord_flip()

plts$B = ggplot(data.frame(varimp(mod.deselect)), aes(variable, reduction, fill = blearner)) +
  geom_bar(stat = "identity") + coord_flip()

ggsave(
  plot = wrap_plots(plts, ncol = 1L) +  plot_annotation(tag_levels = "A")
  , filename = "fig_mdg_variableimportance.png"
  , path = file.path("results", "figures")
  , dpi = 600L, scale = 2, width = 200L, height = 200L
  , units = "mm", device = png, bg = "transparent"
)



# plot main effects
names(coef(mod.deselect))

plts = vector(mode = "list")
plts$A = plt_numeric(.mod = mod.deselect, .data = sv, .var = "cage", .rugged = FALSE, .xlab = "cage")
plts$B = plt_numeric(.mod = mod.deselect, .data = sv, .var = "medu", .rugged = FALSE, .xlab = "medu")

ggsave(
  plot = wrap_plots(plts, ncol = 2L) +  plot_annotation(tag_levels = "A")
  , filename = "fig_mdg_main_effects_smooth.png"
  , path = file.path("results", "figures")
  , dpi = 600L, scale = 1.3, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)


# plot spatial effect
theme_set(theme_void())
plt.spatial = plt_gmrf(.mod = mod.deselect, .data = sv, .var = "dhsregion", .shp = dhs_shp, .limscol = c(-0.8, 0.8))

ggsave(
  plot = plt.spatial
  , filename = "fig_mdg_main_effects_spatial.png"
  , path = file.path("results", "figures")
  , dpi = 600L, scale = 1.3, width = 200L, height = 100L
  , units = "mm", device = png, bg = "transparent"
)

