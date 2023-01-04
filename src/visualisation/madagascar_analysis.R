#
#
#
#

library(here)
library(ggplot2)
library(viridis)
library(data.table)
library(mboost)
library(Metrics)


# load files and helper functions
load(file = here("models", "h9h7v919.rda"))
shp = readRDS(file = here("data", "processed", "madagascar", "dhsboundaries.rds"))
source(file = here("src", "utils", "func_plot_partial_effects.R"))
source(file = here("src", "utils", "func_tidy_baselearner.R"))


# plot theme 
theme_set(theme_classic())
theme_update(
  legend.background = element_rect(fill = "transparent", colour = NA)
  , panel.background = element_rect(fill = "transparent", colour = NA)
  , plot.background = element_rect(fill = "transparent", colour = NA)
)


# variable importance 
variableImportance = rbind(
  cbind(as.data.frame(varimp(fittedModel)), deselect = "none")
  , cbind(as.data.frame(varimp(deselectModel)), deselect = "attributable")
  , cbind(as.data.frame(varimp(deselectModelCum)), deselect = "cumulative")
)

variableImportance$name = tidy_baselearner_names_str(char = variableImportance$blearner)
variableImportance$name = as.factor(variableImportance$name)
variableImportance$name = forcats::fct_reorder(variableImportance$name, variableImportance$reduction)
variableImportance = subset(variableImportance, name != "denom")

pltImportance = ggplot(data = variableImportance, mapping = aes(x = name, y = reduction, fill = deselect)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = viridis(n = 3, alpha = 0.8, begin = 0.3, end = 0.7)) +
  coord_flip() +
  labs(x = "Base-learner", y = "Risk reduction", fill = "Method") + 
  theme(legend.position=c(.9,.2))

ggsave(
  plot = pltImportance
  , filename = "fig_mdg_variableimportance.png"
  , path = here("results", "figures")
  , dpi = 600L
  , scale = 1
  , width = 200L
  , height = 200L
  , units = "mm"
  , device = png
  , bg = "transparent"
)


# plot partial effects panel
names()

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


# plot spatial GMRF effect
plt.gmrf = plt_gmrf(fittedModel, sv, .var = "dhsregion", .shp = shp, .limscol = c(-0.8, 0.8))

ggsave(
  plot = plt.gmrf
  , filename = "fig_mdg_main_effects_spatial.png"
  , path = here::here("results", "figures")
  , dpi = 600L
  , scale = 1.3
  , width = 200L
  , height = 100L
  , units = "mm"
  , device = png
  , bg = "transparent"
)
