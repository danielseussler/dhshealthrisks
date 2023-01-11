# figures
#
#
#

library(ggplot2)
library(viridis)
library(data.table)
library(patchwork)
library(mboost)
library(Metrics)

source(file = file.path("src", "utils", "func_plot_partial_effects.R"))
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))

load(file = file.path("models", "h9h7v919.rda"))
load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
madagascar_shp = readRDS(file = file.path("data", "processed", "madagascar", "dhsboundaries.rds"))


# plot cross validation
png(filename = file.path("results", "figures", "madagascar_moderately_cvrisk.png"),
    width = 200L, height = 80L, units = "mm", res = 96L)
plot(cv_risk, main = "10-fold cross-validation\nstratified by survey strata")
dev.off()


# plot spatial GMRF effect
plt = plt_gmrf(fitted_model, sv, .var = "dhsregion", .shp = madagascar_shp) + theme_void()

ggsave(
  plot = plt
  , filename = "madagascar_moderately_spatial.png"
  , path = file.path("results", "figures")
  , dpi = 600L
  , width = 200L
  , height = 100L
  , units = "mm"
  , device = png
  , bg = "transparent"
)


# plot partial effects panel
names(coef(fitted_model))

continuous_cov = c("cage", "mbmi", "mage", "medu", "healthaccess", "cityaccess")
plts = lapply(
  X = continuous_cov
  , FUN = function(x) plt_smooth(.mod = fitted_model, .data = sv, .var = x, .rugged = TRUE, .xlab = x) + theme_classic()
)

wrap_plots(plts, ncol = 2) + plot_annotation(tag_levels = "A")

ggsave(
  plot = wrap_plots(plts, ncol = 2) + plot_annotation(tag_levels = "A")
  , filename = "madagascar_moderately_smooth.png"
  , path = file.path("results", "figures")
  , dpi = 600L
  , width = 200L
  , height = 200L
  , units = "mm"
  , device = png
  , bg = "transparent"
)



# variable importance
var_importance = rbind(
  cbind(as.data.frame(varimp(fitted_model)), deselect = "none")
  , cbind(as.data.frame(varimp(deselect_model)), deselect = "attributable")
  , cbind(as.data.frame(varimp(deselect_cum_model)), deselect = "cumulative")
)

var_importance$name = tidy_baselearner_names_str(char = var_importance$blearner)
var_importance$name = as.factor(var_importance$name)
var_importance$name = forcats::fct_reorder(var_importance$name, var_importance$reduction)
var_importance = subset(var_importance, name != "denom")

plt = ggplot(data = var_importance, mapping = aes(x = name, y = reduction, fill = deselect)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = viridis(n = 3, alpha = 0.8, begin = 0.3, end = 0.7)) +
  coord_flip() +
  labs(x = "Base-learner", y = "Risk reduction", fill = "Method") +
  theme(legend.position=c(.9,.2))

ggsave(
  plot = plt
  , filename = "madagascar_moderately_varimportance.png"
  , path = file.path("results", "figures")
  , dpi = 600L
  , scale = 1
  , width = 200L
  , height = 200L
  , units = "mm"
  , device = png
  , bg = "transparent"
)



