#
#
#
#

library(here)
library(Metrics)
library(data.table)
library(ggplot2)

load(file = here("models", "nybm9n81.rda"))

# results = results[checkat %in% c(500, 750, 1000, 1250)]
results = results[checkat == 1000L]
results[, risk_diff := true_risk - cv_risk]


ggplot(data = results, mapping = aes(x = type, y = risk_diff)) + 
  geom_line(aes(group = iter), color = "grey") + 
  geom_abline(intercept = 0, slope = 0, color = "black") + 
  # geom_line(aes(y = true_risk), color = "red") + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(~ holdout, scales = "free") +
  labs(x = "", y = "")
  

ggplot(data = results, mapping = aes(x = type, y = mstop)) + 
  geom_line(aes(group = iter), color = "grey") + 
  geom_abline(intercept = 0, slope = 0, color = "black") + 
  # geom_line(aes(y = true_risk), color = "red") + 
  geom_boxplot(alpha = 0.5) + 
  facet_grid(~ holdout, scales = "free") +
  labs(x = "", y = "")

ggsave(plot = plt, filename = "fig_mdg_simulation_2.png", path = here("results", "figures"), scale = 1.2, dpi = 600, width = 200, height = 100, units = "mm", device = png)
