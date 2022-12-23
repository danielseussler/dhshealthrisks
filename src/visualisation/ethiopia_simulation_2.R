#
#
#
#

library(here)
library(Metrics)
library(data.table)
library(ggplot2)

# here I compare the estimated oob loss with the observed oob loss for sampled stopping times
# 

load(file = here("models", "mqp6vhvp.rda"))
results[, risk_diff := true_risk - cv_risk]
results[, risk_diff_ratio := (true_risk - cv_risk) / true_risk]

results[, as.list(summary(risk_diff)), by = .(type)]
results[, as.list(sd(risk_diff)), by = .(type)]

results[, name := type]
results[type == "k-fold stratified", name := "k-fold\nsurvey stratified"]
results[type == "bootstrap stratified", name := "bootstrap\nsurvey stratified"]
results[type == "subsampling survey cluster", name := "subsampling\ncluster + survey strat."]


plt = ggplot(data = results, mapping = aes(x = name, y = risk_diff_ratio)) + 
  geom_line(aes(group = iter), color = "grey") + 
  geom_abline(intercept = 0, slope = 0, color = "black") + 
  geom_boxplot(alpha = 0.5) + 
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = expression((italic(R)- hat(italic(R))) / italic(R))) +
  facet_grid(~ holdout, scales = "free") +
  theme_bw()

ggsave(
  plot = plt
  , filename = "fig_eth_simulation_2.png"
  , path = here("results", "figures")
  , scale = 1.4
  , dpi = 600
  , width = 200
  , height = 80
  , units = "mm"
  , device = png
)