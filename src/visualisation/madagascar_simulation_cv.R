# figures
#
#
#

library(here)
library(Metrics)
library(data.table)
library(ggplot2)
library(patchwork)


# load simulation data
# overlay boxplots of empirical distribution of predicited oob risk with empirical distr of true risk


load(file = here("models", "k30w39r3.rda"))
# load(file = here("models", "k30w39r3severe.rda"))
# load(file = here("models", "k30w39r3gaussian.rda"))
# load(file = here("models", "sd0s5lew.rda"))
results[, risk_diff := true_risk - cv_risk]
results[, risk_diff_ratio := (true_risk - cv_risk) / true_risk]

results[, as.list(summary(risk_diff)), by = .(type)]
results[, as.list(sd(risk_diff)), by = .(type)]

results[, name := type]
results[type == "k-fold stratified", name := "k-fold\nsurvey stratified"]
results[type == "bootstrap stratified", name := "bootstrap\nsurvey stratified"]
results[type == "subsampling survey cluster", name := "subsampling\ncluster + survey strat."]



trueQuantilesA = quantile(subset(results, holdout == "A")$true_risk, probs = c(0.05, 0.95))
plt.A = ggplot(data = subset(results, holdout == "A"), mapping = aes(x = name, y = cv_risk)) +
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = trueQuantilesA[1], ymax = trueQuantilesA[2]), fill = "grey", color = NA, alpha = 0.2) +
  geom_boxplot(alpha = 0.5) +
  geom_boxplot(aes(y = true_risk), alpha = 0.5, color = "red") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(x = "", y = expression(italic(hat(R)))) +
  facet_grid(~holdout, scales = "free") +
  theme_bw()


trueQuantilesB = quantile(subset(results, holdout == "B")$true_risk, probs = c(0.05, 0.95))
plt.B = ggplot(data = subset(results, holdout == "B"), mapping = aes(x = name, y = cv_risk)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = trueQuantilesB[1], ymax = trueQuantilesB[2]), fill = "grey", color = NA, alpha = 0.2) +
  geom_boxplot(alpha = 0.5) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(limits = c(0.615, 0.655)) +
  labs(x = "", y = "") +
  facet_grid(~holdout, scales = "free") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


ggsave(
  plot = plt.A + plt.B
  , filename = "fig_mdg_simulation_3.png"
  , path = here("results", "figures")
  , scale = 1.4
  , dpi = 600
  , width = 200
  , height = 80
  , units = "mm"
  , device = png
)
