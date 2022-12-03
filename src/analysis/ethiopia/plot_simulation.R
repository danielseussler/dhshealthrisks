#
#
#
#

library(here)
library(data.table)
library(ggplot2)
library(Metrics)


theme_set(theme_light())

load(file = here("results", "x4qx59v1.rda"))

results[, predc := as.integer(pred >= 0.5)]
results[, strategy := paste(type, "\n", nfold, "-folds ", nrep, "-rep", sep = "")]


# compute metrics
metr = results[,
               .(ncoef = min(ncoef),
                 mstop = min(mstop),
                 loss = logLoss(actual, pred),
                 acc = accuracy(actual, predc),
                 ce = ce(actual, predc),
                 auc = auc(actual, pred),
                 brier = mse(actual, pred),
                 rec = recall(actual, predc),
                 pre = precision(actual, predc),
                 f1 = f1(actual, predc)),
               by = list(holdout, strategy, cvstrat, iter)]

summary(metr)



# results
# model complexity
tmp = melt(metr, id.vars = c("holdout", "strategy", "cvstrat"), measure.vars = c("ncoef", "mstop"))
plt.1 = ggplot(tmp, aes(x = strategy, y = value, color = cvstrat)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "#00204DFF", alpha = 0.6) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(variable ~ holdout, scales = "free") +
  theme(legend.position = "bottom")


# generalization error (true)
tmp = melt(metr, id.vars = c("holdout", "strategy", "cvstrat"), measure.vars = c("loss", "ce", "auc", "brier"))
plt.2 = ggplot(tmp, aes(x = strategy, y = value, fill = cvstrat)) +
  geom_boxplot(outlier.alpha = 0.3, fill = "#00204DFF", alpha = 0.6) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  facet_grid(variable ~ holdout, scales = "free") +
  theme(legend.position = "bottom")


ggsave(plot = plt.1, filename = "fig_sim_modelcomplexity_eth.png", path = here("figures"), dpi = 600, scale = 1.5, width = 200, height = 200, units = "mm", device = png)
ggsave(plot = plt.2, filename = "fig_sim_holdouterror_eth.png", path = here("figures"), dpi = 600, scale = 1.5, width = 200, height = 297, units = "mm", device = png)
