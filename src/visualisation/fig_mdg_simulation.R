#
#
#
#

library(here)
library(data.table)
library(ggplot2)
library(viridis)
library(Metrics)

theme_set(theme_light())
load(file = here("models", "88qlclkf.rda"))

results[, predc := as.integer(pred >= 0.5)]
results[, method := paste(type, "\n", nfold, "-folds ", nrep, "-rep", sep = "")]

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
  by = list(holdout, method, cvstrat, iter)]

summary(metr)

# results model complexity generalization error (true)
tmp = melt(metr, id.vars = c("holdout", "method", "cvstrat"), measure.vars = c("ncoef", "mstop", "loss", "ce", "auc", "rec", "pre"))
plt.1 = ggplot(tmp, aes(x = method, y = value, fill = cvstrat)) +
  geom_boxplot(outlier.alpha = 0.3, alpha = 0.6) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values = viridis(n = 2, alpha = 0.9, begin = 0.3, end = 0.7), name = "Stratification") +
  facet_grid(variable ~ holdout, scales = "free") +
  labs(x = "Method", y = "") +
  theme(legend.position = "bottom")

ggsave(plot = plt.1, filename = "fig_mdg_simresults.png", path = here("results", "figures"), dpi = 600, scale = 0.9, width = 210, height = 297, units = "mm", device = png)
