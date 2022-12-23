#
#
#
#

library(here)
library(data.table)
library(ggplot2)
library(viridis)
library(Metrics)

load(file = here("models", "x4qx59v1.rda"))

results[, predc := as.integer(pred >= 0.5)]
results[, method := paste(type, "\n", nfold, "-folds ", nrep, "-rep", sep = "")]
results = results[type != "regional cv"]

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

# k-fold fails to stop for some iterations, in total these are only a selected few and therefore ignored
metr[mstop == 1000 & method == "kfold\n10-folds 1-rep"]

# results model complexity generalization error (true)
tmp = melt(metr, id.vars = c("holdout", "method", "cvstrat"), measure.vars = c("ncoef", "mstop", "loss", "ce", "auc", "rec", "pre"))
tmp = tmp[method %in% c("bootstrap\n25-folds 1-rep", "kfold\n10-folds 1-rep", "subsampling\n25-folds 1-rep", "subsampling cluster\n25-folds 1-rep")]

plt = ggplot(tmp, aes(x = method, y = value, fill = cvstrat)) +
  geom_boxplot(outlier.alpha = 0.3, alpha = 0.6) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_grey(name = "Stratification") +
  facet_grid(variable ~ holdout, scales = "free") +
  labs(x = "", y = "") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave(
  plot = plt
  , filename = "fig_eth_simulation.png"
  , path = here("results", "figures")
  , dpi = 600
  , width = 200
  , height = 260
  , units = "mm"
  , device = png
)



# check if there are changes on strata level prediction improvement
# the variance of the strata level estimates is reduced marginally at best
# not pursued further

metrStrata = results[
  , .(ncoef = min(ncoef), mstop = min(mstop), loss = logLoss(actual, pred))
  , by = list(holdout, method, cvstrat, strata, iter)
]

metrStrata = metrStrata[method == "subsampling cluster\n25-folds 1-rep" | (method == "kfold\n10-folds 1-rep" & cvstrat == "none")]
metrStrata[, .(meanLoss = mean(loss), varLoss = var(loss)), by = .(holdout, method)]

ggplot(data = metrStrata, mapping = aes(x = strata, y = loss, fill = method)) +
  geom_boxplot() +
  facet_grid(~holdout, scales = "free") +
  theme_bw()
