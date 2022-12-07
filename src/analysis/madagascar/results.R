#
#
#
#

library(here)
library(ggplot2)
library(data.table)


# check simulation
sim = new.env()
load(file = here("models", "88qlclkf.rda"), envir = sim)

sim$results[, predc := as.integer(pred >= 0.5)]
sim$results[, method := paste(type, "\n", nfold, "-folds ", nrep, "-rep", sep = "")]
sim$metr = sim$results[,
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
summary(sim$metr)


# check quasi stability selection
qss = new.env()
load(file = here("models", "wt79adpc.rda"), envir = qss)

qss$cnt = qss$res[, .(count = .N), by = list(method, selected)][, prop := count / 50L]
qss$cnt = dcast(qss$cnt, formula = selected ~ method, value.var = "count")
qss$cnt[, "rn > ss" := A > B]
qss$cnt


# check model selection
ms = new.env()
load(file = here("models", "lvcw0a2q.rda"), envir = ms)
