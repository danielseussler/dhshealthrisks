#
#
#
#

library(here)
library(ggplot2)
library(data.table)
library(Metrics)


# check simulation
sim = new.env()
load(file = here("models", "88qlclkf.rda"), envir = sim)

sim$results[, predc := as.integer(pred >= 0.5)]
sim$results[, method := paste(type, "\n", nfold, "-folds ", nrep, "-rep ", cvstrat, "-strat", sep = "")]
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
  by = list(holdout, method, iter)]

summary(sim$metr)
# sim$metrwide = dcast(sim$metr, holdout + iter ~ method, value.var = c("ncoef", "loss"))
# sim$metrwide[, `:=`("m1" = )]


ggplot(data = subset(sim$metr, holdout == "A"), aes(x = method, y = loss)) + 
  geom_line(aes(group = iter))
  
ggplot(data = subset(sim$metr, holdout == "A"), aes(x = interaction(method, cvstrat), y = ncoef)) + 
  geom_line(aes(group = iter))





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

ms$res[, predc := as.integer(pred.V1 >= 0.5)]
ms$res[, pred := pred.V1]
ms$metr = ms$res[,
                 .(loss = logLoss(actual, pred),
                   acc = accuracy(actual, predc),
                   ce = ce(actual, predc),
                   auc = auc(actual, pred),
                   brier = mse(actual, pred),
                   rec = recall(actual, predc),
                   pre = precision(actual, predc)),
                 by = list(model)]
ms$metr
