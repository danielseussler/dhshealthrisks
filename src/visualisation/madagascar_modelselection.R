# check model selection
#
#
#

library(data.table)
library(Metrics)
library(ggplot2)

load(file = file.path("models", "lvcw0a2q.rda"))

res[, predc := as.integer(pred >= 0.5)]
res[model == "A", name := "Base model"]
res[model == "B", name := "Base model\n+ gender interaction"]
res[model == "C", name := "Base model\n+ urbanicity interaction"]
res[model == "D", name := "Boosted with\nregression trees"]

metricsSummary = res[,
                     .(mstop = min(mstop)
                       , loss = logLoss(actual, pred)
                       , acc = accuracy(actual, predc)
                       , ce = ce(actual, predc)
                       , auc = auc(actual, pred)
                       , brier = mse(actual, pred)
                       , rec = recall(actual, predc)
                       , pre = precision(actual, predc)),
                     by = list(name, iter)]

metricsSummary

plt = ggplot(data = metricsSummary, mapping = aes(x = name, y = loss)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "", y = expression(hat(R))) +
  theme_bw()

ggsave(
  plot = plt
  , filename = "madagascar_modelselection.png"
  , path = file.path("results", "figures")
  , dpi = 600
  , width = 200
  , height = 80
  , units = "mm"
  , device = png
)
