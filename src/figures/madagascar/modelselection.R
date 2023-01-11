# plot model selection
#
#
#

library(data.table)
library(Metrics)
library(ggplot2)

load(file = file.path("models", "lvcw0a2q.rda"))

results[, class := as.integer(pred >= 0.5)]
results[model == "A", name := "Base model"]
results[model == "B", name := "Base model\n+ gender interaction"]
results[model == "C", name := "Base model\n+ urbanicity interaction"]
results[model == "D", name := "Boosted w/\nregression trees"]
results[model == "E", name := "Linear model"]

metrics_summary = results[
  , .(loss = logLoss(actual, pred)
      , acc = accuracy(actual, class)
      , ce = ce(actual, class)
      , auc = auc(actual, pred)
      , brier = mse(actual, pred)
      , rec = recall(actual, class)
      , pre = precision(actual, class))
  , by = list(name, iter)
]

metrics_summary

ggplot(data = metrics_summary, mapping = aes(x = name, y = auc)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "", y = expression(hat(R))) +
  theme_minimal()

plt = ggplot(data = metrics_summary, mapping = aes(x = name, y = loss)) +
  geom_line(mapping = aes(group = iter), color = "grey") +
  geom_boxplot(alpha = 0.5, width = 0.5) +
  labs(x = "", y = expression(hat(R))) +
  theme_minimal()

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
