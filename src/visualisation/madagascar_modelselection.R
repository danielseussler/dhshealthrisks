# check model selection
#
#
#

library(here)
library(data.table)
library(Metrics)
library(ggplot2) 

theme_set(theme_bw())

load(file = here("models", "lvcw0a2q.rda"))

res[, predc := as.integer(pred >= 0.5)]
res[model == "A", name := "Base model"]
res[model == "B", name := "Base model\n+ gendered effects"]
res[model == "C", name := "Base model\n+ urbanicity effects"]
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


summary(metricsSummary)

plt = ggplot(data = metricsSummary, mapping = aes(x = name, y = loss)) + 
  geom_line(mapping = aes(group = iter), color = "grey") + 
  geom_boxplot(alpha = 0.5) + 
  labs(x = "", y = "Loss")

ggsave(plot = plt, filename = "fig_mdg_modelselection.png", path = here("results", "figures"), scale = 1.2, dpi = 600, width = 200, height = 100, units = "mm", device = png)
