#
#
#
#

library(data.table)
library(gamlss.dist)
library(ggplot2)

# load results from the model selection file
load(file = file.path("models", "4r35hoz4.rda"))

res[model %in% c("B"), score := dBI(k, bd = n, mu = mu, log = TRUE)]
res[model %in% c("A1", "A2", "A3", "C"), score := dBB(k, bd = n, mu = mu, sigma = sigma, log = TRUE)]

res[model == "A1", model := "Beta binomial f1"]
res[model == "A2", model := "Beta binomial f2"]
res[model == "A3", model := "Beta binomial f3"]
res[model == "B", model := "Binomial"]
res[model == "C", model := "Beta binomial w/ boosted trees"]

# compute metrics
res = res[, .(risk = -1 * mean(score)), by = .(model, id)]

ggplot(data = res, mapping = aes(x = model, y = risk)) +
  geom_line(mapping = aes(group = id), alpha = 0.3) + geom_boxplot()



