#
#
#
#

library(here)
library(data.table)
library(gamlss.dist)
library(Metrics)
library(xtable)
library(patchwork)

source(file = here("src", "utils", "func_plot_partial_effects.R"))

# load results from the model comparison
mc = new.env()
load(file = here("models", "4r35hoz4.rda"))

# compute lower and upper 90% prediction intervals
res[model %in% c("B"), lower := qBI(0.05, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), upper := qBI(0.95, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]

res[model %in% c("A", "C"), lower := qBB(0.05, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), upper := qBB(0.95, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]

res[, coverage := ifelse((lower <= k) & (k <= upper), 1L, 0L)]

# compute metrics 
tab = res[, .(bias = mean(k/n - mu), mae = mae(k/n, mu), rmse = rmse(k/n, mu), coverage = mean(coverage)), by = .(model)]

print(xtable(tab, type = "latex"), file = here("results", "tables", "tab_mli_comparison.tex"), include.rownames = FALSE)


