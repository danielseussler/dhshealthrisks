#
#
#
#

library(here)
library(data.table)
library(gamlss.dist)
library(metrics)
library(xtable)

# load results from the model comparison
load(file = here("models", "4r35hoz4.rda"))

# compute lower and upper 90% prediction intervals
res[mod %in% c("B"), lower := qBI(0.05, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[mod %in% c("B"), upper := qBI(0.95, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]

res[mod %in% c("A", "C"), lower := qBB(0.05, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[mod %in% c("A", "C"), upper := qBB(0.95, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]

res[, coverage := ifelse((lower <= k) & (k <= upper), 1L, 0L)]

# compute metrics 
tab = res[, .(bias = mean(k/n - mu), mae = mae(k/n, mu), rmse = rmse(k/n, mu), coverage = mean(coverage)), by = .(mod)]

print(xtable(tab, type = "latex"), file = here("results", "tables", "tab_mli_comparison.tex"), include.rownames = FALSE)
