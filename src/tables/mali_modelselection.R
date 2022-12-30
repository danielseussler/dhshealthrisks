#
#
#
#

library(data.table)
library(gamlss.dist)
library(Metrics)
library(xtable)

# load results from the model selection file
load(file = file.path("models", "4r35hoz4.rda"))

# compute lower and upper 90% prediction intervals for binomial and beta binomial distribution
res[model %in% c("B"), q0025 := qBI(0.025, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q005 := qBI(0.05, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q010 := qBI(0.10, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q025 := qBI(0.25, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q075 := qBI(0.75, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q090 := qBI(0.90, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q095 := qBI(0.95, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("B"), q0975 := qBI(0.975, bd = n, mu = mu, lower.tail = TRUE, log.p = FALSE)]

res[model %in% c("A", "C"), q0025 := qBB(0.025, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q005 := qBB(0.05, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q010 := qBB(0.10, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q025 := qBB(0.25, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q075 := qBB(0.75, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q090 := qBB(0.90, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q095 := qBB(0.95, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]
res[model %in% c("A", "C"), q0975 := qBB(0.975, bd = n, mu = mu, sigma = sigma, lower.tail = TRUE, log.p = FALSE)]

res[, coverage50 := ifelse((q025 <= k) & (k <= q075), 1L, 0L)]
res[, coverage80 := ifelse((q010 <= k) & (k <= q090), 1L, 0L)]
res[, coverage90 := ifelse((q005 <= k) & (k <= q095), 1L, 0L)]
res[, coverage95 := ifelse((q0025 <= k) & (k <= q0975), 1L, 0L)]

res[model == "A", model := "Beta binomial"]
res[model == "B", model := "Binomial"]
res[model == "C", model := "Beta binomial w/ boosted trees"]

# compute metrics
res = res[, .(bias = mean(k/n - mu), mae = mae(k/n, mu), rmse = rmse(k/n, mu), coverage50 = mean(coverage50),
              coverage80 = mean(coverage80), coverage90 = mean(coverage90), coverage95 = mean(coverage95)), by = .(model)]

# print table
print(xtable(res, type = "latex"), file = file.path("results", "tables", "tab_mli_modelselection.tex"), include.rownames = FALSE)
