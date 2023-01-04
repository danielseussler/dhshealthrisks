#
#
#
#

library(mboost)
library(gamboostLSS)
library(gamlss.dist)
library(data.table)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "mali", "surveydata.rda"))
load(file = file.path("data", "processed", "mali", "geodata.rda"))
source(file = file.path("src", "analysis", "mali", "formula.R"))

cl = dplyr::left_join(cl, cloc)
cl = dplyr::mutate(cl, log_pop = log(pop + 1.0))
grid = dplyr::mutate(grid, log_pop = log(pop + 1.0))
pos = matrix(data = c(cl$npos, cl$nneg), ncol = 2L) # binomial outcome


# model
# fit distributional boosting model with beta binomial distribution
# resampling based on survey stratified folds subsampling the data

mod = gamboostLSS(
  formula = frml.1
  , data = cl
  , families = as.families("BB")
  , method = "noncyclic"
  , control = boost_control(mstop = 2000L, nu = 0.1, trace = TRUE)
)

cv = cvrisk(
  object = mod
  , grid = seq(from = 25L, to = mstop(mod), by = 10L)
  , folds = cv(model.weights(mod), type = "subsampling", B = 25L, strata = cl$strata, prob = 0.8)
)

plot(cv)


# predictions on country level grid
# add quantiles of the predicted response distribution at different levels
# conditional on grid level population, share of under 14 is 47% as by World Bank, assume 1*0.47*5/14
mod[mstop(cv)]

pred = predict(mod, newdata = grid, type = "response")
pred = data.table(h3_index = grid$h3_index, mu = pred$mu, sigma = c(pred$sigma), N = grid$pop)

pred[, q0025 := qBB(p = 0.025, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q005 := qBB(p = 0.05, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q010 := qBB(p = 0.10, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q025 := qBB(p = 0.25, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q050 := qBB(p = 0.50, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q075 := qBB(p = 0.75, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q090 := qBB(p = 0.90, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q095 := qBB(p = 0.95, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]
pred[, q0975 := qBB(p = 0.975, mu = mu, sigma = sigma, bd = N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)]


samp = replicate(n = 100L, rBB(n = 1L, mu = pred$mu, sigma = pred$sigma, bd = pred$N, fast = TRUE))
samp = data.table(admin1 = grid$admin1, admin2 = grid$admin2, N = as.integer(pred$N), samp)

samp.m = melt(samp, id.vars = c("admin1", "admin2", "N"), variable.name = "iter", value.name = "value")

samp.m[, prev := ifelse(is.na(value / N), 0, value / N)]
samp.m[, .(prev = mean(prev, na.rm = TRUE)), by = admin1]


# save results
fwrite(pred, file = file.path("results", "predictions", "mali_malaria_risk.csv"))
save(cl, pos, mod, cv, pred, file = file.path("models", "9dkw7wyn.rda"))
