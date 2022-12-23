# 
#
#
#

library(here)
library(mboost)
library(gamboostLSS)
library(gamlss.dist)
library(data.table)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "mali", "surveydata.rda"))
load(file = here("data", "processed", "mali", "geodata.rda"))
source(file = here("src", "analysis", "mali", "formula.R"))

cl = dplyr::left_join(cl, cloc)
cl = dplyr::mutate(cl, pop = log(pop + 1.0))
y = matrix(c(cl$npos, cl$nneg), ncol = 2L) # binomial outcome



# model
# fit distributional boosting model with beta binomial distribution
# resampling based on survey stratified folds subsampling the data

mod = gamboostLSS(
  formula = frml.1
  , data = cl
  , families = as.families("BB")
  , method = "noncyclic"
  , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
)

cv = cvrisk(
  object = mod
  , grid = 1:mstop(mod)
  , folds = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = cl$strata)
)

plot(cv)
mod[mstop(cv)]



# predictions on country level grid
# add quantiles of the predicted response distributin at 90%

pred = predict(mod, newdata = grid, type = "response")
pred = data.table(h3_index = grid$h3_index, mu = pred$mu, sigma = c(pred$sigma), N = 1000L)

pred$mean = pred$N * pred$mu
pred$lower = qBB(p = 0.05, mu = pred$mu, sigma = pred$sigma, bd = pred$N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)
pred$upper = qBB(p = 0.95, mu = pred$mu, sigma = pred$sigma, bd = pred$N, lower.tail = TRUE, log.p = FALSE, fast = TRUE)



# save data
fwrite(pred, file = here("results", "predictions", "malaria_risk.csv"))
save(cl, mod, cv, pred, file = here("models", "9dkw7wyn.rda"))
