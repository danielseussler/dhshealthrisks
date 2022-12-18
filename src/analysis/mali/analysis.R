# analysis
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

# define outcome
y = matrix(c(cl$npos, cl$nneg), ncol = 2L)


# fit distributional boosting model with beta binomial distribution
# resampling based on survey stratified folds
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

mod[mstop(cv)]


# check results from stability selection (not discussed in the report)
# first compute number of learners overall 
(length(attr(terms(frml.1$mu), "term.labels")) + length(attr(terms(frml.1$sigma), "term.labels")))

stabsel_parameters(x = mod, p = 27, q = 15L, PFER = 1L, assumption = "unimodal")
stabsel_parameters(x = mod, p = 27, q = 10L, PFER = 1L, assumption = "r-concave")
stabsel_parameters(x = mod, p = 27, q = 10L, PFER = 1L, assumption = "none")

stab = stabsel(
  x = mod
  , q = 15L
  , PFER = 1L
  , mstop = 1000L
  , sampling.type = "SS"
  , folds = subsample(model.weights(mod), B = 50L, strata = cl$strata)
)

plot(stab, type = "path")
plot(stab, type = "maxsel")


# predictions on country level grid
pred = predict(mod, newdata = grid, type = "response")
pred = data.table(h3_index = grid$h3_index, mu = pred$mu, sigma = c(pred$sigma))

save(cl, mod, pred, stab, file = here("models", "9dkw7wyn.rda"))



# # also fit a binomial model to compare the predictive distributions
# mod.binom = gamboost(
#   formula = frml.1$mu
#   , data = cl
#   , family = Binomial(type = "glm")
#   , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
# )
# 
# cv.binom = cvrisk(
#   object = mod.binom 
#   , folds = cv(weights = model.weights(mod.binom), type = "subsampling", strata = cl$strata)
# )
# 
# mod.binom[mstop(cv.binom)]
# 
# save(cl, mod, mod.binom, file = here("models", "fg5kpezg.rda"))
