# analysis
#
#
#

library(here)
library(gamboostLSS)
library(gamlss.dist)
library(data.table)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "mali", "surveydata.rda"))
load(file = here("data", "processed", "mali", "geodata.rda"))
source(file = here("source", "mali", "formula.R"))
cl = dplyr::left_join(cl, cloc)
y = matrix(c(cl$npos, cl$nneg), ncol = 2)

# run model
mod = gamboostLSS(
  formula = frml.3
  , data = cl
  , families = as.families("BB")
  , method = "noncyclic"
  , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
)

# cross-validate based with survey stratified folds
cv = cvrisk(
  object = mod
  , folds = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = cl$strata)
  , grid = make.grid(max = 1000L, length.out = 400L, log = TRUE)
)

mod[mstop(cv)]

# do predictions
pred = predict(mod, newdata = grid, type = "response")
pred = data.table(h3_index = grid$h3_index, mu = pred$mu, sigma = pred$sigma)

stab = stabsel(# FIXME
  x = mod
  , cutoff = 0.8
  , q = 10L
  , eval = TRUE
  , mstop = 2000L
  , sampling.type = "SS"
  , folds = subsample(model.weights(mod), B = 50L, strata = cl$strata)
)

stab.2 = stabsel(# FIXME
  x = mod
  , cutoff = 0.8
  , q = 10L
  , eval = TRUE
  , mstop = 2000L
  , sampling.type = "SS"
  , folds = subsample(model.weights(mod), B = 50L, strata = NULL)
)


# check results
summary(res[[1]]$cv)
plot(res[[1]]$cv)

summary(res[[1]]$mod)
plot(res[[1]]$mod)


save(mod, pred, file = here("models", "9dkw7wyn.rda"))
