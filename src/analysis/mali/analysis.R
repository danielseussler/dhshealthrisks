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
source(file = here("src", "analysis", "mali", "formula.R"))

cl = dplyr::left_join(cl, cloc)
cl = dplyr::mutate(cl, pop = log(pop + 1.0))

# define outcome
y = matrix(c(cl$npos, cl$nneg), ncol = 2L)

# run model
mod = gamboostLSS(
  formula = frml.1
  , data = cl
  , families = as.families("BB")
  , method = "noncyclic"
  , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
)

# resampling based on survey stratified folds
cv = cvrisk(
  object = mod
  , folds = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = cl$strata)
  , grid = make.grid(max = 1000L, length.out = 500L, log = TRUE)
)

mod[mstop(cv)]
table(selected(mod)$mu)
table(selected(mod)$sigma)

names(coef(mod, parameter = "mu"))
names(coef(mod, parameter = "sigma"))

plot(mod)


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

plot(stab)



# check results


save(mod, pred, file = here("models", "9dkw7wyn.rda"))
