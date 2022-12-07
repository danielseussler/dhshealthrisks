# comparison of selected model components under different resampling for 
# quasi stability selection
#
#

library(here)
library(mboost)
library(surveyCV)
library(data.table)
library(purrr)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "madagascar", "surveydata.rda"))
source(file = here("src", "analysis", "madagascar", "formula.R"))
source(file = here("src", "utils", "purrr_progress.R"))

ITER = 100L # number of subsampling iterations

# create 100 partitions of the data set, train on one half, validate on second
# this is computationally less demanding, also compatible with survey structure
# compare to random splits with respect to the frequency of selected covariates
folds.ss = 1L * as.matrix(1L == replicate(ITER, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
folds.rn = subsample(weights = rep(1, nrow(sv)), B = ITER, strata = NULL)
folds.st = subsample(weights = rep(1, nrow(sv)), B = ITER, strata = sv$strata)

# define simulation run
sim = function(.method = NULL, .folds = NULL, .iter = NULL) {
  
  mod = gamboost(
    formula = frml.1
    , data = sv
    , weights = .folds[, .iter]
    , family = Binomial()
    , control = boost_control(mstop = 2000L, nu = 0.1, trace = FALSE, risk = "oobag")
  )
  
  mod[which.min(mod$risk())]
  
  dt = data.table(
    method = .method
    , iter = .iter
    , mstop = which.min(mod$risk())
    , selected = names(coef(mod))
  )
  
  return(dt)
}

res.1 = map_with_progress(1:ITER, ~ sim("A", folds.rn, .))
res.2 = map_with_progress(1:ITER, ~ sim("B", folds.ss, .))
res.3 = map_with_progress(1:ITER, ~ sim("C", folds.st, .))

# combine and save all selected coefs
res = list(res.1, res.2, res.3) |>
  lapply(rbindlist) |>
  rbindlist()

all_coefs = labels(terms(frml.1))
  
save(res, all_coefs, file = here("models", "wt79adpc.rda"))
