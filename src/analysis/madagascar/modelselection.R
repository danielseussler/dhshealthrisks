# comparison of different model definitions
# test on non-linearties and interactions with boosted trees
#
#

library(here)
library(data.table)
library(mboost)
library(rsample)
library(party)
library(purrr)
library(surveyCV)
library(Metrics)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "madagascar", "surveydata.rda"))
source(file = here("src", "analysis", "madagascar", "formula.R"))
source(file = here("src", "utils", "purrr_progress.R"))

ITER = 50L # number of repeats
ITER.ss = 25L # number of subsampling folds in inner folds


# define outer folds on which models are evaluated
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required
sv = subset(sv, strata != 2L) |> droplevels()
sv.folds = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))


# that is the main model
sim.1 = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training = sv[which(sv.folds[, .iter] != 3L), ]
  testing = sv[which(sv.folds[, .iter] == 3L), ]

  mod = gamboost(
    formula = .formula
    , data = training
    , family = Binomial()
    , control = boost_control(mstop = 1000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = 1:mstop(mod)
    , folds = 1L * as.matrix(1L == replicate(ITER.ss, folds.svy(training, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )

  mod[mstop(cv)]

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = testing$hazx
    , pred = predict(object = mod, newdata = testing, type = "response")
  )

  return(dt)
}

res.1 = map_with_progress(1:ITER, ~ sim.1("A", frml.1, .))
res.2 = map_with_progress(1:ITER, ~ sim.1("B", frml.2, .)) # with gendered effects
res.3 = map_with_progress(1:ITER, ~ sim.1("C", frml.3, .)) # with urban effects


# tree boosting to assess if higher order interactions are present in the data

sim.tree = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training = sv[which(sv.folds[, .iter] != 3L), ]
  testing = sv[which(sv.folds[, .iter] == 3L), ]

  mod = blackboost(
    formula = .formula
    , data = training
    , family = Binomial()
    , control = boost_control(mstop = 300L, nu = 0.01, trace = TRUE)
    , tree_controls = partykit::ctree_control(maxdepth = 3L, saveinfo = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = 1:mstop(mod)
    # , folds = cv(model.weights(mod), type  = "bootstrap", strata = training$strata)
    , folds = 1L * as.matrix(1L == replicate(ITER.ss, folds.svy(training, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )

  mod[mstop(cv)]

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = testing$hazx
    , pred = predict(object = mod, newdata = testing, type = "response")
  )

  return(dt)
}

res.tree = map_with_progress(1:ITER, ~ sim.tree("D", frml.tree, .))


# combine and save all predictions
res = list(res.1, res.2, res.3, res.tree) |>
  lapply(rbindlist) |>
  rbindlist()

save(res, file = here("models", "lvcw0a2q.rda"))