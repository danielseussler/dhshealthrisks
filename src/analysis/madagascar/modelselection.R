# model selection 
# by nested cross-validation
# test on non-linearties and interactions with boosted trees
# 

library(data.table)
library(mboost)
library(rsample)
library(party)
library(purrr)
library(surveyCV)
library(Metrics)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "func_purrr_progress.R"))

numFolds = 10L # number of repeats
numSS = 25L # number of subsampling folds in inner folds


# define outer folds on which models are evaluated
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

sv = subset(sv, strata != 2L) |> droplevels()
sv.folds = replicate(numFolds, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))


# that is the main model
sim.fml = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training = sv[which(sv.folds[, .iter] != 3L), ]
  testing = sv[which(sv.folds[, .iter] == 3L), ]

  mod = gamboost(
    formula = .formula
    , data = training
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 2000L, nu = 0.25, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(weights = model.weights(mod), type = "subsampling", B = numSS, strata = training$strata, prob = 0.8)
  )

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = testing$hazx
    , pred = c(predict(object = mod[mstop(cv)], newdata = testing, type = "response"))
  )

  return(dt)
}

res.1 = map_with_progress(1:numFolds, ~ sim.fml("A", frml.1, .)) # base model
res.2 = map_with_progress(1:numFolds, ~ sim.fml("B", frml.2, .)) # with gendered effects
res.3 = map_with_progress(1:numFolds, ~ sim.fml("C", frml.3, .)) # with urban effects


# tree boosting to assess if higher order interactions are present in the data

sim.tree = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training = sv[which(sv.folds[, .iter] != 3L), ]
  testing = sv[which(sv.folds[, .iter] == 3L), ]

  mod = blackboost(
    formula = .formula
    , data = training
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 1000L, nu = 0.1, trace = FALSE)
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(weights = model.weights(mod), type = "subsampling", B = numSS, strata = training$strata, prob = 0.8)
  )
  
  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = testing$hazx
    , pred = c(predict(object = mod[mstop(cv)], newdata = testing, type = "response"))
  )

  return(dt)
}

res.tree = map_with_progress(1:numFolds, ~ sim.tree("D", frml.tree, .))


# combine and save all predictions
res = list(res.1, res.2, res.3, res.tree) |>
  lapply(rbindlist) |>
  rbindlist()

save(res, file = file.path("models", "lvcw0a2q.rda"))
