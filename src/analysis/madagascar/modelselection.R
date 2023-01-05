# model selection
# by nested cross-validation
# test on non-linearities and interactions with boosted trees
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

# define outer folds on which models are evaluated
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

sv = subset(sv, strata != 2L) |> droplevels()
sv.folds = replicate(numFolds, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))


# that is the main model
sim.fml = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training_data = sv[which(sv.folds[, .iter] != 3L), ]
  test_data = sv[which(sv.folds[, .iter] == 3L), ]

  mod = gamboost(
    formula = .formula
    , data = training_data
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(weights = model.weights(mod), type = "kfold", B = 10, strata = training_data$strata)
  )

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = test_data$hazx
    , pred = c(predict(object = mod[mstop(cv)], newdata = test_data, type = "response"))
  )

  return(dt)
}

res_1 = map_with_progress(1:numFolds, ~ sim.fml("A", frml.1, .)) # base model
res_2 = map_with_progress(1:numFolds, ~ sim.fml("B", frml.2, .)) # with gendered effects
res_3 = map_with_progress(1:numFolds, ~ sim.fml("C", frml.3, .)) # with urban effects


# tree boosting to assess if higher order interactions are present in the data
sim.tree = function(.model = NULL, .formula = NULL, .iter = NULL) {

  training_data = sv[which(sv.folds[, .iter] != 3L), ]
  test_data = sv[which(sv.folds[, .iter] == 3L), ]

  mod = blackboost(
    formula = .formula
    , data = training_data
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 1000L, nu = 0.1, trace = FALSE)
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(weights = model.weights(mod), type = "kfold", B = 10, strata = training_data$strata)
  )

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = test_data$hazx
    , pred = c(predict(object = mod[mstop(cv)], newdata = test_data, type = "response"))
  )

  return(dt)
}

res_4 = map_with_progress(1:numFolds, ~ sim.tree("D", frml.tree, .))


# include a simple boosted linear model
sim.glm = function(.model = NULL, .iter = NULL) {

  training_data = sv[which(sv.folds[, .iter] != 3L), ]
  test_data = sv[which(sv.folds[, .iter] == 3L), ]

  mod = mboost(
    formula = update(frml.tree, . ~ . - denom)
    , data = training_data
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, trace = FALSE)
    , baselearner = "bols"
  )

  cv = cvrisk(
    object = mod
    , grid = seq(from = 25L, to = mstop(mod), by = 10L)
    , folds = cv(weights = model.weights(mod), type = "kfold", B = 10, strata = training_data$strata)
  )

  dt = data.table(
    model = .model
    , iter = .iter
    , mstop = mstop(cv)
    , actual = test_data$hazx
    , pred = c(predict(object = mod[mstop(cv)], newdata = test_data, type = "response"))
  )

  return(dt)
}

res_5 = map_with_progress(1:numFolds, ~ sim.glm("E", .))


# combine and save all predictions
results = list(res_1, res_2, res_3, res_4, res_5) |>
  lapply(rbindlist) |>
  rbindlist()

save(results, file = file.path("models", "lvcw0a2q.rda"))
