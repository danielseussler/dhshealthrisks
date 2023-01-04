# simulation study 2
#
# draw repeated 2:1 initial splits and test the out-of-sample performance (on average) of different
# resampling strategies but for fixed boosting iteration to assess bias and variance of selected resampling techniques

library(here)
library(mboost)
library(Metrics)
library(data.table)
library(surveyCV)
library(purrr)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "ethiopia", "surveydata.rda"))
source(file = here("src", "analysis", "ethiopia", "formula.R"))
source(file = here("src", "utils", "func_purrr_progress.R"))


# holdout folds: 1/3
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

ITER = 50L # number of simulation interations

sv$moderatelyx = with(sv, ifelse(haz < -200, 1L, 0L))
sv$moderatelyf = factor(sv$moderatelyx, levels = c(0L, 1L), labels = c("no", "yes"))

holdout.A = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))
holdout.B = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = NULL))


# define simulation, estimate holdout risk and cv risk for sampled boosting iteration and fixed
# resampling technique, evaluate cv at iteration only for comp. efficiency

sim = function(.type, .iter, .holdout) {
  
  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)
  
  trainingData = sv[which(initialSplit[, .iter] != 3L), ]
  testData = sv[which(initialSplit[, .iter] == 3L), ]
  
  mod = gamboost(
    formula = update(frml.1, moderatelyf ~ .)
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 500L, nu = 0.1, trace = FALSE)
  )
  
  cvFolds = switch(
    .type
    , "A" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = NULL)
    , "B" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = trainingData$strata)
    , "C" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = NULL)
    , "D" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = trainingData$strata)
    , "E" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "F" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "G" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "H" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "I" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL)
    , "J" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata)
    , "K" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL, prob = 0.8)
    , "L" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata, prob = 0.8)
    , "M" = 1L * as.matrix(1L == replicate(25, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )
  
  # extract  estimate and true risk over holdout set
  # do this for stopping time we want to check
  cv_risk = cvrisk(object = mod, grid = 500L, folds = cvFolds)
  cv_risk = unname(colMeans(cv_risk))
  true_risk = logLoss(testData$moderatelyx, predict(object = mod, newdata = testData, type = "response"))
  
  # .632 rule for bootstrap
  if (.type == "G" | .type == "H") cv_risk = 0.632 * cv_risk + 0.368 * tail(risk(mod) / nrow(trainingData), n = 1L)
  
  dt = data.table(
    holdout = .holdout
    , type = .type
    , iter = .iter
    , cv_risk = cv_risk
    , true_risk = true_risk
  )
  
  return(dt)
}

# define benchmark
bench = data.frame(
  .type = rep(LETTERS[1:13], each = ITER)
  , .iter = rep(1:ITER, 13L)
)

res_a = pmap_with_progress(bench, ~ sim(..1, ..2, "A")) |> rbindlist()
res_b = pmap_with_progress(bench, ~ sim(..1, ..2, "B")) |> rbindlist()

res_moderately = rbindlist(list(res_a, res_b))



# repeat the same for severely stunted children
sv$severelyx = with(sv, ifelse(haz < -300, 1L, 0L))
sv$severelyf = factor(sv$severelyx, levels = c(0L, 1L), labels = c("no", "yes"))

sim = function(.type, .iter, .holdout) {
  
  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)
  
  trainingData = sv[which(initialSplit[, .iter] != 3L), ]
  testData = sv[which(initialSplit[, .iter] == 3L), ]
  
  mod = gamboost(
    formula = update(frml.1, severelyf ~ .)
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 500L, nu = 0.1, trace = FALSE)
  )
  
  cvFolds = switch(
    .type
    , "A" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = NULL)
    , "B" = cv(weights = model.weights(mod), type = "kfold", B = 5L, strata = trainingData$strata)
    , "C" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = NULL)
    , "D" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = trainingData$strata)
    , "E" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "F" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "G" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL)
    , "H" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata)
    , "I" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL)
    , "J" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata)
    , "K" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL, prob = 0.8)
    , "L" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = trainingData$strata, prob = 0.8)
    , "M" = 1L * as.matrix(1L == replicate(25, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )
  
  # extract  estimate and true risk over holdout set
  # do this for stopping time we want to check
  cv_risk = cvrisk(object = mod, grid = 500L, folds = cvFolds)
  cv_risk = unname(colMeans(cv_risk))
  true_risk = logLoss(testData$severelyx, predict(object = mod, newdata = testData, type = "response"))
  
  # .632 rule for bootstrap
  if (.type == "G" | .type == "H") cv_risk = 0.632 * cv_risk + 0.368 * tail(risk(mod) / nrow(trainingData), n = 1L)
  
  dt = data.table(
    holdout = .holdout
    , type = .type
    , iter = .iter
    , cv_risk = cv_risk
    , true_risk = true_risk
  )
  
  return(dt)
}

bench = data.frame(
  .type = rep(LETTERS[1:13], each = ITER)
  , .iter = rep(1:ITER, 13L)
)

res_a = pmap_with_progress(bench, ~ sim(..1, ..2, "A")) |> rbindlist()
res_b = pmap_with_progress(bench, ~ sim(..1, ..2, "B")) |> rbindlist()

res_severly = rbindlist(list(res_a, res_b))

save(res_moderately, res_severly, file = here("models", "avq78t6q.rda"))
