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

ITER = 100L # simulation interations
MSTOP = 2000L # max boosting iterations
frml = frml.1


# holdout folds: 1/3
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

sv = transform(sv, household = paste(cluster, household, sep = "_"))
holdout.A = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))
holdout.B = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = NULL))


get_regionalcv = function(.data){
  svyregions = unique(.data$region)
  foldsMat = matrix(data = 1L, nrow = nrow(.data), ncol = length(svyregions))
  for(i in 1:ncol(foldsMat)) foldsMat[.data$region == svyregions[i], i] = 0L
  return(foldsMat)
}


# define simulation, estimate holdout risk and cv risk for sampled boosting iteration and fixed
# resampling technique, evaluate cv at iteration only for comp. efficiency

sim = function(.type, .iter, .mstop, .holdout) {
  
  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)
  
  trainingData = sv[which(initialSplit[, .iter] != 3L), ]
  testData = sv[which(initialSplit[, .iter] == 3L), ]
  
  mod = gamboost(
    formula = frml
    , data = trainingData
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = .mstop, nu = 0.25, trace = FALSE)
  )
  
  cvFolds = switch(
    .type,
    "k-fold" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = NULL),
    "k-fold stratified" = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = trainingData$strata),
    "bootstrap" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = NULL),
    "bootstrap stratified" = cv(weights = model.weights(mod), type = "bootstrap", B = 25L, strata = trainingData$strata),
    "subsampling" = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = NULL),
    "subsampling survey cluster" = 1L * as.matrix(1L == replicate(25L, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster"))),
    "regional cv" = get_regionalcv(trainingData)
  )
  
  cv = cvrisk(object = mod, grid = .mstop, folds = cvFolds) 
  
  # extract resampled risk estimate and true risk over holdout set
  # do this for stopping time we want to check
  cv_risk = unname(colMeans(cv))
  true_risk = logLoss(testData$hazx, predict(object = mod, newdata = testData, type = "response"))
  
  dt = data.table(
    holdout = .holdout
    , type = .type
    , iter = .iter
    , mstop = .mstop
    , cv_risk = cv_risk
    , true_risk = true_risk
  )
  
  return(dt)
}


# sample stopping times
# gamboostLSS::make.grid(MSTOP, length.out = 200, min = 100, log = TRUE)
drawnSample = sample(x = 0:MSTOP, size = ITER, replace = TRUE)

bench = data.frame(
  .type = rep(c("k-fold", "k-fold stratified", "bootstrap", "bootstrap stratified", "subsampling", "subsampling survey cluster"), each = ITER), 
  .iter = rep(1:ITER, 6L), 
  .mstop = rep(drawnSample, 6L)
)

res.A = pmap_with_progress(bench, ~ sim(..1, ..2, ..3, "A")) |> rbindlist()
res.B = pmap_with_progress(bench, ~ sim(..1, ..2, ..3, "B")) |> rbindlist()
results = rbindlist(list(res.A, res.B))

save(results, file = here("models", "mqp6vhvp.rda"))
