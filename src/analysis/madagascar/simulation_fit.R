# simulation study
#
# draw repeated 2:1 initial splits and test the out-of-sample performance (on average) of 
# different resampling strategies

library(here)
library(mboost)
library(data.table)
library(surveyCV)
library(purrr)

set.seed(seed = 1L)
options("mc.cores" = detectCores())
load(file = here("data", "processed", "madagascar", "surveydata.rda"))
source(file = here("src", "analysis", "madagascar", "formula.R"))

ITER = 100L # simulation interations
MSTOP = 2000L # max boosting iterations
frml = frml.1

# holdout folds: 1/3
# ideally one should not drop one strata from the analysis, but to conduct the
# resampling at least three clusters within a strata are required

sv = subset(sv, strata != 2L) |> droplevels()
sv = transform(sv, household = paste(cluster, household, sep = "_"))
holdout.A = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))
holdout.B = replicate(ITER, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = NULL))


# hyperparam selection by resampling strategies
# cross-validation and bootstrap each with different number of folds / stratification

sim.rs = function(.type, .B, .strata, .holdout, .iter) {

  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)

  dtrain = sv[which(initialSplit[, .iter] != 3L), ]
  dtest = sv[which(initialSplit[, .iter] == 3L), ]

  mod = gamboost(
    formula = frml
    , data = dtrain
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = MSTOP, nu = 0.25, trace = FALSE)
  )

  folds = switch(.strata,
    "none" = cv(weights = model.weights(mod), type = .type, B = .B, strata = NULL),
    "survey strata" = cv(weights = model.weights(mod), type = .type, B = .B, strata = dtrain$strata))

  cv = cvrisk(object = mod, folds = folds)

  mod[mstop(cv)]

  dt = data.table(
    holdout = .holdout
    , type = .type
    , nfold = .B
    , nrep = 1
    , cvstrat = .strata
    , iter = .iter
    , mstop = mstop(cv)
    , ncoef = length(names(coef(mod)))
    , strata = dtest$strata
    , actual = dtest$hazx
    , pred = c(predict(mod, newdata = dtest, type = "response"))
  )

  return(dt)
}

bench = data.frame(
  .type = c("bootstrap", "bootstrap", "kfold", "kfold", "subsampling", "subsampling"),
  .B = c(25L, 25L, 10L, 10L, 25L, 25L),
  .strata = c("survey strata", "none", "survey strata", "none", "survey strata", "none")
)

bench = do.call("rbind", replicate(2L, bench, simplify = FALSE))
bench$.holdout = rep(c("A", "B"), each = nrow(bench) / 2)
bench = do.call("rbind", replicate(ITER, bench, simplify = FALSE))
bench$.iter = rep(1:ITER, each = nrow(bench) / ITER)

res.rs = pmap(bench, ~ sim.rs(..1, ..2, ..3, ..4, ..5))


# custom folds for survey structure, 2-fold stable possible with cluster selection,
# we can additionally have cluster sampling partly of survey structure, namely
# only by regions OR urban/rural indicator

sim.sv = function(.cluster, .rep, .k, .holdout, .iter) {

  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)

  dtrain = sv[which(initialSplit[, .iter] != 3L), ]
  dtest = sv[which(initialSplit[, .iter] == 3L), ]

  mod = gamboost(
    formula = frml
    , data = dtrain
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = MSTOP, nu = 0.25, trace = FALSE)
  )

  # there are different options here. we can draw two-folds at most, as the min
  # count of clusters is 3 per stratum (one is already in the dtest set)
  # as shown above 2-fold is likely to variable, so I do repeated 2-fold here to average
  # over an larger number of folds. note: folds.svy returns vector of 1 2 1 2 -> transform for cv

  get_folds = function(.k, .strata, ...) {
    f = folds.svy(dtrain, nfold = .k, strataID = "strata", clusterID = .cluster)
    f = matrix(f, nrow = nrow(dtrain), ncol = .k, byrow = FALSE) != matrix(1:.k, nrow = nrow(dtrain), ncol = .k, byrow = TRUE)
    return(f * 1L)
  }

  folds.cv = map(1:.rep, ~ get_folds(.k, .strata, .))
  folds.cv = do.call("cbind", folds.cv)

  cv = cvrisk(object = mod, folds = folds.cv)
  mod[mstop(cv)]

  dt = data.table(
    holdout = .holdout
    , type = "survey kfold"
    , nfold = .k
    , nrep = .rep
    , cvstrat = "survey strata"
    , iter = .iter
    , mstop = mstop(cv)
    , ncoef = length(names(coef(mod)))
    , strata = dtest$strata
    , actual = dtest$hazx
    , pred = c(predict(mod, newdata = dtest, type = "response"))
  )

  return(dt)
}

bench = data.frame(.cluster = c("cluster", "cluster"), .rep = c(1L, 5L), .k = c(2L, 2L))
bench = do.call("rbind", replicate(2L, bench, simplify = FALSE))
bench$.holdout = rep(c("A", "B"), each = nrow(bench) / 2L)
bench = do.call("rbind", replicate(ITER, bench, simplify = FALSE))
bench$.iter = rep(1:ITER, each = nrow(bench) / ITER)

res.sv = pmap(bench, ~ sim.sv(..1, ..2, ..3, ..4, ..5))


# adaption of subsampling to the cluster sampling, important for the stability selection later
# this is equal to repeated 2-fold draws, trained on one fold and tested on second

sim.ss = function(.holdout, .iter) {

  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)

  dtrain = sv[which(initialSplit[, .iter] != 3L), ]
  dtest = sv[which(initialSplit[, .iter] == 3L), ]

  mod = gamboost(
    formula = frml
    , data = dtrain
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = MSTOP, nu = 0.25, trace = FALSE)
  )

  cv = cvrisk(
    object = mod
    , folds = 1L * as.matrix(1L == replicate(25L, folds.svy(dtrain, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )
  
  mod[mstop(cv)]

  dt = data.table(
    holdout = .holdout
    , type = "subsampling cluster"
    , nfold = 25L
    , nrep = 1L
    , cvstrat = "survey strata"
    , iter = .iter
    , mstop = mstop(cv)
    , ncoef = length(names(coef(mod)))
    , strata = dtest$strata
    , actual = dtest$hazx
    , pred = c(predict(mod, newdata = dtest, type = "response"))
  )

  return(dt)
}

bench = data.frame(.holdout = c("A", "B"), .iter = rep(1:ITER, each = 2L))
res.ss = pmap(bench, ~ sim.ss(..1, ..2))



# last, check out of region sample

sim.re = function(.holdout, .iter) {
  
  initialSplit = switch(.holdout, "A" = holdout.A, "B" = holdout.B)
  
  dtrain = sv[which(initialSplit[, .iter] != 3L), ]
  dtest = sv[which(initialSplit[, .iter] == 3L), ]
  
  mod = gamboost(
    formula = frml
    , data = dtrain
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = MSTOP, nu = 0.25, trace = FALSE)
  )
  
  svyregions = unique(dtrain$region)
  foldsMat = matrix(data = 1L, nrow = nrow(dtrain), ncol = length(svyregions))
  for(i in 1:ncol(foldsMat)) foldsMat[dtrain$region == svyregions[i], i] = 0L
  
  cv = cvrisk(object = mod, folds = foldsMat)
  
  mod[mstop(cv)]
  
  dt = data.table(
    holdout = .holdout
    , type = "regional cv"
    , nfold = 1L
    , nrep = 1L
    , cvstrat = "none"
    , iter = .iter
    , mstop = mstop(cv)
    , ncoef = length(names(coef(mod)))
    , strata = dtest$strata
    , actual = dtest$hazx
    , pred = c(predict(mod, newdata = dtest, type = "response"))
  )
  
  return(dt)
}

bench = data.frame(.holdout = c("A", "B"), .iter = rep(1:ITER, each = 2L))
res.re = pmap(bench, ~ sim.re(..1, ..2))

# combine and save all predictions
results = list(res.rs, res.sv, res.ss, res.re) |>
  lapply(rbindlist) |>
  rbindlist()

save(results, file = here("models", "88qlclkf.rda"))