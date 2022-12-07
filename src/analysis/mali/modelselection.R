#
#
#
#

library(here)
library(data.table)
library(mboost)
library(gamboostLSS)
library(gamlss.dist)
library(rsample)
library(party)
library(Metrics)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "mali", "surveydata.rda"))
load(file = here("data", "processed", "mali", "geodata.rda"))
source(file = here("src", "analysis", "mali", "formula.R"))
cl = dplyr::left_join(cl, cloc)


# define outer folds on which models are evaluated
rsp = vfold_cv(data = cl, v = 10L, repeats = 1L, strata = strata)


# distributional regression with beta-binomial distributed outcome
res.1 = res.2 = res.3 = data.table()

for (i in 1:nrow(rsp)) {

  dtrain = analysis(rsp$splits[[i]])
  dtest = assessment(rsp$splits[[i]])
  y = matrix(c(dtrain$npos, dtrain$nneg), ncol = 2L)

  mod = gamboostLSS(
    formula = frml.1
    , data = dtrain
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
  )
  
  cv = cvrisk(
    object = mod
    # , grid = make.grid(max = 1000L, length.out = 100L, log = TRUE)
    , grid = 1:mstop(mod)
    , folds = cv(model.weights(mod), type = "subsampling", B = 25L, strata = dtrain$strata)
  )

  mod[mstop(cv)]

  pred = predict(object = mod, newdata = dtest, type = "response")

  res.1 = rbindlist(list(res.1, data.table(model = "A", id = i, mstop = mstop(cv), k = dtest$npos, n = dtest$n, mu = pred$mu, sigma = pred$sigma[, 1L])))

}


# binomial outcome distribution (i.e. no cluster overdispersion)

for (i in 1:nrow(rsp)) {

  dtrain = analysis(rsp$splits[[i]])
  dtest = assessment(rsp$splits[[i]])
  y = matrix(c(dtrain$npos, dtrain$nneg), ncol = 2L)

  mod = gamboost(
    formula = frml.1$mu
    , data = dtrain
    , family = Binomial(type = "glm")
    , control = boost_control(mstop = 1000L, nu = 0.1, trace = TRUE)
  )

  cv = cvrisk(
    object = mod
    # , grid = make.grid(max = 1000L, length.out = 100L, log = TRUE)
    , grid = 1:mstop(mod)
    , folds = cv(model.weights(mod), type = "subsampling", B = 25L, strata = dtrain$strata)
  )

  mod[mstop(cv)]

  pred = predict(object = mod, newdata = dtest, type = "response")

  res.2 = rbindlist(list(res.2, data.table(model = "B", id = i, mstop = mstop(cv), k = dtest$npos, n = dtest$n, mu = pred[, 1L], sigma = NA)))

}


# tree boosting to assess if higher order interactions are present in the data

for (i in 1:nrow(rsp)) {

  dtrain = analysis(rsp$splits[[i]])
  dtest = assessment(rsp$splits[[i]])
  y = matrix(c(dtrain$npos, dtrain$nneg), ncol = 2L)

  mod = blackboostLSS(
    formula = frml.tree
    , data = dtrain
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 500L, nu = 0.1, trace = TRUE)
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  cv = cvrisk(
    object = mod
    # , grid = make.grid(max = 500L, length.out = 100L, log = TRUE)
    , grid = 1:mstop(mod)
    , folds = cv(model.weights(mod), type = "subsampling", B = 25L, strata = dtrain$strata)
  )

  mod[mstop(cv)]

  pred = predict(object = mod, newdata = dtest, type = "response")

  res.3 = rbindlist(list(res.3, data.table(model = "C", id = i, mstop = mstop(cv), k = dtest$npos, n = dtest$n, mu = pred$mu, sigma = pred$sigma[, 1L])))

}


res = rbindlist(list(res.1, res.2, res.3))

save(res, file = here("models", "4r35hoz4.rda"))
