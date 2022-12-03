#
#
#
#

library(here)
library(data.table)
library(gamboostLSS)
library(gamlss.dist)
library(rsample)
library(purrr)
library(party)

set.seed(seed = 1)
options("mc.cores" = 10)#detectCores())

load(file = here("data", "processed", "mali", "surveydata.rda"))
load(file = here("data", "processed", "mali", "geodata.rda"))
source(file = here("src", "analysis", "mali", "formula.R"))
cl = dplyr::left_join(cl, cloc)


# define outer folds on which models are evaluated
rsp = vfold_cv(data = cl, v = 10, repeats = 2, strata = strata)


# distributional regression with beta-binomial distributed outcome
res.1 = data.table(mod = "A", rep = rsp$id, fold = rsp$id2, rmse = NA, mstop = NA)

for (i in 1:nrow(rsp)) {

  train = analysis(rsp$splits[[i]])
  test = assessment(rsp$splits[[i]])
  y = matrix(c(train$npos, train$nneg), ncol = 2)

  mod = gamboostLSS(
    formula = frml.1
    , data = train
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
  )

  folds = cv(model.weights(mod), type = "kfold", strata = train$strata)
  grid = make.grid(max = 1000, length.out = 500L, log = TRUE)
  cv = cvrisk(object = mod, grid = grid, folds = folds)
  mod[mstop(cv)]

  pred = predict(object = mod, newdata = test, type = "response")
  rmse = mean((test$npos / test$n - pred$mu)**2)

  res.1$rmse[i] = rmse
  res.1$mstop[i] = mstop(cv)
}


# alternative formula
res.2 = data.table(mod = "B", rep = rsp$id, fold = rsp$id2, rmse = NA, mstop = NA)

for (i in 1:nrow(rsp)) {

  train = analysis(rsp$splits[[i]])
  test = assessment(rsp$splits[[i]])
  y = matrix(c(train$npos, train$nneg), ncol = 2)

  mod = gamboostLSS(
    formula = frml.2
    , data = train
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 1000L, nu = 0.25, trace = TRUE)
  )

  folds = cv(model.weights(mod), type = "kfold", strata = train$strata)
  grid = make.grid(max = 1000, length.out = 500L, log = TRUE)
  cv = cvrisk(object = mod, grid = grid, folds = folds)
  mod[mstop(cv)]

  pred = predict(object = mod, newdata = test, type = "response")
  rmse = mean((test$npos / test$n - pred$mu)**2)

  res.2$rmse[i] = rmse
  res.2$mstop[i] = mstop(cv)
}



# tree boosting to assess if higher order interactions are present in the data
res.3 = data.table(mod = "C", rep = rsp$id, fold = rsp$id2, rmse = NA, mstop = NA)

for (i in 1:nrow(rsp)) {

  train = analysis(rsp$splits[[i]])
  test = assessment(rsp$splits[[i]])
  y = matrix(c(train$npos, train$nneg), ncol = 2)

  mod = blackboostLSS(
    formula = frml.bb
    , data = train
    , families = as.families("BB")
    , method = "noncyclic"
    , control = boost_control(mstop = 500L, nu = 0.1, trace = TRUE)
    , tree_controls = partykit::ctree_control(maxdepth = 4, saveinfo = FALSE)
  )

  folds = cv(model.weights(mod), type = "kfold", strata = train$strata)
  grid = make.grid(max = 500L, length.out = 125L, log = TRUE)
  cv = cvrisk(object = mod, folds = folds, grid = grid)
  mod[mstop(cv)]

  pred = predict(object = mod, newdata = test, type = "response")
  rmse = mean((test$npos / test$n - pred$mu)**2)

  res.3$rmse[i] = rmse
  res.3$mstop[i] = mstop(cv)
}

res = rbindlist(list(res.1, res.2, res.3))
save(res, file = here("results", "4r35hoz4.rda"))
