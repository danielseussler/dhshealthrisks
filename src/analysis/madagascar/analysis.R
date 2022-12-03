#
#
#
#

library(here)
library(mboost)
library(surveyCV)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here::here("data", "cleaned", "madagascar", "surveydata.rda"))
source(file = here::here("source", "madagascar", "formula.R"))
source(file = here::here("utils", "DeselectBoost.R"))

# estimate model
mod = gamboost(
  formula = frml.1
  , data = sv
  , family = Binomial(type = "adaboost", link = "logit")
  , control = boost_control(mstop = 1000L, nu = 0.1)
)


# resample model to obtain
cv = cvrisk(
  object = mod
  , folds = 1L * as.matrix(1L == replicate(25L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

plot(cv, main = "25-fold survey stratified cluster sampling")


# stability selection
stabsel_parameters(x = mod, q = 20L, PFER = 5L)

stab = stabsel(
  x = mod
  , q = 20L
  , PFER = 5L
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = subsample(model.weights(mod), B = 50L, strata = NULL)
)

stab.2 = stabsel(
  x = mod
  , q = 20L
  , PFER = 5L
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = 1L * as.matrix(1L == replicate(50, folds.svy(sv, nfolds = 2, strataID = "strata", clusterID = "cluster")))
)

par(mfrow = c(1,2))
plot(stab, type = "maxsel", np = 20)
plot(stab.2, type = "maxsel", np = 20)

plot(stab, type = "path")
plot(stab.2, type = "path")

# refit with stability selected?

frml.new = as.formula(paste("hazf ~", paste(names(selected(stab)), collapse = " + ")))
mod.new = gamboost(
  formula = frml.new
  , data = sv
  , family = Binomial(type = "adaboost", link = "logit")
  , control = boost_control(mstop = 2000L, nu = 0.2)
)

plot(mod.new, which = "cage", type = "l")
plot(mod.new$risk(), type = "l")


# sparser model with deselection of base learners, Stömer et al.
mod[mstop(cv)]

modsp = DeselectBoost(mod, fam = Binomial(), data = sv)
length(names(coef(modsp)))




source(file = here::here("utils", "tidy_baselearner_functions.R"))

tidy_baselearner_names(mod, which = "ctwin")

save(mod, cv, stab, modsp, file = here("results", "h9h7v919.rda"))
