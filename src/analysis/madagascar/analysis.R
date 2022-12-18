#
#
#
#

library(mboost)
library(surveyCV)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here::here("data", "processed", "madagascar", "surveydata.rda"))
source(file = here::here("src", "analysis", "madagascar", "formula.R"))
source(file = here::here("src", "utils", "DeselectBoost.R"))
source(file = here::here("src", "utils", "func_plot_partial_effects.R"))
source(file = here::here("src", "utils", "func_tidy_baselearner.R"))


# estimate model based on the simple formula 
# resample model to with cluster sub sampling and survey stratification
# check alternative cross-validation based on regular 10-folds

mod = gamboost(
  formula = frml.1
  , data = sv
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 5000L, nu = 0.25, trace = TRUE)
)

cv = cvrisk(
  object = mod
  , folds = 1L * as.matrix(1L == replicate(25L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

cv.kfold = cvrisk(object = mod, folds = cv(model.weights(mod), type = "kfold", B = 10L))

mstop(cv)
mstop(cv.kfold)

plot(cv, main = "25-fold survey stratified cluster sampling")
plot(cv.kfold)


# use cluster subsampling for the rest of the analyses
# apply number of boosting iterations
mod[mstop(cv)]


# check variable importance before using deseltion approach
# if risk is distributed over too many base learner this might not be the best approach
length(names(coef(mod, which = "")))
names(coef(mod))

plot(varimp(mod), type = "blearner", nbars = 20L)


# sparser model with deselection of base learners, Stömer et al.
# deselects base learners that did not achieve the required threshold in risk reduction and refits

# note: the warning thrown is because the function searches for the response var in the data frame, 
# and converts to numerics which throws this error converting region char, no issue with the estimation

# 

mod.deselect = DeselectBoost(
  object = mod
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "attributable"
) 

mod.deselect.cum = DeselectBoost(
  object = mod
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "cumulative"
) 

length(names(coef(mod)))
length(names(coef(mod.deselect)))
length(names(coef(mod.deselect.cum)))

names(coef(mod.deselect))
names(coef(mod.deselect.cum))


# stability selection
# expected number of relevant terms is 10, per family error rate (expected) 1
# results in a very high cutoff
# I run it once with subsampling clusters + survey stratified, and once subsamples at random

stabsel_parameters(x = mod, q = 10L, PFER = 1)

stab = stabsel(
  x = mod
  , q = 10L
  , PFER = 1
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = 1L * as.matrix(1L == replicate(50L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

stab.ns = stabsel(
  x = mod
  , q = 10L
  , PFER = 1
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = subsample(model.weights(mod), B = 50L, strata = NULL)
)

plot(stab, type = "maxsel", np = 20, main = "Stability Selection")
plot(stab.ns, type = "maxsel", np = 20)

plot(stab, type = "path")
plot(stab.ns, type = "path")

save(mod, cv, cv.kfold, mod.deselect, mod.deselect.cum, stab, stab.ns, 
     file = here::here("models", "h9h7v919.rda"))
