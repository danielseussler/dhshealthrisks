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



# estimate main model based on the simple formula 
# resample model to with cluster sub sampling and survey stratification to select optimal number of 
# boosting iterations and apply early stopping

fittedModel = gamboost(
  formula = frml.1
  , data = sv
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 5000L, nu = 0.25, trace = TRUE)
)

ssResample = cvrisk(
  object = fittedModel
  , grid = seq(25L, 5000L, by = 10L)
  , folds = as.matrix(1L == replicate(25L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

plot(ssResample, main = "25-fold survey straified cluster subsampling")

fittedModel[mstop(ssResample)]



# sparser model with deselection of base learners, Stömer et al.

# deselects base learners that did not achieve the required threshold in risk reduction and refits model
# check variable importance before using deselection approach
# if risk is distributed over too many base learner this might not be the best approach

# note: the warning thrown is because the function searches for the response var in the data frame, 
# and converts to numerics which throws this error converting region char, no issue with the estimation
length(names(coef(fittedModel, which = "")))
names(coef(fittedModel))

plot(varimp(fittedModel), percent = TRUE, nbars = 22, type = "blearner")

deselectModel = DeselectBoost(
  object = fittedModel
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "attributable"
) 

deselectModelCum = DeselectBoost(
  object = fittedModel
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "cumulative"
) 

names(coef(fittedModel))
names(coef(deselectModel))
names(coef(deselectModelCum))



# stability selection

# set the expected number of relevant terms as 10, per family error rate (expected) 2 and 4 
# MB is for the older the more conservative bound, SS uses complementary sampling and is additional 
# assumptions on the distribution of 'noise' variables to derive bounds without the initial assumptions
# is thereby only controlling the number of *expected number of selected variables with low selection probability*

stabsel_parameters(x = fittedModel, q = 10L, PFER = 2L, sampling.type = "SS")
stabsel_parameters(x = fittedModel, q = 10L, PFER = 4L, sampling.type = "MB")

stabMB = stabsel(
  x = fittedModel
  , q = 10L
  , PFER = 4L
  , grid = 0:5000
  , sampling.type = "MB"
  , folds = 1L * as.matrix(1L == replicate(100L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

stabSS = stabsel(
  x = fittedModel
  , q = 10L
  , PFER = 2L
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = 1L * as.matrix(1L == replicate(50L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

plot(stabMB, type = "maxsel", np = 20L, main = "Stability Selection MB")
plot(stabSS, type = "maxsel", np = 20L, main = "Stability Selection SS")

plot(stabMB, type = "path")
plot(stabSS, type = "path")

save(
  fittedModel, ssResample, deselectModel, deselectModelCum, stabMB, stabSS
  , file = here::here("models", "h9h7v919.rda")
)
