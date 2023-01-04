# stability selection
#
#
#

library(stabs)
library(mboost)
library(surveyCV)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("models", "h9h7v919.rda"))


# set the expected number of relevant terms as 10, per family error rate (expected) 2 and 4
# MB is for the older the more conservative bound, SS uses complementary sampling and is additional
# assumptions on the distribution of 'noise' variables to derive bounds without the initial assumptions
# is thereby only controlling the number of *expected number of selected variables with low selection probability*

stabsel_parameters(x = fitModerately, q = 10L, PFER = 2L, sampling.type = "SS")
stabsel_parameters(x = fitModerately, q = 10L, PFER = 4L, sampling.type = "MB")

stabModeratelyMB = stabsel(
  x = fitModerately
  , q = 10L
  , PFER = 4L
  , grid = 0:5000
  , sampling.type = "MB"
  , folds = subsample(weights = model.weights(fitModerately), B = 100L , strata = sv$strata)
)

stabModeratelySS = stabsel(
  x = fitModerately
  , q = 10L
  , PFER = 2L
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = subsample(weights = model.weights(fitModerately), B = 100L , strata = sv$strata)
)

plot(stabModeratelyMB, type = "maxsel", np = 20L, main = "Stability Selection MB")
plot(stabModeratelySS, type = "maxsel", np = 20L, main = "Stability Selection SS")

plot(stabModeratelyMB, type = "path")
plot(stabModeratelySS, type = "path")

