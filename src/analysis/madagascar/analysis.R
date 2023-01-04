#
#
#
#

library(mboost)
library(surveyCV)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "DeselectBoost.R"))

sv$moderatelyx = with(sv, ifelse(haz < -200, 1L, 0L))
sv$moderatelyf = factor(sv$moderatelyx, levels = c(0L, 1L), labels = c("no", "yes"))

sv$severelyx = with(sv, ifelse(haz < -300, 1L, 0L))
sv$severelyf = factor(sv$severelyx, levels = c(0L, 1L), labels = c("no", "yes"))


# estimate main model based on the simple formula 
# resample model to with cluster sub sampling and survey stratification to select optimal number of 
# boosting iterations and apply early stopping

fitModerately = gamboost(
  formula = update(frml.1, moderatelyf ~ .)
  , data = sv
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 10000L, nu = 0.1, trace = TRUE)
)

cvModerately = cvrisk(
  object = fitModerately
  , grid = seq(from = 25L, to = mstop(fitModerately), by = 10L)
  , folds = cv(weights = model.weights(fitModerately), type = "subsampling", B = 25L, prob = 0.8, strata = sv$strata)
)

plot(cvModerately, main = "25-fold survey straified subsampling")

fitModerately[mstop(cvModerately)]
names(coef(fitModerately))


# for severely stunted children only
fitSeverely = gamboost(
  formula = update(frml.1, severelyf ~ .)
  , data = sv
  , family = Binomial(type = "glm", link = "logit")
  , control = boost_control(mstop = 10000L, nu = 0.1, trace = TRUE)
)

cvSeverely = cvrisk(
  object = fitSeverely
  , grid = seq(from = 25L, to = mstop(fitSeverely), by = 10L)
  , folds = cv(weights = model.weights(fitSeverely), type = "subsampling", B = 50L, prob = 0.8, strata = sv$strata)
)

plot(cvSeverely, main = "25-fold survey straified subsampling")

fitSeverely[mstop(cvSeverely)]
names(coef(cvSeverely))



# sparser model with deselection of base learners, Stömer et al.

# deselects base learners that did not achieve the required threshold in risk reduction and refits model
# check variable importance before using deselection approach
# if risk is distributed over too many base learner this might not be the best approach

# note: the warning thrown is because the function searches for the response var in the data frame, 
# and converts to numerics which throws this error converting region char, no issue with the estimation
length(names(coef(fitModerately, which = "")))
names(coef(fitModerately))
plot(varimp(fitModerately), percent = TRUE, nbars = 30, type = "blearner")

deselectModerately = DeselectBoost(
  object = fitModerately
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "attributable"
) 

deselectModeratelyCum = DeselectBoost(
  object = fitModerately
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "cumulative"
) 

names(coef(fitModerately))
names(coef(deselectModerately))
names(coef(deselectModeratelyCum))



deselectSeverely = DeselectBoost(
  object = fitSeverely
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "attributable"
) 

deselectSeverelyCum = DeselectBoost(
  object = fitSeverely
  , fam = Binomial(type = "glm", link = "logit")
  , data = sv
  , tau = 0.01
  , method = "cumulative"
) 

names(coef(fitSeverely))
names(coef(deselectSeverely))
names(coef(deselectSeverelyCum))



# stability selection

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
  , folds = 1L * as.matrix(1L == replicate(100L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

stabModeratelySS = stabsel(
  x = fitModerately
  , q = 10L
  , PFER = 2L
  , grid = 0:5000
  , sampling.type = "SS"
  , assumption = "unimodal"
  , folds = 1L * as.matrix(1L == replicate(50L, folds.svy(sv, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
)

plot(stabModeratelyMB, type = "maxsel", np = 20L, main = "Stability Selection MB")
plot(stabModeratelySS, type = "maxsel", np = 20L, main = "Stability Selection SS")

plot(stabModeratelyMB, type = "path")
plot(stabModeratelySS, type = "path")


save(
  fitModerately, fitSeverely, cvModerately, cvSeverely, deselectModerately, deselectModeratelyCum, 
  deselectSeverely, deselectSeverelyCum, stabModeratelyMB, stabModeratelySS
  , file = file.path("models", "h9h7v919.rda")
)
