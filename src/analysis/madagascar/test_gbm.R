#
#
#
#

library(gbm)

set.seed(seed = 1L)

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))

frml = hazx ~ urban + csex + ctwin + cbord + hmembers + mreligion +
  memployed + nodead + watersource + sanitation + wealth +
  electricity + radio + television + refrigerator + bicycle +
  motorcycle + car + fews + cage + mage + medu + mbmi + healthaccess +
  cityaccess + dhsregion

idx = sample(x = 1:nrow(sv), size = 0.8 * nrow(sv), replace = FALSE)

training_data = sv[idx, ]
test_data = sv[setdiff(1:nrow(sv), idx), ]

fitted_model = gbm(
  formula = frml
  , distribution = "bernoulli"
  , data = training_data
  , n.trees = 2000
  , interaction.depth = 3L
  , shrinkage = 0.01
  , bag.fraction = 1
  , train.fraction = 1
  , cv.folds = 10L
  , verbose = TRUE
  , n.cores = 4L
)

(mstop = which.min(fitted_model$cv.error))
fitted_model$cv.error[mstop] / 2

gbm.perf(fitted_model, method = "cv")


# bernoulli deviance
Metrics::logLoss(
  actual = test_data$hazx
  , predicted = predict(object = fitted_model, newdata = test_data, n.trees = mstop, type = "response")
) * 2L


# classification error
Metrics::ce(
  actual = test_data$hazx
  , predicted = 0.5 <= predict(object = fitted_model, newdata = test_data, n.trees = mstop, type = "response")
)
