# do the figure that shows that this method does not overfit easily
# it's also not lack of capacity, tree boosting exhibits the same behaviour
# difficult to stop then with resampling techniques, as these can exhibit erratic behaviour
# when approximating the average prediction error

library(mboost)
library(surveyCV)
library(ggplot2)
library(Metrics)
library(patchwork)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))
source(file = file.path("src", "utils", "DeselectBoost.R"))
source(file = file.path("src", "utils", "func_plot_partial_effects.R"))
source(file = file.path("src", "utils", "func_tidy_baselearner.R"))

# we drop stratum 2 so that we can have 3 folds of stratified cluster sampling
sv = subset(sv, strata != 2L) |> droplevels()

# create example folds
numSplits = 4L
initialSplit = replicate(numSplits, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))

# do the simulation with model-based boosting
simResults = vector(mode = "list")

for (i in 1:numSplits) {

  training_data = sv[which(initialSplit[, i] != 3L), ]
  validation_data = sv[which(initialSplit[, i] == 3L), ]
  grid = seq(50L, 5000L, by = 25L)

  mod = gamboost(
    formula = frml.1
    , data = training_data
    , family = Binomial(type = "glm", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, trace = TRUE, risk = "inbag")
  )

  cv_1 = cvrisk(
    object = mod
    , grid = grid
    , folds = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = training_data$strata)
  )

  cv_2 = cvrisk(
    object = mod
    , grid = grid
    , folds = cv(weights = model.weights(mod), type = "subsampling", B = 25L, strata = training_data$strata, p = 0.8)
  )

  cv_3 = cvrisk(
    object = mod
    , grid = grid
    , folds = cv(weights = model.weights(mod), type = "kfold", B = 10L, strata = training_data$strata)
  )

  oobagLoss = sapply(
    X = grid,
    FUN = function(m) {
      preds = predict(object = mod[m], newdata = validation_data, type = "response")
      loss = logLoss(validation_data$hazx, preds)
      return(loss)
    }
  )

  simResults[[i]] = rbind(
    data.frame(i = i, type = "inbag", risk = mod$risk() / nrow(training_data), iter = 1:5001)
    , data.frame(i = i, type = "oobag", risk = oobagLoss, iter = grid)
    , data.frame(i = i, type = "subsampling p = 0.5", risk = colMeans(cv_1), iter = grid)
    , data.frame(i = i, type = "subsampling p = 0.8", risk = colMeans(cv_2), iter = grid)
    , data.frame(i = i, type = "k-fold", risk = colMeans(cv_3), iter = grid)
  )
}



# do the plots
simResults = do.call(rbind, simResults)

plt = ggplot(data = simResults, mapping = aes(x = iter, y = risk)) +
  geom_line(mapping = aes(linetype = type)) +
  labs(x = "Iteration", y = "Risk", linetype = "Type") +
  scale_linetype_manual(breaks = c("inbag", "oobag", "subsampling p = 0.5", "subsampling p = 0.8", "k-fold"), values = c("solid", "solid", "dashed", "twodash", "dotted")) +
  facet_wrap(~ i, ) +
  theme_gray()

ggsave(
  plot = plt
  , filename = "madagascar_simulation_overfit.png"
  , path = file.path("results", "figures")
  , dpi = 600, width = 200, height = 260
  , units = "mm", device = png
)
