# do the figure that shows that this method does not overfit easily
# it's also not lack of capacity, tree boosting exhibits the same behaviour
# difficult to stop then with resampling techniques, as these can exhibit erratic behaviour
# when approximating the average prediction error

library(here)
library(mboost)
library(surveyCV)
library(ggplot2)
library(Metrics)
library(patchwork)

set.seed(seed = 1L)
options("mc.cores" = detectCores())

load(file = here("data", "processed", "madagascar", "surveydata.rda"))
source(file = here("src", "analysis", "madagascar", "formula.R"))
source(file = here("src", "utils", "DeselectBoost.R"))
source(file = here("src", "utils", "func_plot_partial_effects.R"))
source(file = here("src", "utils", "func_tidy_baselearner.R"))

sv$sevstunted = as.integer(sv$haz < -300) |> as.factor()
frml.1 = update(frml.1, sevstunted ~ .)

# we drop stratum 2 so that we can have 3 folds of stratified cluster sampling
sv = subset(sv, strata != 2L) |> droplevels()


# create example folds
numSplits = 6L
initialSplit = replicate(numSplits, folds.svy(sv, nfold = 3L, strataID = "strata", clusterID = "cluster"))


# do the simulation with model-based boosting
simResults = vector(mode = "list")

for (i in 1:numSplits) {

  trainingData = sv[which(initialSplit[, i] != 3L), ]
  validationData = sv[which(initialSplit[, i] == 3L), ]
  grid = seq(50L, 5000L, by = 25L)

  mod = gamboost(
    formula = frml.1
    , data = trainingData
    , family = Binomial(type = "adaboost", link = "logit")
    , control = boost_control(mstop = 5000L, nu = 0.1, trace = TRUE, risk = "inbag")
  )

  cv.ss = cvrisk(
    object = mod
    , grid = grid
    , folds = 1L * as.matrix(1L == replicate(25L, folds.svy(trainingData, nfolds = 2L, strataID = "strata", clusterID = "cluster")))
  )

  cv.kfold = cvrisk(
    object = mod
    , grid = grid
    , folds = cv(weights = model.weights(mod), type = "kfold", B = 10L)
  )

  oobagLoss = sapply(
    X = grid,
    FUN = function(m) {
      preds = predict(object = mod[m], newdata = validationData, type = "response")
      loss = logLoss(validationData$warnsevstunted, preds)
      return(loss)
    }
  )

  simResults[[i]] = rbind(
    data.frame(i = i, type = "inbag", risk = mod$risk() / nrow(trainingData), iter = 1:5001),
    data.frame(i = i, type = "oobag", risk = oobagLoss, iter = grid),
    data.frame(i = i, type = "cluster ss", risk = colMeans(cv.ss), iter = grid),
    data.frame(i = i, type = "kfold", risk = colMeans(cv.kfold), iter = grid)
  )
}



# do the plots
simResults = do.call(rbind, simResults)

plt1 = ggplot(data = simResults, mapping = aes(x = iter, y = risk)) +
  geom_line(mapping = aes(linetype = type)) +
  labs(x = "Iteration", y = "Risk", linetype = "Type") +
  scale_linetype_manual(breaks = c("inbag", "oobag", "cluster ss", "kfold"), values = c("solid", "dashed", "twodash", "dotted")) +
  facet_wrap(~i) +
  theme_bw()

# ggsave(
#   plot = plt1
#   , filename = "fig_mdg_overfitting_1.png"
#   , path = here("results", "figures")
#   , dpi = 600
#   , width = 200
#   , height = 150
#   , units = "mm"
#   , device = png
# )



# do the simulation with tree boosting

simResultsTrees = vector(mode = "list")

for (i in 1:numSplits) {

  trainingData = sv[which(initialSplit[, i] != 3L), ]
  validationData = sv[which(initialSplit[, i] == 3L), ]
  grid = seq(5L, 500L, by = 5L)

  mod = blackboost(
    formula = frml.tree
    , data = trainingData
    , family = Binomial(type = "adaboost", link = "logit")
    , control = boost_control(mstop = 500L, nu = 0.05, trace = TRUE, risk = "inbag")
    , tree_controls = partykit::ctree_control(maxdepth = 4L, saveinfo = FALSE)
  )

  oobagLoss = sapply(
    X = grid,
    FUN = function(m) {
      preds = predict(object = mod[m], newdata = validationData, type = "response")
      loss = logLoss(validationData$hazx, preds)
      return(loss)
    }
  )

  simResultsTrees[[i]] = rbind(
    data.frame(i = i, type = "inbag", risk = mod$risk() / nrow(trainingData), iter = 1:501),
    data.frame(i = i, type = "oobag", risk = oobagLoss, iter = grid)
  )
}

# do the plots
simResultsTrees = do.call(rbind, simResultsTrees)

plt2 = ggplot(data = simResultsTrees, mapping = aes(x = iter, y = risk)) +
  geom_line(mapping = aes(linetype = type)) +
  labs(x = "Iteration", y = "Risk", linetype = "Type") +
  scale_linetype_manual(breaks = c("inbag", "oobag"), values = c("solid", "dashed")) + 
  facet_wrap(~ i) +
  theme_bw()

# ggsave(
#   plot = plt2
#   , filename = "fig_mdg_overfitting_trees.png"
#   , path = here("results", "figures")
#   , dpi = 600
#   , width = 200
#   , height = 150
#   , units = "mm"
#   , device = png
# )
