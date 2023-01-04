# autocompboost
#
# test autoML based on componentwise boosting
# but compboost is apparently not available for R4.2.2 :(

library(mlr3verse)
library(autocompboost)

set.seed(seed = 1L)

load(file = file.path("data", "processed", "madagascar", "surveydata.rda"))
source(file = file.path("src", "analysis", "madagascar", "formula.R"))

select = c(attr(terms(frml.tree),"term.labels"), "hazf")
select = setdiff(select, "denom")

task = TaskClassif$new(
  "autoML"
  , backend = sv[, select]
  , target = "hazf"
  , positive = "yes"
)

cboost = lrn(
  "classif.compboost"
  , predict_type = "prob"
  , show_output = TRUE
  , learning_rate = 0.01
  , add_deeper_interactions = TRUE
  , stop_epsylon_for_break = 0
  , stop_patience = 3L
  , df = 4
)

cboost$train(task)


## How much risk was explained by which stage:
rstages = cboost$getRiskStages()

knitr::kable(rstages)
