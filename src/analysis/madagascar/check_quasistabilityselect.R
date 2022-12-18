# check quasi stability selection
#
#
#

library(here)
library(data.table)
library(Metrics)
library(ggplot2)



load(file = here("models", "wt79adpc.rda"))

cnt = res[, .(count = .N), by = .(method, selected)][, prop := count / 50L]
cnt = dcast(cnt, formula = selected ~ method, value.var = "count")
cnt[, "rn > ss" := A > B]
cnt




