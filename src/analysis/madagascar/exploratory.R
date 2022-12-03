#
#
#
#

library(here)
library(dplyr)

load(file = here("data", "processed", "madagascar", "surveydata.rda"))


# infos
n_distinct(tb$strata)
n_distinct(tb$region)
n_distinct(tb$cluster)
n_distinct(tb$household)

count(tb, strata)
count(unique(tb[, c(1,4)]), strata, sort = TRUE)
tail(count(unique(tb[, c(1,4)]), strata, sort = TRUE))