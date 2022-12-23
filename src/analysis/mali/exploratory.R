# survey characteristics
#
#
#

library(here)
library(dplyr)

# data
load(file = here("data", "processed", "mali", "surveydata.rda"))
load(file = here("data", "processed", "mali", "geodata.rda"))
cl = left_join(cl, cloc)

# survey infos
n_distinct(sv$strata)
n_distinct(sv$region)
n_distinct(sv$cluster)

table(cl$strata)
hist(cl$n)

count(sv, strata)
count(sv, cluster)
count(sv, urban)
