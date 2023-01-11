#
#
#
#

library(ggplot2)
library(survey)
library(dplyr)
library(data.table)

load(file = file.path("models", "9dkw7wyn.rda"))

# get design-based estimates
# survey data and extract data
variables = c("hv000", "hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hml32", "hml35", "hv042", "hv103", "hc1")

sv = readRDS(file = file.path("data", "raw", "rdhs", "MLPR81FL.rds"))
sv = sv[variables]
sv = subset(sv, hv042 == 1 & hv103 == 1 & hc1 %in% 6:59 & hml35 %in% 0:1)
sv = mutate(sv, hv024 = labelled::to_character(hv024), hv025 = labelled::to_character(hv025))


# survey design - two stage sampling, clusters are first stage psu (no sampling weights)
# second stage is inverse proportional probability sampling pps
# national estimates can also be extracted by DHS survey package
# weights are household level / 1e6 by DHS Guide
design = svydesign(ids = ~hv001+hv002, strata = ~hv023, data = sv, weights = ~hv005)
estimates_design = svyby(formula = ~hml35, by = ~hv024, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
estimates_design$admin1 = stringr::str_to_title(estimates_design$hv024)
estimates_design$type = "design estimates"

# comparison at the mean
tmp = data.table(admin1 = grid$admin1, admin2 = grid$admin2, pop_u5 = as.integer(pred$pop_u5), p = pred$mu)

tmp[, .(prev = weighted.mean(p, w = pop_u5, na.rm = TRUE))] # not good
tmp[, .(prev = weighted.mean(p, w = pop_u5, na.rm = TRUE)), by = admin1]
tmp[, .(prev = weighted.mean(p, w = pop_u5, na.rm = TRUE)), by = admin2]

est = tmp[, .(prev = weighted.mean(p, w = pop_u5, na.rm = TRUE)), by = admin1][order(admin1)]

plot(estimates_design$hml35, est$prev, xlim = c(0, 0.3), ylim = c(0, 0.3))
abline(a = 0, b = 1)

# whats the urban percentage per admin
grid[, urban1 := ifelse(urban == "urban", 1, 0)]
grid[, (mean = weighted.mean(urban1, pop)), by = admin1]


# check the samples
tmps = data.table(admin1 = grid$admin1, admin2 = grid$admin2, urban = grid$urban, pop = as.integer(grid$pop), pop_u5 = as.integer(pred$pop_u5), samp)
tmps = melt(tmps, id.vars = c("admin1", "admin2", "pop", "pop_u5", "urban"), variable.name = "iter", value.name = "value")

tmps = tmps[, .(pop = sum(pop), pop_u5 = sum(pop_u5), value = sum(value)), by = .(admin1, iter)]

tmps[, prev := value / pop_u5]

ggplot(data = tmps, mapping = aes(x = prev, y = admin1)) +
  geom_density_ridges(stat = "binline", bins = 30)

# tmps = tmps[, .(hml35 = mean(prev), sd = sd(prev)), by = .(admin1)]
# tmps[, ci_l := hml35 - qnorm(0.975) * sd]
# tmps[, ci_u := hml35 + qnorm(0.975) * sd]
tmps = tmps[, .(hml35 = mean(prev), ci_l = quantile(prev, 0.025), ci_u = quantile(prev, 0.975)), by = .(admin1)]
tmps[, type := "model-based"]






ggplot(data = rbindlist(list(tmps, estimates_design), fill = TRUE),
       mapping = aes(x = admin1, y = hml35, ymin = ci_l, ymax = ci_u, group = type, color = type)) +
  geom_pointrange(position=position_dodge(width=0.20))




