# figure
# region/urban disaggregation of design based estimates of malnutrition prevalence
#
#

library(here)
library(dplyr)
library(survey)
library(ggplot2)
library(viridis)
require(rdhs)


# Definition
# Percentage of children under 5 years of age, by nutritional status:
# 1)     Stunted.
# 2)     Wasted and overweight.
# 3)     Underweight and overweight for age.
# 4)     Mean z-score for height-for-age, weight-for-height, and weight-for-age.
#
# Coverage:
# Population base: Living children born 0-59 months before the survey (PR file)
# Time period: Current status at time of survey
#
# Numerators:
# Stunting:
# 1)     Severely stunted: Number of children whose height-for-age z-score is below minus 3 (‑3.0) standard deviations (SD) below the mean on the WHO Child Growth Standards (hc70 < ‑300)
# 2)     Moderately or severely stunted: Number of children whose height-for-age z-score is below minus 2 (‑2.0) standard deviations (SD) below the mean on the WHO Child Growth Standards (hc70 < -200)
# 3)     Mean z-score for height-for-age: Sum of the z-scores of children with a non-flagged height for age score (∑ hc70/100, if hc70 < 9990)

# Denominators:
# Number of de facto living children between ages 0 and 59 months before the survey (hv103 = 1 & hc1 in 0:59) who have:
# 1)     Stunting: valid non-flagged height for age z-scores (hc70 < 9990)
# 2)     Wasting and overweight: valid non-flagged weight for height z-scores (hc72 < 9990)
# 3)     Underweight and overweight for age: valid non-flagged weight for age z-scores (hc71 < 9990)

indicators = rdhs::dhs_indicators()
cindicator = indicators[grepl("stunt", Label)]
statcomp = rdhs::dhs_data(indicatorIds = "CN_NUTS_C_HA2", countryIds = "MD")
statcomp[, .(Indicator, CountryName, SurveyYear, Value, DenominatorWeighted, DenominatorUnweighted)]


# survey data and extract data
variables = c("hv000", "hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hv042", "hv103", "hc1", "hc70")

sv = readRDS(file = here("data", "raw", "rdhs", "MDPR81FL.rds"))
sv = sv[variables]
sv = subset(sv, hv103 == 1 & hc1 %in% 0:59 & hc70 < 9990L)
sv = mutate(sv, hv024 = labelled::to_character(hv024), hv025 = labelled::to_character(hv025), ind = hc70 < -200)


# survey design - two stage sampling, clusters are first stage psu (no sampling weights)
# second stage is inverse proportional probability sampling pps
# national estimates can also be extracted by DHS survey package
# weights are household level / 1e6 by DHS Guide

design = svydesign(ids = ~hv001+hv002, strata = ~hv023, data = sv, weights = ~hv005)

results = vector(mode = "list")
results[[1]] = svyby(formula = ~ind, by = ~hv024, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
results[[3]] = svyby(formula = ~ind, by = ~hv025, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")
results[[4]] = svyby(formula = ~ind, by = ~hv000, design = design, FUN = svyciprop, na.rm = TRUE, vartype = "ci")

results = lapply(results, as.data.frame)
results = do.call(bind_rows, results)

results$region = stringr::str_to_title(results$hv024)
results$region[is.na(results$region)] = stringr::str_to_title(results$hv025[is.na(results$region)])
results$region[is.na(results$region)] = "Madagascar (total)"

results$region[results$region == "Urban"] = "Madagascar (Urban)"
results$region[results$region == "Rural"] = "Madagascar (Rural)"
results$region = forcats::fct_reorder(results$region, results$ind)


theme_set(theme_classic())

plt = ggplot(data = results, mapping = aes(x = region, y = ind)) +
  geom_pointrange(mapping = aes(ymin = ci_l, ymax = ci_u), position = position_dodge(width = 0.7)) +
  geom_abline(intercept = results[results$region == "Madagascar (total)", "ind"], slope = 0, color = "gray") +
  labs(x = "", y = "Prevalence") +
  scale_color_manual(values = viridis(n = 3L, alpha = 0.8, begin = 0.3, end = 0.7), name = "Type") +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_shape(name = "Type") +
  theme(axis.text.x = element_text(angle = 45L, hjust = 1L))

ggsave(plot = plt, filename = "fig_mdg_designestimates.png", path = here("results", "figures"), scale = 1.2, dpi = 600, width = 200, height = 100, units = "mm", device = png)

