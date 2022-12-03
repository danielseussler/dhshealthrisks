# data compilation: cluster level prevalence of malaria in madagascar
#
#
#

library(here)
library(dplyr)
library(sp)
library(sf)
require(rdhs)


# Coverage:
# Population base: De facto children age 6-59 months (PR file)
# Time period: Current status at the time the blood sample was taken
# Numerators:
# 1) Number of de facto children tested using RDT who are positive for malaria (hml35 = 1)
# 2) Number of de facto children tested using microscopy who are positive for malaria (hml32 = 1)
# Denominators:
# 1) Number of de facto children tested using RDT (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml35 in 0,1)
# 2) Number of de facto children tested using microscopy (hv042 = 1 & hv103 = 1 & hc1 in 6:59 & hml32 in 0,1,6)


# load shapefile and create neighborhood matrix
shp = readRDS(file = here("data", "processed", "madagascar", "dhsboundaries.rds"))
nb = bamlss::neighbormatrix(x = as(st_make_valid(shp), "Spatial"), names = "DHSREGEN")


# load survey data and create csv with available questions
sv = readRDS(file = here("data", "raw", "rdhs","MDPR81FL.rds"))
cl = readRDS(file = here("data", "raw", "rdhs", "MDGE81FL.rds"))
cl = as.data.frame(cl)
cl = select(cl, hv001 = DHSCLUST, lon = LONGNUM, lat = LATNUM)

variables = c("hv001", "hv002", "hv005", "hv023", "hv024", "hv025", "hml32", "hml35", "hv042", "hv103", "hc1")
sv = sv[variables]


# restrict to applicable observations
sv$hv024 = labelled::to_factor(sv$hv024)
sv$hv025 = labelled::to_factor(sv$hv025)
sv$hv023 = labelled::to_factor(sv$hv023)
sv = haven::zap_labels(sv)

sv = merge(sv, cl, all.x = TRUE)
sv = subset(sv, lat != 0)
sv = subset(sv, hv042 == 1 & hv103 == 1 & hc1 %in% 6:59 & hml35 %in% c(0, 1))
sv = select(sv, cluster = hv001, strata = hv023, region = hv024, urban = hv025, mtest = hml35, lon, lat)


# aggregate to cluster level and add h3 index
sv = group_by(sv, cluster, strata, region, urban, lon, lat)
sv = summarise(sv, n = n(), npos = sum(mtest), nneg = n - npos, intercept = 1, .groups = "drop")
sv$h3_index = h3::geo_to_h3(c(sv$lat, sv$lon), res = 7)


# infos
n_distinct(sv$strata)
n_distinct(sv$region)
n_distinct(sv$cluster)

hist(sv$n)
hist(sv$npos)

count(sv, strata)
count(unique(sv[, 1:2]), strata, sort = TRUE)
tail(count(unique(sv[, 1:2]), strata, sort = TRUE))


# save
save(sv, cl, nb, file = here("data", "processed", "madagascar", "surveydata_malaria.rda"))
