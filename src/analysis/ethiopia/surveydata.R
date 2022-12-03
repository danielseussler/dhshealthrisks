# compilation of unit level responses of childhood malnutrition
# source: children's recode KR of the survey
#
#

library(here)
library(sp)
library(sf)
library(terra)
library(dplyr)
library(forcats)
library(labelled)


# data
# rdhs::dhs_datasets(surveyIds = "ET2019DHS", fileFormat = "flat")[, 3:8]

shp = readRDS(file = here("data", "processed", "ethiopia", "dhsboundaries.rds"))
survey = readRDS(file = here("data", "raw", "rdhs", "ETKR81FL.rds"))

cluster = readRDS(file = here("data", "raw", "rdhs", "ETGE81FL.rds"))
cluster = st_as_sf(cluster, crs = st_crs(4326))
cluster = select(cluster, cluster = DHSCLUST, lon = LONGNUM, lat = LATNUM, DHSREGCO)


# neighborhood matrix
nb = bamlss::neighbormatrix(x = as(st_make_valid(shp), "Spatial"), names = "DHSREGEN")


# create csv with available questions
label = rdhs::data_and_labels(survey)$variable_names
write.csv2(bind_cols("#", label), file = here("src", "analysis", "ethiopia", "questions.csv"))


# remove header labels
survey = haven::zap_label(survey)


# survey information
# 	v000	Country code and phase
# 	v001	Cluster number
# 	v002	Household number
# 	v005	Women's individual sample weight (6 decimals)
# 	v006	Month of interview
# 	v007	Year of interview
# 	v023	Stratification used in sample design
# 	v024	Region
# 	v025	Type of place of residence
tb = tibble(
  cluster = as.integer(survey$v001),
  household = as.integer(survey$v002),
  weight = as.numeric(survey$v005) / 1e6,
  strata = as.integer(survey$v023),
  region = to_character(survey$v024),
  urban = to_factor(survey$v025, sort_levels = "labels")
)
tb$region = stringr::str_to_title(tb$region)
tb$strata = as.factor(tb$strata)

n_distinct(tb$cluster)
n_distinct(tb[2:3])
table(tb$urban)
table(tb$strata)



# response covariates
# 	b4	Sex of child
# 	hw1	Child's age in months
# 	hw70	Height/Age standard deviation (new WHO)
# 	hw71	Weight/Age standard deviation (new WHO)
# 	hw72	Weight/Height standard deviation (new WHO)
tb$denom = with(survey, (hw1 %in% 0:59 & hw70 < 9990)) * 1L

tb$haz = as.numeric(survey$hw70)
tb$hazx = with(survey, ifelse(hw70 < -200, 1L, 0L))
tb$hazf = factor(tb$hazx, levels = c(0, 1), labels = c("no", "yes"))

# tb$whz = as.numeric(survey$hw72)
# tb$whz[tb$whz > 9990] = NA
# tb$whzx = with(survey, ifelse(hw72 < -200, TRUE, FALSE))
# tb$whzx[is.na(tb$whz)] = NA

select = (tb$denom == 1 & !is.na(tb$denom)) # drop observations w/o valid score
tb = tb[select, ]
survey = survey[select, ]

table(tb$hazx)
hist(tb$haz)

# table(tb$whzx)
# hist(tb$whz)

aggregate(hazx ~ urban, data = tb, mean)
# aggregate(whzx ~ urban, data = tb, mean)

tb$csex = to_factor(survey$b4)
table(tb$csex)

tb$cage = survey$hw1
hist(tb$cage)



# 	b0	Child is twin
# 	v218	Number of living children
# 	bord	Birth order number
# 	v130	Religion
#	  v136	Number of household members (listed)
#	  v137	Number of children 5 and under in household (de jure)
# 	m5	  Months of breastfeeding
#	  m14	  Number of antenatal visits during pregnancy
tb$ctwin = ifelse(survey$b0 > 0, "yes", "no")
tb$ctwin = as.factor(tb$ctwin)
table(tb$ctwin)

tb$cbord = survey$bord
tb$cbord[tb$cbord > 10] = 10
table(tb$cbord)

tb$hmembers = as.numeric(survey$v136)
hist(tb$hmembers)

tb$mreligion = to_factor(survey$v130, drop_unused_labels = TRUE) |>
  fct_collapse(NULL = "missing", other = c("traditional", "catholic", "other"))
table(tb$mreligion)



# WASH characteristics
# 	v113	Source of drinking water
# 	v116	Type of toilet facility
#	  v161	Type of cooking fuel
tb$watersource = to_factor(survey$v113, drop_unused_labels = TRUE)
tb$watersource = fct_collapse(
  # https://data.unicef.org/topic/water-and-sanitation/drinking-water/
  tb$watersource,
  NULL = c("missing", "not a dejure resident"),
  unimproved = c("unprotected well", "unprotected spring", "tube well or borehole", "river/dam/lake/ponds/stream/canal/irrigation channel", "rainwater", "other"),
  improved = c("protected well", "protected spring", "public tap/standpipe", "bottled water", "tanker truck", "cart with small tank"),
  piped = c("piped into dwelling", "piped to yard/plot", "piped to neighbor")
) |> fct_relevel("unimproved", "improved")

table(tb$watersource)


tb$sanitation = to_factor(survey$v116, drop_unused_labels = TRUE)
tb$sanitation = fct_collapse(
  tb$sanitation,
  NULL = c("missing", "not a dejure resident"),
  improved = c(
    "flush to piped sewer system", "flush to septic tank", "flush to pit latrine",
    "flush to somewhere else", "flush, don't know where"),
  unimproved = c(
    "ventilated improved pit latrine (vip)", "composting toilet", "bucket toilet",
    "hanging toilet/latrine", "pit latrine without slab/open pit", "no facility/bush/field",
    "pit latrine with slab", "other")
) |> fct_relevel("unimproved", "improved")

table(tb$sanitation)


# parental characteristics
# 	v012	Respondent's current age
# 	v133	Education in single years
# 	v437	Respondent's weight in kilograms (1 decimal)
# 	v438	Respondent's height in centimeters (1 decimal)
# 	v445	Body Mass Index
#	  v501	Current marital status
#	  v535	Ever been married or in union
# 	v715	Husband/partner's total number of years of education
# 	v717	Respondent's occupation (grouped)
# 	v730	Husband/partner's age
# 	v732	Respondent employed all year/seasonal
tb$mage = as.numeric(survey$v012)
hist(tb$mage)

tb$medu = as.numeric(survey$v133)
tb$medu[tb$medu > 90] = NA
hist(tb$medu)



# household characteristics
# 	v190	Wealth index combined
# 	v190a	Wealth index for urban/rural
# 	v119	Household has: electricity
# 	v120	Household has: radio
# 	v121	Household has: television
# 	v122	Household has: refrigerator
# 	v123	Household has: bicycle
# 	v124	Household has: motorcycle/scooter
# 	v125	Household has: car/truck
# 	v127	Main floor material
# 	v161	Type of cooking fuel
# 	v169a	Owns a mobile telephone
# 	v169b	Use mobile telephone for financial transactions
# 	v170	Has an account in a bank or other financial institution
# 	v171a	Use of internet
# 	v171b	Frequency of using internet last month
tb$wealth = to_factor(survey$v190) |> fct_relevel("middle")
table(tb$wealth)

aggregate(hazx ~ wealth, data = tb, mean)
# aggregate(whzx ~ wealth, data = tb, mean)

tb$electricity = to_factor(survey$v119) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$electricity, useNA = "ifany")

tb$radio = to_factor(survey$v120) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$radio, useNA = "ifany")

tb$television = to_factor(survey$v121) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$television, useNA = "ifany")

tb$refrigerator = to_factor(survey$v122) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$refrigerator, useNA = "ifany")

tb$bicycle = to_factor(survey$v123) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$bicycle, useNA = "ifany")

tb$motorcycle = to_factor(survey$v124) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$motorcycle, useNA = "ifany")

tb$car = to_factor(survey$v125) |> fct_recode(NULL = "not a dejure resident", NULL = "missing")
table(tb$car, useNA = "ifany")



# add geospatial data
access = malariaAtlas::getRaster(
  surface = c("A global map of travel time to cities to assess inequalities in accessibility in 2015",
              "Walking-only travel time to healthcare map without access to motorized transport"),
  shp = as(shp, "Spatial"),
  file_path = tempdir()
)

access = terra::rast(access)
access = extract(x = access, y = cluster[, c("lon", "lat")], method = "bilinear", ID = FALSE)
colnames(access) = c("cityaccess", "healthaccess")

fews = read_sf(here("data", "raw", "FEWSNET", "ET_201902_CS.shp"))
fews = select(fews, fews = CS)

cluster = st_join(cluster, fews, join = st_within, left = TRUE)
cluster = st_drop_geometry(cluster)

cluster = merge(cluster, st_drop_geometry(shp[, c("DHSREGEN", "REGCODE")]), by.x = "DHSREGCO", by.y = "REGCODE")
cluster = cbind(cluster, access)
cluster = select(cluster, cluster, healthaccess, cityaccess, fews, dhsregion = DHSREGEN)
cluster$fews = as.factor(cluster$fews)
cluster$fews[is.na(cluster$fews)] = 1
cluster$dhsregion = as.factor(cluster$dhsregion)


# complete case analysis
tb = left_join(tb, cluster)
tb = tb[complete.cases(tb), ]
sv = as.data.frame(tb)


# save
save(sv, nb, file = here("data", "processed", "ethiopia", "surveydata.rda"))
