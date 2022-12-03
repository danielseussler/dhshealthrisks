# load all required dhs files for later use and access them directly by filename
#
#
#

library(here)
library(data.table)


# run dhs api setup
source(file = here("src", "configs", "rdhs.R"))


# country selection
mycountries = c("Ethiopia", "Madagascar", "Mali")


# load available surveys for selected countries
countries = rdhs::dhs_countries()
cselected = countries[CountryName %in% mycountries, DHS_CountryCode]


# survey characteristics: Anthropometry
surveychar = rdhs::dhs_survey_characteristics()
surveychar[grepl("Anthropometry", SurveyCharacteristicName, ignore.case = TRUE)]


# survey selection
surveys = rdhs::dhs_surveys(
  countryIds = cselected
  , surveyType = c("DHS", "MIS")
  , surveyYearStart = 2015
  , surveyYearEnd = 2022
)

surveys[, .(SurveyId, CountryName, SurveyYear, NumberOfWomen, SurveyNum, FieldworkEnd)]
surveys = surveys[SurveyId %in% c("ET2019DHS", "MD2021DHS", "ML2021MIS")]


# fileType is PR Household Member Recode KR Children's Recode GE Geographic Data
# https://dhsprogram.com/data/File-Types-and-Names.cfm
datasets = rdhs::dhs_datasets(surveyIds = surveys$SurveyId, fileType = c("PR", "KR", "GE"), fileFormat = "flat")
datasets[, .(SurveyId, FileType, SurveyNum, FileDateLastModified, FileName)]


# download datasets
rdhs::get_datasets(
  dataset_filenames = datasets$FileName
  , output_dir_root = here("data", "raw", "rdhs")
  , clear_cache = TRUE
)

rdhs::get_downloaded_datasets()
