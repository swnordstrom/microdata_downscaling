library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'ACS data (2020 PUMAs) for language, draw 1',
  samples = c('us2022a', 'us2023a'),
  variables = c(
    # Identifiers (and weights)
    'STATEFIP', 'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX', 'LANGUAGE', 'SPEAKENG', 'LINGISOL'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

#### NHGIS table

tab_extract = define_extract_nhgis(
  description = 'language + age + sex, draw 1',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = 'B01001', geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'B16002', geog_levels = 'tract')
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

# Ah - realizing that B16002 is household level only
# Not sure how to reconcile that with the multi-individual households with multiple languages!
# C16001 has less info but I think still the languages we want - let's download that + ages
# B16004 also has age data (will need to do some binning...)

tab_extract = define_extract_nhgis(
  description = 'language + age + sex, draw 2 (individual-level tables)',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = c('B01001', 'B16004'), geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'C16001', geog_levels = 'tract')
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

#### ACS PUMS (running again)

acs_extract = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'ACS data (2020 PUMAs) for language, draw 2 (more years)',
  samples = paste0('us', 2018:2023, 'a'), # c('us2022a', 'us2023a'),
  variables = c(
    # Identifiers (and weights)
    'STATEFIP', 'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX', 'LANGUAGE', 'SPEAKENG', 'LINGISOL'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Extract #23

# And another extract to get missing cases
acs_extract_extra = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'ACS data (2020 PUMAs) for language, draw 3 (missing cases 2010-2017)',
  samples = paste0('us', 2010:2017, 'a'), # c('us2022a', 'us2023a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX', 
    var_spec('LANGUAGE', case_selections = as.character(c(2:11, 13:64, 70:96))), 
    'SPEAKENG', 'LINGISOL'
  )
)

acs_extract_extra %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as #25

