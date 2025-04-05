library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for group quarters living, draw 2 (including PUMA)',
  samples = c('us2022a', 'us2023a'),
  variables = c(
    # Identifiers (and weights)
    'STATEFIP', 'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 'HHWT', 
    # Variables for downscale
    'AGE', 'SEX', 'GQTYPE'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for group quarters living, draw 3 (2018-2023, OR only)',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 'HHWT', 
    # Variables for downscale
    'AGE', 'SEX', 'GQTYPE'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# extract # 36

# Trying to get institutionalized individuals age 18-54 in PUMA 5103

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for group quarters living, draw 5 (institutionalized extras)',
  samples = paste0('us', 2012:2017, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA',
    'SERIAL', 'PERNUM', 'PERWT', 'HHWT', 
    # Variables for downscale
    'AGE', 'SEX', 
    var_spec('GQ', case_selections = '3')
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# extract # 38 (#37 had a mistake)

#### NHGIS table

tab_extract = define_extract_nhgis(
  description = 'Group housing + age (prev. draws were not at tract level), draw 3',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = 'B01001', geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'B26001', geog_levels = 'tract')
  )
)
# ah puts into two different files.
# argument file_select isn't working for me so will need to do this in two lines lol

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
