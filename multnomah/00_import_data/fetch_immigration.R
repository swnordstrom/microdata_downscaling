library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'birthplace ACS (2017-2023), draw 1',
  samples = paste0('us', 2017:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX', 'BPL'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as #26

# Oops... constraint table needs citizenship too
acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'birthplace ACS (2018-2023), draw 2 (with citizenship)',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX', 'BPL', 'CITIZEN'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as #27

#### NHGIS table

tab_extract = define_extract_nhgis(
  description = 'nativity (+citizenship, incidentally), draw 1',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = 'B01001', geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'B05003', geog_levels = 'tract')
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as #19