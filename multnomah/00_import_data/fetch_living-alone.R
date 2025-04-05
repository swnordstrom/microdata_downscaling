# Script to fetch data for downscaled estimates people living alone (by age
# group)
# This requires getting 

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS PUMS (2018-2023) for living alone downscaling, draw 1',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 'HHWT',
    # Variables for downscale
    'AGE', 'SEX', 'FAMSIZE', 'FAMUNIT', 'NFAMS', 'SPLOC'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #30

#### NHGIS table

tab_extract = define_extract_nhgis(
  description = 'tabular data for living alone, draw 1',
  datasets = ds_spec(
    name = '2018_2022_ACS5a', data_tables = c('B01001', 'B11016'), geog_levels = 'tract'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as #21
