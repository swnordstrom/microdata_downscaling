# Data for rent>30% downscaling in Multnomah County

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### --- Get ACS Data

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS PUMS data for housing cost-income data, draw 1',
  samples = paste0('us', 2018:2023, 'a'), 
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'HHWT', 'PUMA', 
    'OWNERSHP', 'OWNCOST', 'RENTGRS', 'HHINCOME'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #28

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS PUMS data for housing cost-income data, draw 2 (incl. age+sex)',
  samples = paste0('us', 2018:2023, 'a'), 
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP',
    'SERIAL', 'PERNUM', 'PERWT', 'HHWT', 'PUMA', 
    'OWNERSHP', 'OWNCOST', 'RENTGRS', 'HHINCOME',
    'AGE', 'SEX'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #29

#### --- Get tabular data

tab_extract = define_extract_nhgis(
  description = 'Tabular data for housing cost-income data, draw 1',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = 'B01001', geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'B25106', geog_levels = 'tract')
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #20


tab_extract = define_extract_nhgis(
  description = 'Owner/renter (to get people living in households), draw 1',
  datasets = ds_spec(name = '2018_2022_ACS5a', data_tables = c('B01001', 'B25008'), geog_levels = 'tract')
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #23
