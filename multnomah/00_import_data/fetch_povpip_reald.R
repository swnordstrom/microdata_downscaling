# Script for fetching any/all income/poverty + ancestry data (for REALD)

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# # # ------------ PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age+ancestry/race+povpip, Mult Co. draw 1',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'PUMA',
    'AGE', 'SEX', 'OCC', 'GQ',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER',
    'HISPAN', 'ANCESTR1', 'ANCESTR2', 
    'POVERTY'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 53 (lol)

# # # ------------ Tabular controls

tab_extract = define_extract_nhgis(
  description = 'tract-level age+sex+race+pov for controls, draw 1',
  datasets = ds_spec(
    name = '2019_2023_ACS5a',
    data_tables = c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'C17002'),
    geog_levels = 'tract'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 33
