# Script for fetching any/all disability + ancestry data (for REALD)

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# # # ------------ PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age+disability+ancestry, Mult Co. draw 1',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    # # Not sure why below doesn't work... says that these are invalid cases?
    # var_spec(
    #   'PUMA', 
    #   case_selections = as.character(c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))
    # ),
    'PUMA',
    'AGE', 'SEX', 'OCC', 'GQ',
    'RACE', 'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER',
    'HISPAN',
    'ANCESTR1', 'ANCESTR2', 
    'DIFFCARE', 'DIFFEYE', 'DIFFHEAR', 'DIFFMOB', 'DIFFPHYS', 'DIFFREM', 'DIFFSENS'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 51 (lol)

# # # ------------ Tabular controls

tab_extract = define_extract_nhgis(
  description = 'tract-level age+sex+disability+ancestry for controls, draw 1',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003'),
      c(paste0('B1810', 2:7))
    ),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'tract'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 32
