# Script for fetching any/all housing tenure + ancestry/race data (for REALD)

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# # # ------------ PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age+housting+ancestry, Mult Co. draw 1',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    'PUMA', 'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'GQ', 'OWNERSHP', 'OWNCOST', 'RENTGRS', 'HHINCOME'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 57


# # # ------------ Tabular controls

tab_extract = define_extract_nhgis(
  description = 'tract-level housing-tenure+related for controls, draw 1',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      # Housing tenure pop counts; age of householder by house costs
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'B25008', 'B25093'),
      # BUT: is group quarters count is not the same as institutional count...
      c('B26001')
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
# Downloaded as extract # 37


tab_extract = define_extract_nhgis(
  description = 'tract-level housing-tenure+related for controls, draw 2 w rent/income',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      # Housing tenure pop counts; age of householder by house costs; gross rent / household income
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'B25008', 'B25093', 'B25070'),
      # BUT: is group quarters count is not the same as institutional count...
      c('B26001')
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
# Downloaded as extract # 38
