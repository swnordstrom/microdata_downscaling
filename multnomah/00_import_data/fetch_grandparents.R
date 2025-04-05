library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### --- Get ACS Data

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for grandparent data, draw for reproducibility, pt. 1',
  samples = c('us2022a', 'us2023a'),
  variables = c(
    'STATEFIP', 'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'PUMA', 
    'AGE', 'SEX', 'RELATE', 'FAMSIZE', 'FAMUNIT', 
    'MULTGEN', 'MOMLOC', 'POPLOC', 'SPLOC'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for grandparent data, draw for reproducibility, pt. 2 (or data only, 2018-2023)',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'PUMA', 
    'AGE', 'SEX', 'RELATE', 'MOMLOC', 'POPLOC', 'SPLOC'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract 35

#### --- Get tabular data

tab_extract = define_extract_nhgis(
  description = 'Tabular data for grandparent table, draw for reproducibility, pt. 1',
  datasets = map(
    c('2018_2022_ACS5b'),
    ~ ds_spec(
      name = .x,
      data_tables = c('B10051'),
      geog_levels = 'tract'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
