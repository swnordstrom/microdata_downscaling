# Fetching data for very first downscaling attempt
# Downscaling veteran status + age to census tracts in OR
# based on age+race+pov fetch script
# 18 Nov 2024 (re-run for more data 25 Nov)

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

acs_extract = define_extract_micro(
  collection = 'usa',
  # mistake... ugh... forgot to change the description
  description = 'ACS data (2020 PUMAs) for veteran+age+sex downscaling, draw 2',
  samples = c('us2022a'),
  variables = c(
    'STATEFIP', 'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'PUMA', 'AGE', 'SEX', 'VETSTAT'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

tab_extract = define_extract_nhgis(
  description = 'Age + vet status at census tract level, draw 1',
  datasets = map(
    c('2018_2022_ACS5a'),
    ~ ds_spec(
      name = .x,
      data_tables = c('B21001'),
      geog_levels = 'tract'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

### Adding in race data

# 25 Nov 2024
acs_extract = define_extract_micro(
  collection = 'usa',
  # mistake... ugh... forgot to change the description
  description = 'ACS data (2020 PUMAs) for veteran+age+race+sex downscaling, draw 3',
  samples = c('us2022a'),
  variables = c(
    'STATEFIP', 'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'PUMA', 
    'AGE', 'SEX', 'RACE', 'HISPAN', 'VETSTAT'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')


# 28 Dec 2024

acs_extract = define_extract_micro(
  collection = 'usa',
  # mistake... ugh... forgot to change the description
  description = 'ACS data (2017-2023) for veteran+age+race+sex downscaling, draw 4',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    var_spec('STATEFIP', case_selections = '41'), 
    'COUNTYFIP', 'SERIAL', 'PERNUM', 'PERWT', 'PUMA', 
    'AGE', 'SEX', 'RACE', 'HISPAN', 'VETSTAT'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as extract #31
