library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'disability ACS (2018-2023), draw 1',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    'US2018A_DIS', 'US2019A_DIS', 'US2020A_DIS', 
    'US2021A_DIS', 'US2022A_DIS', 'US2023A_DIS'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #33

# Another PUMS with occupation and group quarters for universe spec.
acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'disability ACS (2018-2023), draw 2 (incl. occ + gq)',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    # Occupation and group quarters
    'OCC', 'GQ',
    # Disability status
    'US2018A_DIS', 'US2019A_DIS', 'US2020A_DIS', 
    'US2021A_DIS', 'US2022A_DIS', 'US2023A_DIS'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# Downloaded as extract #34

# Running a third time now to include race and ethnicity data
acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'disability ACS (2018-2023), draw 3 (incl. race/eth)',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    # Identifiers (and weights)
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    # Race and ethnicity,
    'RACE', 'HISPAN',
    # Occupation and group quarters
    'OCC', 'GQ',
    # Disability status
    'US2018A_DIS', 'US2019A_DIS', 'US2020A_DIS', 
    'US2021A_DIS', 'US2022A_DIS', 'US2023A_DIS'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as extract #39

#### NHGIS table

tab_extract = define_extract_nhgis(
  # shoot... forgot to edit the description on this one (it is wrong)
  description = 'nativity (+citizenship, incidentally), draw 1',
  datasets = list(
    ds_spec(name = '2018_2022_ACS5a', data_tables = 'B01001', geog_levels = 'tract'),
    ds_spec(name = '2018_2022_ACS5b', data_tables = 'B18101', geog_levels = 'tract')
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as #22

tab_extract = define_extract_nhgis(
  description = 'disability, draw 2 (racial+ethnicity info)',
  datasets = map2(
    # paste0('2018_2022_ACS5', letters[c(1, rep(2, 9))]),
    # paste0('B01001', c('', LETTERS[1:9])),
    .x = list('2018_2022_ACS5a','2018_2022_ACS5b'),
    .y = list('B01001', paste0('B01001', LETTERS[1:9])),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'tract'
    )
  )
)