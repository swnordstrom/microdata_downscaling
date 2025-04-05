library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

### - Download ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS PUMS for ancestry, race+ethnicity, language + english prof., draw 1',
  samples = paste0('us', 2018:2022, 'c'),
  variables = list(
    var_spec('STATEFIP', case_selections = c('41', '53')),
    'PUMA', 'ANCESTR1', 'ANCESTR2', 'RACE', 'HISPAN', 'LANGUAGE', 'SPEAKENG'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# downloaded as extract 42

# Trying again with another sample
acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for ancestry, race+ethnicity, language + english prof., draw 2 w/ one year acs',
  samples = paste0('us', 2018:2023, 'a'),
  variables = list(
    var_spec('STATEFIP', case_selections = c('41', '53')),
    'PUMA', 'ANCESTR1', 'ANCESTR2', 'RACE', 'HISPAN', 'LANGUAGE', 'SPEAKENG'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# downloaded as extract 43

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for ancestry, race+ethnicity, language + english prof., draw 3 (5y acs, race y/n vars incl.)',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = c('41', '53')),
    'PUMA', 'AGE',
    'RACE', 'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'RACNUM',
    'HISPAN',
    'ANCESTR1', 'ANCESTR2', 
    'LANGUAGE', 'SPEAKENG'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# downloaded as extract # 44

# Testing to see what disability data looks like in the five-year sample
acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age + disability in oregon+washington, draw 2 (w/ gq and employment)',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = c('41', '53')),
    'PUMA', 'AGE', 'SEX', 'OCC', 'GQ',
    'DIFFCARE', 'DIFFEYE', 'DIFFHEAR', 'DIFFMOB', 'DIFFPHYS', 'DIFFREM', 'DIFFSENS'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloaded as extract # 47

# according to get_sample_info('usa') # us20XXa is a one year sample
# but this link:
# https://www.census.gov/programs-surveys/acs/guidance/estimates.html
# suggests that using a five-year estimate is preferable for precision

### - Download ACS tabular data

tab_extract = define_extract_nhgis(
  description = 'Tabular tract-level data with ancestry, race+ethn, language+english prof, draw 1',
  datasets = map2(
    .x = list('2018_2022_ACS5a', '2018_2022_ACS5b'),
    .y = list(c('B02001', 'B03003'), c('B04006', 'B16001')),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'tract'
    )
  )
)

# ah... error above, I guess b01001 is only in acs5a

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# downloaded as extract 24

### --- Getting county-level estimates here
tab_extract = define_extract_nhgis(
  description = 'Tabular tract-level data with ancestry, race+ethn, language+english prof, draw 2 (county level)',
  datasets = map2(
    # same data sources, but maybe we could do something different?
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(c('B02001', 'B03003'), c('B04006', 'B16001')),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'county'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# downloaded as extract 25

### --- County-level estimates, hopefully with the correct data source...
tab_extract = define_extract_nhgis(
  description = 'Tabular tract-level data with ancestry, race+ethn, language+english prof, draw 3 (county level, corrected lang form)',
  datasets = map2(
    # same data sources, but maybe we could do something different?
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(c('B02001', 'B03003'), c('B04006', 'C16001')),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'county'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# data extract 26

tab_extract = define_extract_nhgis(
  # forgot to update description...
  description = 'Tabular tract-level data with ancestry, race+ethn, language+english prof, draw 3 (county level, corrected lang form)',
  datasets = ds_spec(
      name = '2019_2023_ACS5b',
      data_tables = 'B18101',
      geog_levels = 'county'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloaded as extract # 27

# Getting separate tables for each disability
tab_extract = define_extract_nhgis(
  description = 'Tabular county-level data disability(+age), draw 2 (separate disability tables)',
  datasets = ds_spec(
    name = '2019_2023_ACS5b',
    data_tables = paste0('B1810', 2:7),
    geog_levels = 'county'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloaded as extract # 28

tab_extract = define_extract_nhgis(
  description = 'Tabular county-level data for race+ethnicity counts, draw 1',
  datasets = ds_spec(
    # same data sources, but maybe we could do something different?
    name = '2019_2023_ACS5a',
    data_tables = c(paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003'),
    geog_levels = 'county'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloaded as extract # 29

tab_extract = define_extract_nhgis(
  description = 'Tabular county-level data for household occupancy + group quarters, draw 1',
  datasets =  map2(
    # same data sources, but maybe we could do something different?
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list('B25002', 'B26001'),
    ~ ds_spec(
      name = .x,
      data_tables = .y,
      geog_levels = 'county'
    )
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloaded as extract #30

### Getting tabular data for age(+sex) for Oregon + Clark counties,
# for verifying Ethan's estimates (march 2025)

tab_extract = define_extract_nhgis(
  description = 'county level age+sex data, draw 1',
  datasets = ds_spec(
    # same data sources, but maybe we could do something different?
    name = '2019_2023_ACS5a',
    data_tables = 'B01001',
    geog_levels = 'county'
  )
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'oregon_oha/01_data_inputs/')
# Downloadd as extract 31
