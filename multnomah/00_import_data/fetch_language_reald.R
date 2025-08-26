library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#### ACS PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'PUMS for language + ancestry, draw 1',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    # Identifiers (and weights)
    'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'LANGUAGE', 'SPEAKENG'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 60

acs_extract = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'PUMS for language + ancestry, draw 2 including race categorized',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    # Identifiers (and weights)
    'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    'RACE', 'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'LANGUAGE', 'SPEAKENG'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as # 61

acs_extract = define_extract_micro(
  collection = 'usa',
  # NOTE: forgot to change this before submitting so the description is old and wrong
  description = 'PUMS for language + ancestry, draw 3 including birthplace+citizenship',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    # Identifiers (and weights)
    'PUMA', 'SERIAL', 'PERNUM', 'PERWT', 
    # Variables for downscale
    'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'LANGUAGE', 'SPEAKENG',
    'BPL', 'CITIZEN'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as # 62


# # # ------------ Tabular controls

tab_extract = define_extract_nhgis(
  description = 'tract-level language+ancesry+race for controls, draw 1',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      # Age/sex, race + hispanic, language proficiency
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003'),
      # BUT: is group quarters count is not the same as institutional count...
      c('C16001')
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
# Downloaded as extract # 40


tab_extract = define_extract_nhgis(
  description = 'tract-level language+ancesry+race for controls, draw 2 with age-sex-race included',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      # Age/sex, race + hispanic, language proficiency
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003'),
      # BUT: is group quarters count is not the same as institutional count...
      c('C16001', paste0('B01001', LETTERS[1:9]))
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
# Downloaded as extract # 41


tab_extract = define_extract_nhgis(
  description = 'tract-level language+ancesry+race for controls, draw 3 with citizenship',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      # Age/sex, race + hispanic, language proficiency
      c('B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003'),
      c('B05001', 'C16001')
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
# Downloaded as extract # 42
