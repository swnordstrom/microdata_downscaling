# Script for fetching any/all veterans + ancestry/race data (for REALD)

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
    var_spec('COUNTYICP', case_selections = '0510'),
    'PUMA', 'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'VETSTAT', 'OCC', 'GQ',
    'DIFFCARE', 'DIFFMOB', 'DIFFPHYS', 'DIFFREM', 'DIFFSENS',
    'MULTGEN', 'MARST', 'FAMUNIT', 'FAMSIZE'
  )
)

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 54

### Trying again now with `RELATE` field

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age+disability+ancestry, Mult Co. draw 3 with relate+hhtype included',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    'PUMA', 'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'VETSTAT', 'OCC', 'GQ',
    'DIFFCARE', 'DIFFMOB', 'DIFFPHYS', 'DIFFREM', 'DIFFSENS',
    'MULTGEN', 'MARST', 'FAMUNIT', 'FAMSIZE', 'RELATE', 'HHTYPE'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 56

# Download
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 54

### Combining housing data and veterans data into one PUMS

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'PUMS for age+disability+ancestry, Mult Co. draw 4 with housing cost included',
  samples = 'us2023c',
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    var_spec('COUNTYICP', case_selections = '0510'),
    'PUMA', 'AGE', 'SEX',
    'RACAMIND', 'RACASIAN', 'RACBLK', 'RACPACIS', 'RACWHT', 'RACOTHER', 'HISPAN',
    'ANCESTR1', 'ANCESTR2',
    'VETSTAT', 'OCC', 'GQ',
    'DIFFCARE', 'DIFFMOB', 'DIFFPHYS', 'DIFFREM', 'DIFFSENS',
    'MARST', 'RELATE', 
    # Housing cost variables:
    'OWNERSHP', 'OWNCOST', 'RENTGRS', 'HHINCOME'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = 'multnomah/01_raw_data/')
# Downloaded as extract # 59

# # # ------------ Tabular controls

tab_extract = define_extract_nhgis(
  description = 'tract-level veteterans+related for controls, draw 1',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      c(
        'B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'B21001', 
        'B11001', 'B12001'
      ),
      c('B18101', 'B26001', 'B11017')
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
# Downloaded as extract # 35


tab_extract = define_extract_nhgis(
  description = 'tract-level veteterans+related for controls, draw 2 with gq and grandparent tabs',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      c(
        'B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'B21001', 
        'B11001', 'B12001'
      ),
      c('B18101', 'B26001', 'B10063')
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
# Downloaded as extract # 36


tab_extract = define_extract_nhgis(
  description = 'tract-level veteterans+related for controls, draw 3 with housting cost tabs',
  datasets = map2(
    .x = list('2019_2023_ACS5a', '2019_2023_ACS5b'),
    .y = list(
      c(
        'B01001', paste0('B020', c(paste0('0', 8:9), 10:13)), 'B03003', 'B21001', 
        'B11001', 'B12001', 'B25008', 'B25093', 'B25070'
      ),
      c('B18101', 'B26001', 'B10063')
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
# Downloaded as extract # 39
