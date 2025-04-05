# Fetching data for very first downscaling attempt
# Downscaling age+race+income-poverty-ratio to census tracts in OR
# run 11 oct 2024

library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for poverty+age+race downscaling, draw 1',
  samples = c('us2021a', 'us2022a'),
  variables = c(
    'STATEFIP', 'COUNTYFIP', 'PUMA', 'AGE', 
    'RACE', 'US2021A_POVPIP', 'US2022A_POVPIP'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

tab_extract = define_extract_nhgis(
  description = 'Poverty + poverty-age + race census tract, draw 1',
  datasets = map(
    c('2018_2022_ACS5b'),
    ~ ds_spec(
      name = .x,
      data_tables = c(
        # 'B01001', # Age (by sex) # (not available in compiled form?)
        # 'B02001', # Race
        paste0('B01001', LETTERS[1:9]), # Age by sex for race groups A-I
        'B17024'# , # Age by ratio of income to poverty level
        # 'C17024'  # Age by ratio of income to poverty level (collapsed) # (also not available lol)
      ),
      geog_levels = 'tract'
    )
  )
)

# Okay I guess this works!
tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')

# # ---- 17 Oct 2024: adding in tract-level overall estimates

tab_extract = define_extract_nhgis(
  description = 'Tract-level total population estimates, draw 1',
  datasets = ds_spec('2018_2022_ACS5a', data_tables = 'B01003', geog_levels = 'tract')
)

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')


# # ---- 25 Oct 2024: new extract including sex and hispanic ancestry to ACS
# # ----              query and removing 2021 estimates

rm(list = ls())

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for pov+age+race+sex+hisp downscaling, draw 2 (more years)',
  samples = paste0('us2022a'),
  variables = c(
    'STATEFIP', 'COUNTYFIP', 'PUMA', 'AGE', 
    'RACE', 'SEX', 'HISPAN', 'US2022A_POVPIP'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')


# # ---- 17 Dec 2024: new extract including sex and hispanic ancestry to ACS
# # ----              this time including more years AND just Oregon

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (2020 PUMAs) for pov+age+race+sex+hisp downscaling, draw 3 (more years + only OR)',
  samples = paste0('us', 2018:2022, 'a'),
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'AGE', 
    'RACE', 'SEX', 'HISPAN',
    # ah... paste0 won't work nicely lmao
    'US2018A_POVPIP',
    'US2019A_POVPIP',
    'US2020A_POVPIP',
    'US2021A_POVPIP',
    'US2022A_POVPIP'
  )
)
  
acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')


# # ---- 18 Dec 2024: NHGIS for age, sex, race, no poverty, but with overall age
#                     cols this time


tab_extract = define_extract_nhgis(
  description = 'Age + race + sex / census tract, draw 1',
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

# ah... error above, I guess b01001 is only in acs5a

tab_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')


# # ---- 02 Jan 2025: PUMS from 5-year ACS instead of one year
#                     

acs_extract = define_extract_micro(
  collection = 'usa',
  description = 'ACS data (5-year) for pov+age+race+sex+hisp downscaling, draw 4',
  samples = paste0('us', 2018:2022, 'c'),
  variables = list(
    var_spec('STATEFIP', case_selections = '41'),
    'COUNTYFIP', 'PUMA', 'AGE', 
    'RACE', 'SEX', 'HISPAN', 'POVERTY'
  )
)

acs_extract %>%
  submit_extract() %>%
  wait_for_extract() %>%
  download_extract(download_dir = '01_raw_data/')
# downloaded as extract 40