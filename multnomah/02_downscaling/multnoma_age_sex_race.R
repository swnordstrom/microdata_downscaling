####### == Setup
####### ==
####### ==

# Load packages
library(PMEDMrcpp)
library(ipumsr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Clear namespace
rm(list = ls())

### Read in data

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
pum.m = rbind(
  read.csv('01_raw_data/2010_Census_Tract_to_2010_PUMA.csv') %>%
    filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
    select(-c(STATEFP, COUNTYFP)),
  read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
    filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
    select(-c(STATEFP, COUNTYFP)) 
) %>%
  mutate(PUMA2 = gsub('^\\d{2}(\\d{2}$)', '\\1', PUMA5CE))

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00022.xml')
# Read in sample and select relevant columns
acs.m   = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just relevant PUMAs
  # (ah this won't be super neat...)
  merge(pum.m %>% distinct(PUMA5CE, PUMA2), by.x = 'PUMA', by.y = 'PUMA5CE') %>%
  # Remove unnecessary columns
  select(
    PUMA2, YEAR, SERIAL, PERNUM, PERWT, 
    SEX, AGE, RACE, RACED, HISPAN, HISPAND, 
  )

# Read in NHGIS tables

# tab.raw = read_nhgis('01_raw_data/nhgis0005_csv.zip') %>% 
#   filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
#   select(!matches('^AQ68'))

tab1.raw = read_nhgis('01_raw_data/nhgis0018_csv.zip', file_select = 1) %>%
    filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
    select(TRACT = TRACTA, matches('^AQ'))
tab2.raw = read_nhgis('01_raw_data/nhgis0018_csv.zip', file_select = 2) %>%
    filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
    select(TRACT = TRACTA, matches('^AQ'))

tab.m = merge(tab1.raw, tab2.raw, by = 'TRACT') %>% 
  mutate(TRACT = as.numeric(TRACT)) %>%
  merge(pum.m %>% distinct(TRACTCE, PUMA2), by.x = c('TRACT'), by.y = c('TRACTCE')) %>%
  select(TRACT, PUMA2, everything())

####### == Formatting
####### ==
####### ==


y.m = tab.m %>%
  # Pivot variable columns to rows and merge in variable information
  pivot_longer(-c(TRACT, PUMA2), names_to = 'var_name', values_to = 'value') %>%
  # Merging in variable info
  merge(
    rbind(
      ipums_var_info(tab1.raw, starts_with('AQ')) %>% select(-val_labels),
      ipums_var_info(tab2.raw, starts_with('AQ')) %>% select(-val_labels)
    )
  ) %>%
  # Convert variable info to lowercase for easier regexing
  mutate(across(c(var_label, var_desc), tolower)) %>%
  # Split the variable name up so that we can pivot out estimates and margins
  # for same variable
  separate_wider_position(var_name, c(form = 4, me = 1, code = 3)) %>%
  # Removing estimates/margin of error from var label
  mutate(
    var_label = gsub('estimates:\\s', '', var_label),
    var_label = gsub('margins\\sof\\serror\\:\\s', '', var_label)
  ) %>%
  pivot_wider(names_from = me, values_from = value) 

y.m = y.m %>%
  # Scrape out race easily
  mutate(
    univ = ifelse(
      # if it's the 'total population' universe
      grepl('total', var_desc),
      # scrape out the parenthetical minus 'universe: '
      gsub('.+\\(universe\\:\\s(.+)\\)', '\\1', var_desc),
      # otherwise get the race
      gsub('.+\\(([^\\:]+)\\).*', '\\1', var_desc)
    )
  ) %>%
  # swap out 'under' for '0 to'
  mutate(var_label = gsub('[Uu]nder', '0 to', var_label)) %>%
  # Modify race (collapsing Asian categories)
  # Need to combine the asian and pacific islander columns
  mutate(
    univ = ifelse(
      grepl('([Aa]sian)', univ) | grepl('[Pp]acific', univ),
      'aanhpi',
      univ
    )
  ) %>%
  group_by(TRACT, PUMA2, var_label, univ) %>%
  summarise(
    E = sum(E),
    M = sqrt(sum(M^2))
  ) %>%
  ungroup()

y.m = y.m %>% 
  # Classify constraints
  mutate(
    univ.type = case_when(
      grepl('total', univ) ~ 'total',
      grepl('hisp', univ) ~ 'ethn',
      !grepl('hisp', univ) ~ 'race',
      .default = NA
    )
  ) %>%
  mutate(
    grand.total  = grepl('total', var_label) & univ.type %in% 'total',
    race.total   = grepl('total', var_label) & univ.type %in% 'race',
    ethn.total   = grepl('total', var_label) & univ.type %in% 'ethn',
    sex.total    = grepl('total', univ) & grepl('(fe)?male$', var_label),
    race.sex.total = grepl('(fe)?male$', var_label) & univ.type %in% 'race',
    ethn.sex.total = grepl('(fe)?male$', var_label) & univ.type %in% 'ethn',
    sex.age.total  = grepl('\\d', var_label) & univ.type %in% 'total',
    race.sex.age = grepl('\\d', var_label) & univ.type %in% 'race',
    ethn.sex.age = grepl('\\d', var_label) & univ.type %in% 'ethn'
  ) %>%
  mutate(
    race = ifelse(race.total | race.sex.total | race.sex.age, univ, NA),
    ethn = ifelse(ethn.total | ethn.sex.total | ethn.sex.age, univ, NA),
    sex  = case_when(
      sex.total | race.sex.total | ethn.sex.total ~ var_label,
      sex.age.total | race.sex.age | ethn.sex.age ~ gsub('\\:.+$', '', var_label),
      .default = NA
    ),
    # age col is wrong...
    age = ifelse(
      sex.age.total | race.sex.age | ethn.sex.age,
      gsub('.+\\:\\s(\\d{1,2}).+', '\\1', var_label),
      NA
    ) %>% as.numeric()
  )

y.m = y.m %>%
  arrange(
    desc(grand.total), desc(race.total), desc(ethn.total), desc(sex.total),
    desc(race.sex.total), desc(ethn.sex.total), desc(sex.age.total),
    desc(race.sex.age), desc(ethn.sex.age),
    sex, age, race, ethn, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf) 

age.bins = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% pull()

# Now, format X

x.m.all = acs.m %>%
  select(-c(RACED, HISPAND)) %>%
  mutate(
    age = cut(AGE, c(age.bins, Inf), right = FALSE),
    age = gsub('\\[(\\d{1,2}).+', '\\1', age) %>% as.numeric(),
    race = case_match(
      RACE,
      1 ~ 'white',
      2 ~ 'black',
      3 ~ 'aian',
      4:6 ~ 'aanhpi',
      7 ~ 'other',
      8:9 ~ 'two.or.more'
    ),
    ethn = case_when(
      HISPAN > 0 ~ 'hisp',
      RACE %in% 1 ~ 'nhwh',
      .default = 'nhnw'
    ),
    sex = ifelse(SEX > 1, 'female', 'male')
  )

# desc(grand.total), desc(race.total), desc(ethn.total), desc(sex.total),
# desc(race.sex.total), desc(ethn.sex.total), desc(sex.age.total),
# desc(race.sex.age), desc(ethn.sex.age),
# sex, age, race, ethn, TRACT

x.m = x.m.all %>%
  # Sort
  arrange(sex, age, race, ethn) %>%
  # Time to pivot.
  mutate(
    total = 1,
    race.ones = 1,
    race.cols = race,
    ethn.ones = 1,
    ethn.cols = ethn,
    sex.ones = 1,
    sex.cols = sex,
    race.sex.ones = 1,
    race.sex.race = race,
    race.sex.sex  = sex,
    ethn.sex.ones = 1,
    ethn.sex.ethn = ethn,
    ethn.sex.sex  = sex,
    sex.age.ones = 1,
    sex.age.sex  = sex,
    sex.age.age  = age,
    race.sex.age.ones = 1,
    race.sex.age.race = race,
    race.sex.age.sex  = sex,
    race.sex.age.age  = age,
    ethn.sex.age.ones = 1,
    ethn.sex.age.ethn = ethn,
    ethn.sex.age.sex  = sex,
    ethn.sex.age.age  = age
  ) %>%
  pivot_wider(names_from = race.cols, values_from = race.ones, values_fill = 0) %>%
  pivot_wider(names_from = ethn.cols, values_from = ethn.ones, values_fill = 0) %>%
  pivot_wider(names_from = sex.cols,  values_from = sex.ones , values_fill = 0) %>%
  pivot_wider(
    names_from = c(race.sex.race, race.sex.sex), names_sep = '_',
    values_from = race.sex.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(ethn.sex.ethn, ethn.sex.sex), names_sep = '_',
    values_from = ethn.sex.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.sex, sex.age.age), names_sep = '_',
    values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(race.sex.age.race, race.sex.age.sex, race.sex.age.age), names_sep = '_',
    values_from = race.sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(ethn.sex.age.ethn, ethn.sex.age.sex, ethn.sex.age.age), names_sep = '_',
    values_from = ethn.sex.age.ones, values_fill = 0
  ) %>%
  select(-contains('nhnw')) %>%
  arrange(YEAR, SERIAL, PERNUM)

names(x.m)[-(1:13)] %>% length()
y.m %>% filter(TRACT %in% 101, PUMA2 %in% '03') %>% nrow()


data.frame(
  xn = names(x.m)[-(1:13)][81:120],
  yn = y.m %>% filter(TRACT %in% 101) %>% select(race, ethn, sex, age) %>% slice(81:120)
)
# hohoho.....

# ahhhh okay there are different age bins for the race/ethnicity and overall tables lmaooo

