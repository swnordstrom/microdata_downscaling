library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# ==============================================================
# Read in raw and curated data

# Get PUMS ddi
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00053.xml')

# Read in PUMS and subset to MultCo
pums.raw = pums.raw.ddi %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Read in tabular data
acs.tab.raw = read_nhgis('multnomah/01_raw_data/nhgis0033_csv.zip') %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')


# ==============================================================
# Combine and massage data (before formatting for data)

### Handle tabular constraints

# Format ACS tables and merge in to get PUMAs for each tract
acs.tab = acs.tab.raw %>% 
  # Get tract and tabular values only
  select(TRACTA, matches('[EM]\\d{3}$')) %>%
  # Rename tract column for merge
  mutate(tract = as.numeric(TRACTA)) %>%
  select(-TRACTA) %>%
  # Merge to get PUMAs
  merge(puma.tract %>% filter(puma > 5000) %>% select(tract, puma))

head(acs.tab)

# Do some label name/constraint massaging 

acs.tab = acs.tab %>%
  pivot_longer(-c(puma, tract), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    ipums_var_info(acs.tab.raw, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
  ) %>%
  # Get rid of 'Margins of error' or 'Estimates' in val label
  mutate(var_label = sub('^[^\\:]+\\:\\s', '', tolower(var_label))) %>%
  # Get var code for pivoting
  separate_wider_position(var_name, widths = c(form = 4, em = 1, 3)) %>%
  pivot_wider(names_from = em, values_from = var_value) %>%
  # Remove table name to separate out var description and universe
  mutate(
    var_desc = gsub('^table\\s[A-Za-z0-9]{4}\\:\\s', '', tolower(var_desc)),
    var_desc = gsub('\\)$', '', var_desc)
  ) %>%
  separate_wider_delim(var_desc, delim = ' (universe: ', names = c('var_desc', 'univ')) %>%
  # Change age labels ('Under' to '00 to', and add preceding zero where needed)
  mutate(
    var_label = gsub('under\\s\\.', '0.00 to 0.', var_label),
    var_label = gsub('under\\s(\\d)', '00 to \\1', var_label),
    var_label = gsub('^\\.(\\d{2})', '0.\\1', var_label),
    var_label = gsub('\\s\\.(\\d{2})', ' 0.\\1', var_label),
    var_label = gsub('\\s(\\d{1})\\s', ' 0\\1 ', var_label)
  )

# Format the ancestry table
ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) 


### Working with PUMS

# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, 
      STRATA, HISPAND, ANCESTR1D, ANCESTR2D,
    )
  ) %>%
  mutate(PUMA = PUMA + ifelse(PUMA < 5000, (5100-1300), 0))

nrow(pums)
table(pums$PUMA)

# # Format the other ancestry dummy record sfor controls
# ancestry.dummies = synthetic.records %>%
#   filter(is.na(CBSERIAL)) %>%
#   mutate(
#     # Add in serial number, person number, weight
#     CBSERIAL = -1 * (1:nrow(.)),
#     PERNUM = 1,
#     PERWT = 1,
#   ) %>%
#   # Arrange first four columns
#   select(PUMA, CBSERIAL, PERNUM, PERWT, ancestry) 


# ==============================================================
# Combine and massage data (before formatting for data)

# ==============================================================
# Now start formatting data

# all.vars = acs.tab %>% distinct(form, var_label, var_desc, univ)

y.tab = acs.tab %>%
  # Filter out duplicate cases
  filter(
    # Total in Hispanic/Latino (ASOB) table is a duplicate
    !(var_label %in% 'total' & form %in% 'ASOB')
  ) %>%
  # Classify 
  mutate(
    # Get totals (tract-level total is in ASNQ)
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    # Whole poverty universe
    is.total.pov.univ = form %in% 'ASQI' & var_label %in% 'total',
    # Total sex
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    # Sex-age (whole population)
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    # Race totals
    is.race = form %in% paste0('ASN', 4:9),
    # Hispanic ethnicity
    is.hisp = form %in% 'ASOB',
    # Poverty level
    is.pov  = form %in% 'ASQI' & grepl('\\d', var_label)
  ) %>%
  # Change the var_label to universe for the race (var_label is currently 'total')
  mutate(var_label = ifelse(is.race, univ, var_label)) %>%
  # Now, arrange rows
  arrange(
    # Grand total + poverty universe total
    desc(is.total), desc(is.total.pov.univ),
    # Race and Hispanic ethnicity
    desc(is.race), desc(is.hisp),
    # Sex totals 
    desc(is.sex), 
    # Sex-age (grand and disability universe)
    desc(is.sex.age),
    # Sex-age-disability combos
    desc(is.pov),
    # Now sort within constraints
    var_label,
    # sort geographies
    tract
  ) %>%
  # Pivot the constraint label columns
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf) %>%
  rbind(
    ancestry.tab %>%
      mutate(form = NA, var_desc = NA, univ = NA, vartype = 'is.anc') %>%
      select(form, tract, puma, var_label, var_desc, univ, E, M, vartype)
  )

y.tab
nrow(y.tab) / 197
