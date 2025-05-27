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
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00057.xml')

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro(pums.raw.ddi)

# Read in tabular data
acs.tab.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0038_csv.zip', file_select = 1) %>%
  filter(grepl('Multno', COUNTY))
acs.tab.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0038_csv.zip', file_select = 2) %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')

# Synthetic records
# Get synthetic records (PUMS and dummy)
# synthetic.records = read.csv('')

# ==============================================================
# Merge data products together and do some pre-processing

# ACS tabular controls
acs.tab = merge(
  acs.tab.raw1 %>% select(TRACTA, matches('[EM]\\d{3}$')),
  acs.tab.raw2 %>% select(TRACTA, matches('[EM]\\d{3}$'))
) %>%
  # Rename tract column for merge
  mutate(tract = as.numeric(TRACTA)) %>%
  select(-TRACTA) %>%
  # Merge to get PUMAs
  merge(puma.tract %>% filter(puma > 5000) %>% select(tract, puma)) %>%
  # Do some label name/constraint massaging 
  pivot_longer(-c(puma, tract), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    rbind(
      ipums_var_info(acs.tab.raw1, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(acs.tab.raw2, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
    )
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
    # Handle age bins (no decimal)
    var_label = gsub('under\\s(\\d)', '00 to \\1', var_label),
    var_label = gsub('^(\\d{1})\\s', '0\\1 ', var_label),
    var_label = gsub('\\:\\s(\\d{1})\\s', ': 0\\1 ', var_label),
    # Handle percentages
    var_label = gsub('less\\sthan\\s(\\d)', '00.0 to \\1', var_label)
  ) %>%
  # Get rid of some duplicate cases
  filter(
    # Hispanic total (same as sex/age total)
    !(form %in% 'ASOB' & var_label %in% 'total')
  ) %>%
  # Change 'total' label to race for race columns
  mutate(
    var_label = ifelse(
      form %in% paste0('ASN', 4:9),
      gsub('^(.+)\\salone.+', '\\1', var_desc),
      var_label
    )
  )

# Format the ancestry table
ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) %>%
  mutate(
    form = NA, var_desc = 'ancestry re-groupings', univ = 'people reporting ancestry'
  ) %>%
  select(names(acs.tab))

# Combine the acs tables into one:
acs.tab = rbind(acs.tab, ancestry.tab)

### PUMS microdata
# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, COUNTYICP,
      STRATA, HISPAND, ANCESTR1D, ANCESTR2D
    )
  )


# ==============================================================
# Get matrices/vectors for PMED-M


y.tab = acs.tab %>%
  mutate(
    ### various totals
    # grand
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    # housing tenure total
    is.total.ten = form %in% 'ASTM' & var_label %in% 'total',
    # renter total
    is.total.ren = form %in% 'ASVH' & var_label %in% 'total',
    # owner total
    is.total.own = form %in% 'ASWA' & var_label %in% 'total',
    ### race-ethnicity
    is.rac = form %in% paste0('ASN', 4:9),
    is.hsp = form %in% 'ASOB',
    ### sex/age-sex
    # grand-total for sexes
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    # age-sex
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    # housing tenure
    is.ten = form %in% 'ASTM' & grepl('occupied$', var_label),
    # renter percentage
    is.ren.pct = form %in% 'ASVH' & grepl('percent', var_label),
    # renter percentage (NA)
    is.ren.pct.na = form %in% 'ASVH' & grepl('not\\scomput', var_label),
    # renter other/NA?
    # householder by age
    is.own.age = form %in% 'ASWA' & grepl('years', var_label) & !grepl('\\:', var_label),
    # householder by age by percent
    is.own.age.pct = form %in% 'ASWA' & grepl('\\:\\s\\d', var_label),
    # householder by age by percent (NA)
    is.own.age.pct.na = form %in% 'ASWA' & grepl('not\\scomput', var_label),
    # group quarters
    is.gquart = form %in% 'ATFB',
    # ancestry
    is.anc = grepl('ancestry', var_desc)
  ) %>%
  arrange(
    # Arrange totals
    desc(is.total), desc(is.total.ten), desc(is.total.ren), desc(is.total.own),
    # Arrange race and ethnicity
    desc(is.rac), desc(is.hsp),
    # Arrange age/age-sex
    desc(is.sex), desc(is.sex.age),
    # Arrange housing tenure
    desc(is.ten),
    # Arrange renter cost columns
    desc(is.ren.pct), desc(is.ren.pct.na),
    # Arrange ownership cost
    desc(is.own.age), desc(is.own.age.pct), desc(is.own.age.pct.na),
    # Arrange group quarters
    desc(is.gquart),
    # Arrange ancestry
    desc(is.anc),
    # Alphabetize within groups and arrange tracts
    var_label, tract
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

### Get age bins

ages.fine = y.tab %>%
  filter(vartype %in% 'is.sex.age') %>%
  distinct(var_label) %>%
  mutate(age = gsub('(fe)?male\\:\\s(\\d{2}).*', '\\2', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

ages.own = y.tab %>%
  filter(vartype %in% 'is.own.age') %>%
  distinct(var_label) %>%
  mutate(age = gsub('householder\\s(\\d{2}).*', '\\1', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

pcts.ren = y.tab %>% 
  filter(vartype %in% 'is.ren.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('^(\\d{2}).+', '\\1', var_label)) %>%
  distinct(pct) %>% arrange() %>% pull()

pcts.own = y.tab %>% 
  filter(vartype %in% 'is.own.age.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('.+\\:\\s(\\d{2}).+', '\\1', var_label)) %>%
  distinct(pct) %>% arrange() %>% pull()

ages.fine
ages.own
pcts.ren
pcts.own

### Format PUMS (X)

