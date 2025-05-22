library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00053.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Read in and process the reald assignments
reald = read.csv('multnomah/01_raw_data/5acs23_orwa_reldpri.csv') %>%
  # modify serial number for merge
  mutate(
    CBSERIAL = gsub('GQ', '01', serialno),
    CBSERIAL = gsub('HU', '00', CBSERIAL),
    CBSERIAL = as.numeric(CBSERIAL)
  ) %>%
  # change person number identifier for merge
  rename(PERNUM = sporder) %>%
  # assign reald categories
  mutate(
    reald = case_match(
      reldpri,
      c('HisMex', 'HisCen', 'HisOth', 'HisSou') ~ 'HispLat',
      c('NHPIoth', 'Cham', 'Samoan', 'COFA', 'Marshall', 'NatHaw') ~ 'NHPI',
      c('WestEur', 'WhiteOth', 'Slavic', 'EastEur') ~ 'White',
      c('AmInd', 'AlaskNat', 'LatInd') ~ 'AmInd',
      c('AfrAm', 'African', 'Caribbean', 'Ethiopian', 'Somali') ~ 'Black', 
      c('MidEast', 'NoAfr') ~ 'MENA',
      c('Chinese', 'Filipino', 'Cambodian', 'Vietnamese', 'Korean', 
        'Japanese', 'Myanmar', 'AsianInd', 'SoAsian', 'Hmong', 
        'Laotian', 'AsianOth') ~ 'Asian',
      'RaceOth' ~ 'Other'
    )
  )

# Read in estimates
weights = read.csv('multnomah/03_downscale_out/poverty_all_weights.csv')

# Get MultCo tract geographies for plots
multco.tracts = tracts('OR', 'Multnomah') %>%
  mutate(tract = as.numeric(TRACTCE))

# Get synthetic records (PUMS and dummy)
synthetic.pums = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_poverty_synthetic_records.csv') %>%
  # Remove the ancestry-only records
  filter(!is.na(CBSERIAL)) %>%
  # Merge in with PUMS to get other data
  merge(pums.raw %>% select(-PUMA), by = c('CBSERIAL', 'PERNUM')) %>%
  select(names(pums.raw)) %>%
  # Remove duplicates
  # (these cause issues when getting the disability-all varb and the duplicates
  # with weighting get re-assigned later anyway)
  distinct(CBSERIAL, PERNUM, .keep_all = TRUE)


# ==============================================================
# Neaten and combine

# Add reald categories to both PUMS datasets and rbind them together
# (it's easier to add reald before they are combined)
pums = rbind(
  merge(pums.raw, reald %>% select(CBSERIAL, PERNUM, reald)),
  merge(synthetic.pums, reald %>% select(CBSERIAL, PERNUM, reald)) %>%
    mutate(CBSERIAL = -1 * CBSERIAL)
)

# Combine PUMS and do other processing steps
pums = pums %>%
  # Give me only individuals with poverty listed (in universe)
  filter(POVERTY > 0) %>%
  # Bin age and poverty
  mutate(age = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE))
  
# Now merge PUMS and weight
pums.weight = merge(pums, weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'))


# ==============================================================
# Table 1: whole poverty universe

univ.table = pums.weight %>%
  group_by(tract, age, reald) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
univ.table.plot = merge(multco.tracts, univ.table)

ggplot(univ.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# a few missing 

univ.table = univ.table %>%
  mutate(
    age = LETTERS[as.numeric(age)],
    reald = case_match(
      reald,
      'White' ~ 'W',
      'Asian' ~ 'A',
      'AmInd' ~ 'N',
      'Black' ~ 'B',
      'NHPI'  ~ 'P',
      'MENA'  ~ 'E',
      'HispLat' ~ 'H',
      'Other' ~ 'O'
    )
  ) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(univ.table)
# should also pivot out reald

# ==============================================================
# Table 2: below the poverty line

sub100.table = pums.weight %>%
  group_by(tract, age, reald, sub.100 = POVERTY < 100) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = sub.100, values_from = TOTAL, names_prefix = 's100', values_fill = 0)

### For plotting:
sub100.table.plot = merge(multco.tracts, sub100.table)

ggplot(sub100.table.plot) +
  geom_sf(aes(fill = s100TRUE / (s100TRUE + s100FALSE))) +
  scale_fill_viridis_c('proportion\nbelow 100%') +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')

# Looks alright

sub100.table = sub100.table %>%
  select(-s100FALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = s100TRUE)

head(sub100.table)


# ==============================================================
# Table 3: below 2.5X the poverty line

sub250.table = pums.weight %>%
  group_by(tract, age, reald, sub.250 = POVERTY < 250) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = sub.250, values_from = TOTAL, names_prefix = 's250', values_fill = 0)

### For plotting:
sub250.table.plot = merge(multco.tracts, sub250.table)

ggplot(sub250.table.plot) +
  geom_sf(aes(fill = s250TRUE / (s250TRUE + s250FALSE))) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')

# Oof... the kids

sub250.table = sub250.table %>%
  select(-s250FALSE) %>%
  mutate(
    age = LETTERS[as.numeric(age)],
    reald = case_match(
      reald,
      'White' ~ 'W',
      'Asian' ~ 'A',
      'AmInd' ~ 'N',
      'Black' ~ 'B',
      'NHPI'  ~ 'P',
      'MENA'  ~ 'E',
      'HispLat' ~ 'H',
      'Other' ~ 'O'
    )
  ) %>%
  pivot_wider(names_from = age, values_from = s250TRUE)

head(sub250.table)


# ==============================================================
# Table 4: below 4X the poverty line

sub400.table = pums.weight %>%
  group_by(tract, age, reald, sub.400 = POVERTY < 400) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = sub.400, values_from = TOTAL, names_prefix = 's400', values_fill = 0)

### For plotting:
sub400.table.plot = merge(multco.tracts, sub400.table)

ggplot(sub400.table.plot) +
  geom_sf(aes(fill = s400TRUE / (s400TRUE + s400FALSE))) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')

# Clear east-west dividing line at 

sub400.table = sub400.table %>%
  select(-s400FALSE) %>%
  mutate(
    age = LETTERS[as.numeric(age)],
    reald = case_match(
      reald,
      'White' ~ 'W',
      'Asian' ~ 'A',
      'AmInd' ~ 'N',
      'Black' ~ 'B',
      'NHPI'  ~ 'P',
      'MENA'  ~ 'E',
      'HispLat' ~ 'H',
      'Other' ~ 'O'
    )
  ) %>%
  pivot_wider(names_from = age, values_from = s400TRUE)

head(sub400.table)

