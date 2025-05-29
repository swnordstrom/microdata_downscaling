library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00056.xml') %>%
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
weights = read.csv('multnomah/03_downscale_out/vets-and-housing_all_weights.csv')

# Get MultCo tract geographies for plots
multco.tracts = tracts('OR', 'Multnomah') %>%
  mutate(tract = as.numeric(TRACTCE))

# Get synthetic records (PUMS and dummy)
synthetic.pums = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_vet_hou_synthetic_records.csv') %>%
  # Remove the ancestry-only records
  filter(!is.na(CBSERIAL))

# %>%
#   # Merge in with PUMS to get other data
#   merge(pums.raw %>% select(-PUMA), by = c('CBSERIAL', 'PERNUM')) %>%
#   select(names(pums.raw)) %>%
#   # Remove duplicates
#   # (these cause issues when getting the disability-all varb and the duplicates
#   # with weighting get re-assigned later anyway)
#   distinct(CBSERIAL, PERNUM, .keep_all = TRUE)


# ==============================================================
# Neaten and combine

# Add reald categories to both PUMS datasets and rbind them together
# (it's easier to add reald before they are combined)
pums.processed = pums.raw %>%
  mutate(
    # Get disability
    has.diff = ifelse(
      (DIFFSENS > 1) | (DIFFMOB > 1) | (DIFFPHYS > 1) | (DIFFCARE > 1) | (DIFFREM > 1),
      'with.disb',
      'no.disb'
    ),
    # Recode veteran
    veteran = case_match(
      VETSTAT,
      0:1 ~ 'nonvet',
      2 ~ 'vet'
    ),
    # Recode marital status
    mar = case_match(
      MARST,
      1:3 ~ 'now.married',
      4 ~ 'divorced',
      5 ~ 'widowed',
      6 ~ 'nev.mar'
    ),
  ) %>%
  # Add household stats
  group_by(CBSERIAL) %>%
  mutate(
    lives.alone = ifelse(n() < 2, 'liv.alone', 'not.liv.alone'),
    # family.hous = ifelse(any(PERNUM != FAMUNIT), 'family.hou', 'nonfam.hou')
    family.hous = ifelse(any(RELATE[PERNUM > 1] <= 10), 'fam.hou', 'nonfam.hou'),
    grandparent = ifelse(
      ((any(RELATE %in% 9)) | (any(RELATE %in% 3:4) & any(RELATE %in% 5:6))),
      'witha.grandparent',
      'without.grandparent'
    )
  ) %>%
  ungroup() %>%
  merge(reald %>% select(CBSERIAL, PERNUM, reald))
  
pums.synthetic.processed = merge(
  synthetic.pums %>% select(CBSERIAL, PERNUM, PUMA),
  pums.processed %>% select(-PUMA)
) %>%
  select(names(pums.processed)) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1)
  
#   rbind(
#   merge(pums.raw, reald %>% select(CBSERIAL, PERNUM, reald)),
#   merge(
#     synthetic.pums %>% mutate(new.serial = -1 * (1:nrow(.))), 
#     reald %>% select(CBSERIAL, PERNUM, reald)
#   ) %>%
#     mutate(CBSERIAL = new.serial) %>%
#     select(-new.serial)
# )

# Combine PUMS and do other processing steps
pums = rbind(pums.processed, pums.synthetic.processed) %>%
  # Add binned age
  mutate(age = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE))

# Now merge PUMS and weight
# pums.weight = merge(pums, weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'))


# ==============================================================
# Table 1: total veterans

vet.table = pums %>%
  filter(VETSTAT > 1) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.table.plot = merge(multco.tracts, vet.table)

ggplot(vet.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

vet.table = vet.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.table)

# ==============================================================
# Table 2: veterans with disabilities

vet.disb.table = pums %>%
  filter(VETSTAT > 1, has.diff %in% 'with.disb') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.disb.table.plot = merge(multco.tracts, vet.disb.table)

ggplot(vet.disb.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

vet.disb.table = vet.disb.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.disb.table)

# ==============================================================
# Table 3: veterans in family households

vet.fam.table = pums %>%
  filter(VETSTAT > 1, family.hous %in% 'fam.hou') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.fam.table.plot = merge(multco.tracts, vet.fam.table)

ggplot(vet.fam.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.fam.table = vet.fam.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.fam.table)


# ==============================================================
# Table 4: veterans living alone

vet.alo.table = pums %>%
  filter(VETSTAT > 1, lives.alone %in% 'liv.alone') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.alo.table.plot = merge(multco.tracts, vet.alo.table)

ggplot(vet.alo.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.alo.table = vet.alo.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.alo.table)

# ==============================================================
# Table 5: veterans living in group quarters

vet.gq.table = pums %>%
  filter(VETSTAT > 1, GQ %in% (3:5)) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.gq.table.plot = merge(multco.tracts, vet.gq.table)

ggplot(vet.gq.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.gq.table = vet.gq.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.gq.table)


# ==============================================================
# Table 6: veterans living in multigenerational households

vet.mul.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1, grandparent %in% 'witha.grandparent') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.mul.table.plot = merge(multco.tracts, vet.mul.table)

ggplot(vet.mul.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')
# good!

# Convert back to wide
vet.mul.table = vet.mul.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.mul.table)


# ==============================================================
# Table 7: veterans in nonfamily households

vet.nonfam.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1, family.hous %in% 'nonfam.hou') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.nonfam.table.plot = merge(multco.tracts, vet.nonfam.table)

ggplot(vet.nonfam.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')
# good!

# Convert back to wide
vet.nonfam.table = vet.nonfam.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.nonfam.table)


# ==============================================================
# Table 8: single, never-married veterans

vet.single.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1, mar %in% 'nev.mar') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.single.table.plot = merge(multco.tracts, vet.single.table)

ggplot(vet.single.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.single.table = vet.single.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.single.table)


# ==============================================================
# Table 9: veterans currently married

vet.mar.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1, mar %in% 'now.married') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.mar.table.plot = merge(multco.tracts, vet.mar.table)

ggplot(vet.mar.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.mar.table = vet.mar.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.mar.table)


# ==============================================================
# Table 10: veterans divorced or widowed

vet.divwid.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1, mar %in% c('divorced', 'widowed')) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.divwid.table.plot = merge(multco.tracts, vet.divwid.table)

ggplot(vet.divwid.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.divwid.table = vet.divwid.table %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(vet.divwid.table)


# ==============================================================
# Table 11: veterans by race/ethnicity

vet.reald.table = pums %>%
  # Filter out only family households and veterans
  filter(VETSTAT > 1) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
vet.reald.table.plot = merge(multco.tracts, vet.reald.table)

ggplot(vet.reald.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')

# Convert back to wide
vet.reald.table = vet.reald.table %>%
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

head(vet.reald.table)


# ==============================================================
# Table 12: race/ethnicity totals

reald.total.table = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.total.table.plot = merge(multco.tracts, reald.total.table)

ggplot(reald.total.table.plot) +
  geom_sf(aes(fill = log(TOTAL, base = 10))) +
  scale_fill_viridis_c(breaks = (-1:3), labels = c(0.1, 1, 10, 100, 1000)) +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# Less interesting than I was hoping!

# Convert back to wide
reald.total.table = reald.total.table %>%
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

head(reald.total.table)


# ==============================================================
# Table 13: people in family households by race/ethnicity

reald.fam.table = pums %>%
  filter(family.hous %in% 'fam.hou') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.fam.table.plot = merge(multco.tracts, reald.fam.table)

ggplot(reald.fam.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')


# Convert back to wide
reald.fam.table = reald.fam.table %>%
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

head(reald.fam.table)


# ==============================================================
# Table 14: people in living alone by race/ethnicity

reald.alo.table = pums %>%
  filter(lives.alone %in% 'liv.alone') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.alo.table.plot = merge(multco.tracts, reald.alo.table)

ggplot(reald.alo.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# A few missing combinations

# Here's a look at the order of magnitude (col names are log 10 scale) by counts for each race
# reald.alo.table %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(age, reald) %>%
#   print(n = nrow(.))
#
# okay... single-digit number under 18 per tract, at most
# at older groups though, seems possible

# Convert back to wide
reald.fam.table = reald.fam.table %>%
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

head(reald.fam.table)


# ==============================================================
# Table 15: people in group quarters by race/ethnicity

reald.gq.table = pums %>%
  filter(GQ %in% 3:5) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.gq.table.plot = merge(multco.tracts, reald.gq.table)

ggplot(reald.gq.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# Ah - some reald-age combos are missing from the PUMS

# Here's a look at the order of magnitude (col names are log 10 scale) by counts for each race
# reald.gq.table %>% 
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>% 
#   count(age, reald, total) %>% 
#   pivot_wider(names_from = total, values_from = n) %>% 
#   arrange(age, reald) %>% 
#   print(n = nrow(.))
# 
# juvenile counts may be small (<10), 55-60 counts look very small

# Convert back to wide
reald.gq.table = reald.gq.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)

head(reald.gq.table)
# NOTE: zeros imputed here


# ==============================================================
# Table 16: people in multigenerational family households by race/ethnicity

reald.mul.table = pums %>%
  filter(grandparent %in% 'witha.grandparent') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.mul.table.plot = merge(multco.tracts, reald.mul.table)

ggplot(reald.mul.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# Same as above - a handful of missing groups

# # Counts of tracts by order of magnitude of estimated counts
# reald.mul.table %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(age, reald) %>%
#   print(n = nrow(.))

# Other: the max number of multi-generational "other" in any given tract is 1...
# Likely missing NHPI and American Indian individuals 85+,

# Convert back to wide
reald.mul.table = reald.mul.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# NOTE: zeros imputed here

head(reald.mul.table)


# ==============================================================
# Table 17: people in nonfamily households by race/ethnicity

reald.nonfam.table = pums %>%
  filter(family.hous %in% 'nonfam.hou') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.nonfam.table.plot = merge(multco.tracts, reald.nonfam.table)

ggplot(reald.nonfam.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# Missing MENA 0-18 in nonfamily households

# reald.nonfam.table %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(age, reald) %>%
#   print(n = nrow(.))
# # Seems pretty likely that the MENA <18 nonfamily population is very small...

# Convert back to wide
reald.nonfam.table = reald.nonfam.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# Note: zeros imputed here

head(reald.nonfam.table)

# ==============================================================
# Table 18: never-married people by race/ethnicity

reald.single.table = pums %>%
  filter(mar %in% 'nev.mar') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.single.table.plot = merge(multco.tracts, reald.single.table)

ggplot(reald.single.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) + s
  theme(legend.position = 'top')
# Missing 85+ year olds in most groups

reald.single.table %>%
  mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
  count(age, reald, total) %>%
  pivot_wider(names_from = total, values_from = n) %>%
  arrange(reald, age) %>%
  print(n = nrow(.))
# Having few individuals 85+ never married is likely
# 

# Convert back to wide
reald.single.table = reald.single.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# NOTE: zeros imputed here
# ALSO NOTE: universe for marital status is ages 15+ - consider removing
# youngest group

head(reald.single.table)


# ==============================================================
# Table 19: married people by race/ethnicity

reald.mar.table = pums %>%
  filter(mar %in% 'now.married') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.mar.table.plot = merge(multco.tracts, reald.mar.table)

ggplot(reald.mar.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# This is fine

reald.mar.table %>%
  mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
  count(age, reald, total) %>%
  pivot_wider(names_from = total, values_from = n) %>%
  arrange(age, reald) %>%
  print(n = nrow(.))
# Looks like if any it would just be a single-digit number of Indigenous people
# >85 per tract

# Convert back to wide
reald.mar.table = reald.mar.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# Note: zeros imputed here
# (also see note about marriage universe above)

head(reald.mar.table)


# ==============================================================
# Table 20: divorced or widowed people by race/ethnicity

reald.divwid.table = pums %>%
  filter(mar %in% c('divorced', 'widowed')) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Now aggregate by tract/age
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.divwid.table.plot = merge(multco.tracts, reald.divwid.table)

ggplot(reald.divwid.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(legend.position = 'top')
# All missing ones are in the youngest age group...

# Convert back to wide
reald.divwid.table = reald.divwid.table %>%
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
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# Note: zeros imputed here
# (also see note about marriage universe above)

head(reald.mar.table)

# ==============================================================
# Aggregate all tables


