library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
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
    reald.cat = case_match(
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
weights = read.csv('multnomah/03_downscale_out/disability_all_weights.csv')

# Get MultCo tract geographies for plots
multco.tracts = tracts('OR', 'Multnomah') %>%
  mutate(tract = as.numeric(TRACTCE))

# Synthetic pums (generated in `make_reld_ancestry_synthetic_records.R`)
synthetic.pums = read.csv('multnomah/02_downscaling/phaseII/reld_all_synthetic_records.csv') %>%
  # Remove the ancestry-only records
  filter(!is.na(CBSERIAL))

# ==============================================================
# Merge and process data

# Add reald categories to both PUMS datasets and rbind them together
# (it's easier to add reald before they are combined)
pums.processed = pums.raw %>%
  # in civilian noninstitutionalized population (disability universe)
  mutate(in.disb.univ = !(OCC %in% 9800:9850) & !(GQ %in% 3)) %>%
  merge(reald %>% select(CBSERIAL, PERNUM, reald = reald.cat))

pums.synthetic.processed = merge(
  synthetic.pums %>% select(CBSERIAL, PERNUM, PUMA),
  pums.processed %>% select(-PUMA)
) %>%
  select(names(pums.processed)) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1)

pums = rbind(pums.processed, pums.synthetic.processed) %>%
  filter(in.disb.univ) %>%
  select(-c(starts_with('RAC'), starts_with('ANCESTR'))) %>%
  mutate(age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE))


# ==============================================================
# Table: total disability universe (all civilian non-institutionalized)

### Aggregate for sum
total.univ = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.univ)

### Visualize
total.univ.plot = merge(multco.tracts, total.univ) %>%
  group_by(tract) %>%
  mutate(p_age_within_tract = TOTAL / sum(TOTAL)) %>%
  group_by(age) %>%
  mutate(p_tract_within_age = TOTAL / sum(TOTAL))

ggplot(total.univ.plot) +
  geom_sf(aes(fill = p_age_within_tract)) +
  scale_fill_viridis_c() +
  facet_wrap(~ age, nrow = 3) +
  theme(panel.background = element_blank())

# Neato! Not very interesting outputs though.

total.univ = total.univ %>%
  # Change age bin labels
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(total.univ)

# ==============================================================
# Table: individuals (in disability universe) with a disability yes/no

any.disb = pums %>%
  mutate(
    has.diff = (DIFFREM > 1) | (DIFFPHYS > 1) | (DIFFMOB > 1) | (DIFFCARE > 1) | (DIFFSENS > 1) | (DIFFEYE > 1) | (DIFFHEAR > 1)
  ) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age, has.diff) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = has.diff, names_prefix = 'diff', values_from = TOTAL)

head(any.disb)

### Visualize
any.disb.plot = merge(multco.tracts, any.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(any.disb.plot) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Pr. disabled') +
  facet_wrap(~ age, nrow = 3) +
  theme_void() +
  theme(legend.position = 'top')
# Wow... very high values for 85+ (probably not surprising)

ggplot(any.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')


any.disb = any.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(any.disb)

# ==============================================================
# Table: individuals (in universe) with hearing difficulty

### Aggregate for sum
hear.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age, diff.hear = DIFFHEAR > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(hear.disb)

### Visualize
hear.disb.plot = merge(multco.tracts, hear.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(hear.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Hearing\ndifficulty') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(hear.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

hear.disb = hear.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(hear.disb)


# ==============================================================
# Table: vision difficulty (in universe)

### Aggregate for sum
visn.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age, diff.hear = DIFFEYE > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(visn.disb)

### Visualize
visn.disb.plot = merge(multco.tracts, visn.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(visn.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Vision\ndifficulty') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')
# Interesting that there's more hearing difficulty downtown among 60-84 year
# olds
# Hot spot in NE near Woodland Park has a specialty hospital

ggplot(visn.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

visn.disb = visn.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(visn.disb)


# ==============================================================
# Table: cognitive difficulty (in universe)

### Aggregate for sum
cogn.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Remove individuals not in universe (age)
  filter(DIFFREM > 0) %>%
  group_by(tract, age, diff.hear = DIFFREM > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(cogn.disb)

### Visualize
cogn.disb.plot = merge(multco.tracts, cogn.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(cogn.disb.plot) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Cognitive\nndifficulty') +
  facet_wrap(~ age, nrow = 3) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(cogn.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Cognitive\nndifficulty') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')
# Also concentrated downtown for 55-60, 60-85 year olds
# (55-60 on the east side, 60-85 on the west?)

ggplot(cogn.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

cogn.disb = cogn.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(cogn.disb)


# ==============================================================
# Table: ambulatory difficulty (in universe)

### Aggregate for sum
ambu.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Remove individuals not in universe (age)
  filter(DIFFPHYS > 0) %>%
  group_by(tract, age, diff.hear = DIFFPHYS > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(ambu.disb)

### Visualize
ambu.disb.plot = merge(multco.tracts, ambu.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(ambu.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Ambulatory\nndifficulty') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(ambu.disb.plot %>% filter(as.numeric(age) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Ambulatory\ndifficulty') +
  facet_wrap(~ age, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(ambu.disb.plot %>% filter(as.numeric(age) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age) +
  theme_void() +
  theme(legend.position = 'top')

ambu.disb = ambu.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(ambu.disb)


# ==============================================================
# Table: self-care difficulty (in universe)

### Aggregate for sum
care.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Remove individuals not in universe (age)
  filter(DIFFCARE > 0) %>%
  group_by(tract, age, diff.hear = DIFFCARE > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(care.disb)

### Visualize
care.disb.plot = merge(multco.tracts, care.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(care.disb.plot %>% filter(as.numeric(age) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Self-care\ndifficulty') +
  facet_wrap(~ age, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(care.disb.plot %>% filter(as.numeric(age) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Self-care\nnifficulty') +
  facet_wrap(~ age, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(care.disb.plot) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age, nrow = 3) +
  theme_void() +
  theme(legend.position = 'right')

care.disb = care.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)

head(care.disb)


# ==============================================================
# Table: independent living difficulty (within universe)

### Aggregate for sum
indp.disb = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  # Remove individuals not in universe (age)
  filter(DIFFMOB > 0) %>%
  group_by(tract, age, diff.mob = DIFFMOB > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.mob, names_prefix = 'diff', values_from = TOTAL)

head(indp.disb)

### Visualize
indp.disb.plot = merge(multco.tracts, indp.disb) %>%
  group_by(tract, age) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(indp.disb.plot %>% filter(as.numeric(age) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Independent\nliving\ndifficulty') +
  facet_wrap(~ age, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(indp.disb.plot) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ age, nrow = 3) +
  theme_void() +
  theme(legend.position = 'right')

indp.disb = indp.disb %>%
  select(-diffFALSE) %>%
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = diffTRUE)
# NOTE: the ACS controls here don't include individuals under 18
# so these estimates likely should be ignored or excluded

head(indp.disb)
mean(indp.disb$A)
# lol okay this is a very small number anyway


# ==============================================================
# Table: Disabiltiy yes/no by REALD

reld.disb = pums %>%
  mutate(
    has.diff = (DIFFREM > 1) | (DIFFPHYS > 1) | (DIFFMOB > 1) | (DIFFCARE > 1) | (DIFFSENS > 1) | (DIFFEYE > 1) | (DIFFHEAR > 1)
  ) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, has.diff, reald = reald) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = has.diff, names_prefix = 'diff', values_from = TOTAL)

head(reld.disb)

### Visualize
reld.disb.plot = merge(multco.tracts, reld.disb) %>%
  group_by(tract, reald) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(reld.disb.plot) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Disability') +
  facet_wrap(~ reald, ncol = 2) +
  theme_void() +
  theme(legend.position = 'top')
  
ggplot(reld.disb.plot) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('Disability') +
  facet_wrap(~ reald, ncol = 2) +
  theme_void() +
  theme(legend.position = 'top')

reld.disb = reld.disb %>%
  select(-diffFALSE) %>%
  mutate(
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
  ) # %>%
  # pivot_wider(names_from = reald, values_from = diffTRUE)

  
# ==============================================================
# Table: Disabiltiy yes/no by REALD

reld.age.disb = pums %>%
  mutate(
    has.diff = (DIFFREM > 1) | (DIFFPHYS > 1) | (DIFFMOB > 1) | (DIFFCARE > 1) | (DIFFSENS > 1)
  ) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age, has.diff, reald) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = has.diff, names_prefix = 'diff', values_from = TOTAL, values_fill = 0)

head(reld.age.disb)

# Checking which groups are missing.
reld.age.disb %>%
  complete(tract, age, reald) %>%
  filter(is.na(diffTRUE))
# missing `other` individuals, ages 55-59 and 85+

### Visualize
# too much to visualize

reld.age.disb = reld.age.disb %>%
  select(-diffFALSE) %>% 
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
  ) # %>%
  # pivot_wider(names_from = c(reald, age), values_from = diffTRUE, values_fill = 0)

head(reld.age.disb)


# ==============================================================
# Aggregate tables

# TOT_CNI
# DIS_ANY
# DIS_HEA
# DIS_VIS
# DIS_COG
# DIS_AMB
# DIS_CAR
# DIS_IND

disb.tables = rbind(
  total.univ %>% mutate(table = 'TOTCNI'),
  any.disb   %>% mutate(table = 'DISANY'),
  hear.disb  %>% mutate(table = 'DISHEA'),
  visn.disb  %>% mutate(table = 'DISVIS'),
  cogn.disb  %>% mutate(table = 'DISCOG'),
  ambu.disb  %>% mutate(table = 'DISAMB'),
  care.disb  %>% mutate(table = 'DISCAR'),
  indp.disb  %>% mutate(table = 'DISIND')
) %>%
  pivot_wider(
    names_from = table, values_from = A:E, 
    values_fill = 0, names_glue = "{table}_L_{.value}"
    # "L" is for "all races"
  )

reld.disb = reld.disb %>% 
  pivot_wider(names_from = reald, values_from = diffTRUE, names_prefix = 'DISANY_') %>%
  # add "L" to end for "all ages"
  rename_with(~ paste0(., "_L"), -tract)
  
reld.age.disb = reld.age.disb %>%
  pivot_wider(
    names_from = c(reald, age), values_from = diffTRUE,
    names_prefix = 'DISANY_', values_fill = 0
  )

tables.out = merge(disb.tables, reld.disb) %>%
  merge(reld.age.disb)

apply(tables.out, 2, \(x) sum(is.na(x)))

write.csv(
  tables.out, row.names = FALSE,
  'multnomah/04_aggregate_weights/phaseII_out/disability_out.csv'
)
