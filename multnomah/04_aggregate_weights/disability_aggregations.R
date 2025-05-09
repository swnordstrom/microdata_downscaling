library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16))) %>%
  select(CBSERIAL, PERNUM, AGE, SEX, OCC, GQ, starts_with('DIFF'))

# Read in estimtes
weights = read.csv('multnomah/03_downscale_out/disability_all_weights_redone.csv')

# Read in reald for allocation
reald = read.csv('multnomah/01_raw_data/5acs23_orwa_reldpri.csv') %>%
  mutate(
    CBSERIAL = gsub('GQ', '01', serialno),
    CBSERIAL = gsub('HU', '00', CBSERIAL),
    CBSERIAL = as.numeric(CBSERIAL)
  ) %>%
  rename(PERNUM = sporder) %>%
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

# Get MultCo tract geographies for plots
multco.tracts = tracts('OR', 'Multnomah') %>%
  mutate(tract = as.numeric(TRACTCE))

# ==============================================================
# Merge and process data

# Adding in the disability variable here because it's slow and will be slower if
# we do it after the merge
pums = pums %>%
  mutate(across(starts_with('DIFF'), as.integer)) %>%
  pivot_longer(starts_with('DIFF'), names_to = 'diff', values_to = 'value') %>%
  group_by(CBSERIAL, PERNUM) %>%
  mutate(has.diff = any(value > 1)) %>%
  pivot_wider(names_from = diff, values_from = value)

# Combine with a merge
pums.weights = merge(
  pums, weights,
  by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum')
) %>%
  # Remove institutionalized and non-civilian population
  filter(!(OCC %in% 9800:9850) & !(GQ %in% 3)) %>%
  select(-c(OCC, GQ)) %>%
  merge(reald %>% select(CBSERIAL, PERNUM, reald.cat))

# Some records lost due to removing non-civilian individuals

nrow(pums.weights)

# Add in extra detail/variables
pums.weights = pums.weights %>%
  # Add binned age
  mutate(agebin = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE))

head(pums.weights)

# ==============================================================
# Table: total disability universe (all civilian non-institutionalized)

### Aggregate for sum
total.univ = pums.weights %>%
  group_by(tract, agebin) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.univ)

### Visualize
total.univ.plot = merge(multco.tracts, total.univ) %>%
  group_by(tract) %>%
  mutate(p_age_within_tract = TOTAL / sum(TOTAL)) %>%
  group_by(agebin) %>%
  mutate(p_tract_within_age = TOTAL / sum(TOTAL))

ggplot(total.univ.plot) +
  geom_sf(aes(fill = p_age_within_tract)) +
  scale_fill_viridis_c() +
  facet_wrap(~ agebin, nrow = 3) +
  theme(panel.background = element_blank())

# Neato! Not very interesting outputs though.

total.univ = total.univ %>%
  # Change age bin labels
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = TOTAL)


# ==============================================================
# Table: individuals (in disability universe) with a disability yes/no

any.disb = pums.weights %>%
  group_by(tract, agebin, has.diff) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = has.diff, names_prefix = 'diff', values_from = TOTAL)

head(any.disb)

### Visualize
any.disb.plot = merge(multco.tracts, any.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(any.disb.plot) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Pr. disabled') +
  facet_wrap(~ agebin, nrow = 3) +
  theme_void() +
  theme(legend.position = 'top')
# Wow... very high values for 85+ (probably not surprising)

ggplot(any.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')


any.disb = any.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(any.disb)

# ==============================================================
# Table: individuals (in universe) with hearing difficulty

### Aggregate for sum
hear.disb = pums.weights %>%
  group_by(tract, agebin, diff.hear = DIFFHEAR > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(hear.disb)

### Visualize
hear.disb.plot = merge(multco.tracts, hear.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(hear.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Hearing\ndifficulty') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(hear.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

hear.disb = hear.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(hear.disb)


# ==============================================================
# Table: vision difficulty (in universe)

### Aggregate for sum
visn.disb = pums.weights %>%
  group_by(tract, agebin, diff.hear = DIFFEYE > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(visn.disb)

### Visualize
visn.disb.plot = merge(multco.tracts, visn.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(visn.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Vision\ndifficulty') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')
# Interesting that there's more hearing difficulty downtown among 60-84 year
# olds
# Hot spot in NE near Woodland Park has a specialty hospital

ggplot(visn.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

visn.disb = visn.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(visn.disb)


# ==============================================================
# Table: cognitive difficulty (in universe)

### Aggregate for sum
cogn.disb = pums.weights %>%
  # Remove individuals not in universe (age)
  filter(DIFFREM > 0) %>%
  group_by(tract, agebin, diff.hear = DIFFREM > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(cogn.disb)

### Visualize
cogn.disb.plot = merge(multco.tracts, cogn.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(cogn.disb.plot) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Cognitive\nndifficulty') +
  facet_wrap(~ agebin, nrow = 3) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(cogn.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Cognitive\nndifficulty') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')
# Also concentrated downtown for 55-60, 60-85 year olds
# (55-60 on the east side, 60-85 on the west?)

ggplot(cogn.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

cogn.disb = cogn.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(cogn.disb)


# ==============================================================
# Table: ambulatory difficulty (in universe)

### Aggregate for sum
ambu.disb = pums.weights %>%
  # Remove individuals not in universe (age)
  filter(DIFFPHYS > 0) %>%
  group_by(tract, agebin, diff.hear = DIFFPHYS > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(ambu.disb)

### Visualize
ambu.disb.plot = merge(multco.tracts, ambu.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(ambu.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Ambulatory\nndifficulty') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(ambu.disb.plot %>% filter(as.numeric(agebin) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Ambulatory\ndifficulty') +
  facet_wrap(~ agebin, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(ambu.disb.plot %>% filter(as.numeric(agebin) > 1)) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin) +
  theme_void() +
  theme(legend.position = 'top')

ambu.disb = ambu.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(ambu.disb)


# ==============================================================
# Table: self-care difficulty (in universe)

### Aggregate for sum
care.disb = pums.weights %>%
  # Remove individuals not in universe (age)
  filter(DIFFCARE > 0) %>%
  group_by(tract, agebin, diff.hear = DIFFCARE > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.hear, names_prefix = 'diff', values_from = TOTAL)

head(care.disb)

### Visualize
care.disb.plot = merge(multco.tracts, care.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(care.disb.plot %>% filter(as.numeric(agebin) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Self-care\ndifficulty') +
  facet_wrap(~ agebin, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(care.disb.plot %>% filter(as.numeric(agebin) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Self-care\nnifficulty') +
  facet_wrap(~ agebin, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(care.disb.plot) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin, nrow = 3) +
  theme_void() +
  theme(legend.position = 'right')

care.disb = care.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)

head(care.disb)


# ==============================================================
# Table: independent living difficulty (within universe)

### Aggregate for sum
indp.disb = pums.weights %>%
  # Remove individuals not in universe (age)
  filter(DIFFMOB > 0) %>%
  group_by(tract, agebin, diff.mob = DIFFMOB > 1) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = diff.mob, names_prefix = 'diff', values_from = TOTAL)

head(indp.disb)

### Visualize
indp.disb.plot = merge(multco.tracts, indp.disb) %>%
  group_by(tract, agebin) %>%
  mutate(p.diff = diffTRUE / (diffTRUE + diffFALSE)) %>%
  ungroup()

ggplot(indp.disb.plot %>% filter(as.numeric(agebin) > 3)) +
  geom_sf(aes(fill = p.diff)) +
  scale_fill_viridis_c('Independent\nliving\ndifficulty') +
  facet_wrap(~ agebin, nrow = 2) +
  theme_void() +
  theme(legend.position = 'top')

ggplot(indp.disb.plot) +
  geom_sf(aes(fill = diffTRUE)) +
  scale_fill_viridis_c('people') +
  facet_wrap(~ agebin, nrow = 3) +
  theme_void() +
  theme(legend.position = 'right')

indp.disb = indp.disb %>%
  select(-diffFALSE) %>%
  mutate(agebin = LETTERS[as.numeric(agebin)]) %>%
  pivot_wider(names_from = agebin, values_from = diffTRUE)
# NOTE: the ACS controls here don't include individuals under 18
# so these estimates likely should be ignored or excluded

head(indp.disb)
mean(indp.disb$A)
# lol okay this is a very small number anyway


# ==============================================================
# Table: Disabiltiy yes/no by REALD

# Okay... first thing I want to do is look for missing REALD-disability-PUMA combos...
# (especially by age group... woof)
pums.weights %>%
  filter(has.diff) %>%
  separate_wider_delim(tract, names = c('puma', 'tract'), delim = '_') %>%
  distinct(CBSERIAL, PERNUM, puma, reald.cat) %>%
  count(reald = reald.cat, puma) %>%
  pivot_wider(names_from = puma, values_from = n, names_prefix = 'p') %>%
  print(n = nrow(.))
# Okay... missing a disabled person with 'Other' reald in puma 5105

# But also, will want to look at the reld-age combos by group as well...

pums.weights %>%
  filter(has.diff) %>%
  separate_wider_delim(tract, names = c('puma', 'tract'), delim = '_') %>%
  distinct(CBSERIAL, PERNUM, puma, reald.cat, agebin) %>%
  count(reald = reald.cat, agebin, puma) %>%
  complete(reald, agebin, puma) %>%
  filter(is.na(n)) %>%
  print(n = nrow(.))
# Unsurprisingly, many of these as well...

pums.weights %>%
  filter(has.diff) %>%
  separate_wider_delim(tract, names = c('puma', 'tract'), delim = '_') %>%
  distinct(CBSERIAL, PERNUM, puma, reldpri, agebin) %>%
  count(reald = reldpri, agebin, puma) %>%
  complete(reald, agebin, puma) %>%
  filter(is.na(n))
# over 600 of these hnghh
# Are there any age groups that are just completely missing

pums.weights %>%
  filter(has.diff) %>%
  separate_wider_delim(tract, names = c('puma', 'tract'), delim = '_') %>%
  distinct(CBSERIAL, PERNUM, puma, reald.cat, agebin) %>%
  count(reald = reald.cat, agebin) %>%
  complete(reald, agebin) %>%
  filter(is.na(n))
# Okay, there are zero disabled "other" individuals in three age bins across the dataset

