library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00062.xml') %>%
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
weights = read.csv('multnomah/03_downscale_out/languages_all_weights.csv')

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
  # in language universe (older than age 5)
  mutate(
    # in universe
    in.lang.univ = AGE > 4,
    language.orig = case_match(
      # see lang12 in langx
      LANGUAGE,
      1 ~ 'speak.only.english',
      12 ~ 'spanish',
      57 ~ 'arabic',
      43 ~ 'chinese',
      11 ~ 'french.haitian.cajun',
      2:4 ~ 'german.west.germanic',
      49 ~ 'korean',
      18:26 ~ 'russian.polish.slavic',
      54 ~ 'tagalog',
      50 ~ 'vietnamese',
      setdiff(c(2:10, 13:32), c(1:4, 18:26)) ~ 'other.indo-euro',
      setdiff(40:56, c(43, 49:50, 54)) ~ 'other.asian',
      .default = 'other.and.unspec'
    ),
    # ah too lazy to fix this at the root so I'll correct it here
    # (.default above sets language to other for those not in universe
    #  probably doesn't matter but that's fine)
    language.orig = ifelse(in.lang.univ, language.orig, NA),
    language = case_match(
      language.orig,
      'arabic' ~ 'ARB',
      'chinese' ~ 'CHN',
      'french.haitian.cajun' ~ 'FRN',
      'german.west.germanic' ~ 'GRM',
      'korean' ~ 'KRN',
      'russian.polish.slavic' ~ 'RUS',
      'spanish' ~ 'SPN',
      'tagalog' ~ 'TGL',
      'vietnamese' ~ 'VTN',
      'speak.only.english' ~ 'ENG',
      c('other.asian', 'other.indo-euro', 'other.and.unspec') ~ 'OTH'
    ),
    proficiency = case_when(
      !(LANGUAGE %in% 1) & SPEAKENG %in% 2:4 ~ 'eng.very.well',
      SPEAKENG %in% c(1, 5:6) ~ 'less.than.very.well',
      .default = NA
    ),
    is.immigrant = (CITIZEN > 1)
  ) %>%
  merge(reald %>% select(CBSERIAL, PERNUM, reald = reald.cat))

pums.synthetic.processed = merge(
  synthetic.pums %>% select(CBSERIAL, PERNUM, PUMA),
  pums.processed %>% select(-PUMA)
) %>%
  select(names(pums.processed)) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1)

pums = rbind(pums.processed, pums.synthetic.processed) %>%
  select(-c(starts_with('RAC'), starts_with('ANCESTR'))) %>%
  mutate(age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE))


# ==============================================================
# Table 0: total number of people for each age-race group

### Aggregate for sum
total.reald = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.reald)

### Visualize
total.reald.plot = merge(multco.tracts, total.reald)

ggplot(total.reald.plot) +
  geom_sf(aes(fill = log(TOTAL, base = 10))) +
  scale_fill_viridis_c(breaks = (-1:3), labels = c(0.1, 1, 10, 100, 1000)) +
  facet_grid(reald ~ age) +
  theme(panel.background = element_blank())
# color scale too compressed to be useful here

total.reald = total.reald %>%
  # Change age bin labels
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

head(total.reald)


# ==============================================================
# Table 1: total number of people for each age-race group

### Aggregate for sum
total.sex = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, sex = as_factor(SEX), age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.sex)

### Visualize
total.sex.plot = merge(multco.tracts, total.sex %>% pivot_wider(names_from = sex, values_from = TOTAL))

ggplot(total.sex.plot) +
  geom_sf(aes(fill = Female / (Male + Female))) +
  scale_fill_gradient2(low = 'dodgerblue', high = 'pink', mid = 'white', midpoint = 0.5) +
  facet_wrap( ~ age, nrow = 3) +
  theme(panel.background = element_blank())

total.sex = total.sex %>%
  # Change age bin labels
  mutate(
    age = LETTERS[as.numeric(age)],
    sex = paste0('SEX', gsub('^([MF]).+', '\\1', sex))
  ) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(total.sex)


# ==============================================================
# Table 2: total number of people in language universe

### Aggregate for sum
total.univ = pums %>%
  filter(in.lang.univ) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.univ)

### Visualize
total.univ.plot = merge(multco.tracts, total.univ)

ggplot(total.univ.plot) +
  geom_sf(aes(fill = log(TOTAL, base = 10))) +
  scale_fill_viridis_c(breaks = (-1:3), labels = c(0.1, 1, 10, 100, 1000)) +
  facet_wrap( ~ age, nrow = 3) +
  theme(panel.background = element_blank())
# color scale too compressed to be useful here

total.univ = total.univ %>%
  # Change age bin labels
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL)

head(total.univ)


# ==============================================================
# Table 3 total number of people by age-language

### Aggregate for sum
total.lang = pums %>%
  filter(in.lang.univ) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, language, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(total.lang)

### Visualize
total.lang.plot = merge(multco.tracts, total.lang) %>%
  mutate(total.cutoff = ifelse(TOTAL < 0.01, 0.01, TOTAL))

ggplot(total.lang.plot) +
  geom_sf(aes(fill = log(total.cutoff, base = 10))) +
  scale_fill_viridis_c(breaks = (-1:3), labels = c(0.1, 1, 10, 100, 1000)) +
  facet_grid(language ~ age) +
  theme(panel.background = element_blank())


total.lang = total.lang %>%
  # Change age bin labels
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# NOTE: imputed zeros here

head(total.lang)


# ==============================================================
# Table 4 total number of people with limited proficiency by age-language 


### Aggregate for sum
lep.lang = pums %>%
  filter(proficiency %in% 'less.than.very.well') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, language, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

head(lep.lang)

### Visualize
lep.lang.plot = merge(multco.tracts, lep.lang) %>%
  mutate(total.cutoff = ifelse(TOTAL < 0.01, 0.01, TOTAL))

ggplot(lep.lang.plot) +
  geom_sf(aes(fill = log(total.cutoff, base = 10))) +
  scale_fill_viridis_c(breaks = (-1:3), labels = c(0.1, 1, 10, 100, 1000)) +
  facet_grid(language ~ age) +
  theme(panel.background = element_blank())
# some missing but that's fine

lep.lang = lep.lang %>%
  # Change age bin labels
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = TOTAL, values_fill = 0)
# NOTE: imputed zeros here

head(lep.lang)


# ==============================================================
# Table 5 total number of immigrants

immig.total = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, is.immigrant, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract))) %>%
  pivot_wider(names_from = is.immigrant, values_from = TOTAL, names_prefix = 'imm')

head(immig.total)

### Visualize
immig.total.plot = merge(multco.tracts, immig.total)

ggplot(immig.total.plot) +
  geom_sf(aes(fill = immTRUE / (immTRUE + immFALSE))) +
  scale_fill_viridis_c() +
  facet_wrap( ~ age, nrow = 3) +
  theme(
    panel.background = element_blank(),
    legend.position = 'none'
  )

# Pivot to long format
immig.total = immig.total %>%
  select(-immFALSE) %>%
  # Change age bin labels
  mutate(age = LETTERS[as.numeric(age)]) %>%
  pivot_wider(names_from = age, values_from = immTRUE, values_fill = 0)


# ==============================================================
# Combineth!


# Merge pivoted tables:
table.out = merge(
  # Reald totals
  x = total.reald %>% pivot_wider(names_from = reald, values_from = A:E, names_glue = 'TOTAL_{reald}_{.value}'),
  # Language universe totals
  y = total.univ  %>% rename_with(.cols = -tract, .fn = ~ paste0('LNGTOT_L_', .))
) %>%
  merge(total.sex %>% pivot_wider(names_from = sex, values_from = A:E, names_glue = '{sex}_L_{.value}')) %>%
  merge(
    rbind(
      total.lang %>% mutate(table = ''),
      lep.lang   %>% mutate(table = 'LEP')
    ) %>%
      pivot_wider(names_from = c(language, table), values_from = A:E, names_glue = 'LNG{language}{table}_L_{.value}')
  ) %>%
  merge(immig.total %>% rename_with(.cols = -tract, .fn = ~ paste0('IMM_L_', .)))

apply(table.out, 2, \(x) sum(is.na(x)))
# no NAs, that's good

data.frame(col = names(table.out)[-1]) %>%
  separate_wider_delim(col, delim = "_", names = c('table', 'race', 'age')) %>%
  sapply(unique)
# neato  !

write.csv(
  table.out, row.names = FALSE,
  'multnomah/04_aggregate_weights/phaseII_out/languages_out.csv'
)
