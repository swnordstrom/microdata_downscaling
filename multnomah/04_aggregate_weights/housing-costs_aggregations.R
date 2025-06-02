library(ggplot2)
library(ipumsr)
library(tigris)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in data and estimates

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro('multnomah/01_raw_data/usa_00057.xml')

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
multco.tracts = tracts('OR', 'Multnomah') %>% mutate(tract = as.numeric(TRACTCE))

# Get synthetic records (PUMS and dummy)
synthetic.pums = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_vet_hou_synthetic_records.csv')


# ==============================================================
# Neaten and combine

# Add and modify columns
pums.processed = pums.raw %>% 
  mutate(
    # housing tenure
    tenure = case_match(
      OWNERSHP,
      0 ~ 'NA',
      1 ~ 'owner',
      2 ~ 'renter'
    ),
    # estimate housing costs / income - need to do this separately for renters and owners
    own.pct.num = ifelse(
      # If respondent is homeowner with positive income and housing cost
      (OWNERSHP %in% 1 & HHINCOME > 0 & OWNCOST > 0),
      # get housing cost as a percentage of annual income
      100 * ((12 * OWNCOST) / HHINCOME),
      # otherwise give an NA
      NA
    ),
    ren.pct.num = ifelse(
      # If the respondent is a renter with positive income and rent cost,
      OWNERSHP %in% 2 & HHINCOME > 0 & RENTGRS > 0,
      # get rent as a percentage of annual income
      100 * ((12 * RENTGRS) / HHINCOME),
      # otherwise give NA
      NA
    ),
    # Get binned household or renter cost percentages
    own.pct = cut(own.pct.num, breaks = c(0, 30, 100), right = FALSE, labels = c('under30', 'over30')),
    # Get 'not computed' housing costs for owners
    own.pct = ifelse(
      OWNERSHP %in% 1 & !(HHINCOME > 0 & OWNCOST > 0),
      'NC',
      as.character(own.pct)
    ),
    ren.pct = cut(ren.pct.num, breaks = c(0, 30, 100), right = FALSE, labels = c('under30', 'over30')),
    # Get 'not computed' housing costs for renters
    ren.pct = ifelse(
      OWNERSHP %in% 2 & !(HHINCOME > 0 & RENTGRS > 0),
      'NC',
      as.character(ren.pct)
    ),
    # flag for group quarters
    in.gq = GQ %in% (3:4)
  ) %>%
  merge(reald %>% select(CBSERIAL, PERNUM, reald))

nrow(pums.processed)

pums.synthetic.processed = merge(
  synthetic.pums %>% select(CBSERIAL, PERNUM, PUMA),
  pums.processed %>% select(-PUMA)
) %>%
  select(names(pums.processed)) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1)

# Combine PUMS and do other processing steps
pums = rbind(pums.processed, pums.synthetic.processed) %>%
  # Add binned age
  mutate(age = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE))


# ==============================================================
# Table 1: total reald

reald.total.table = pums %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.total.table.plot = merge(multco.tracts, reald.total.table)

ggplot(reald.total.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

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
# Table 2: total institutionalized by race/ethnicity

reald.inst.table = pums %>%
  filter(GQ %in% 3) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.inst.table.plot = merge(multco.tracts, reald.inst.table)

ggplot(reald.inst.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )
# oof... there just aren't very many records to spread around...

# reald.inst.table %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(age, reald) %>%
#   print(n = nrow(.))
# # Okay it looks to me like:
# # - Under 18 and 55-59 are probably too small to matter...
# # - 85+: might be too small (see: asians) but might not be

reald.inst.table = reald.inst.table %>%
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
# NOTE: imputed zeros

head(reald.inst.table)


# ==============================================================
# Table 3: total living "at home" (in a household?) by race/ethnicity

reald.home.table = pums %>%
  filter(tenure %in% c('renter', 'owner')) %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.home.table.plot = merge(multco.tracts, reald.home.table)

ggplot(reald.home.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

reald.home.table = reald.home.table %>%
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
# NOTE: imputed zeros

head(reald.home.table)


# ==============================================================
# Table 4: renters by race/ethnicity

reald.ren.table = pums %>%
  filter(tenure %in% 'renter') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.ren.table.plot = merge(multco.tracts, reald.ren.table)

ggplot(reald.ren.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

# reald.ren.table %>%
#   filter(grepl('85', age)) %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(reald, age) %>%
#   print(n = nrow(.))
# # Okay, not sure how we don't have any Hispanic renters above 85 in the whole
# # PUMS
# # That seems wrong...

reald.ren.table = reald.ren.table %>%
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
# NOTE: imputed zeros

head(reald.ren.table)


# ==============================================================
# Table 4: homeowners by race/ethnicity

reald.own.table = pums %>%
  filter(tenure %in% 'owner') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.own.table.plot = merge(multco.tracts, reald.own.table)

ggplot(reald.own.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

# reald.ren.table %>%
#   filter(grepl('85', age)) %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(reald, age) %>%
#   print(n = nrow(.))
# # Okay, not sure how we don't have any Hispanic renters above 85 in the whole
# # PUMS
# # That seems wrong...

reald.own.table = reald.own.table %>%
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
# NOTE: imputed zeros

head(reald.own.table)


# ==============================================================
# Table 5: renters with >30% of income dedicated
# to housing, by race/ethnicity

reald.r30.table = pums %>%
  filter(ren.pct %in% 'over30') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.r30.table.plot = merge(multco.tracts, reald.r30.table)

ggplot(reald.r30.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

# reald.r30.table %>%
#   mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
#   count(age, reald, total) %>%
#   pivot_wider(names_from = total, values_from = n) %>%
#   arrange(reald, age) %>%
#   print(n = nrow(.))
# # Yeah the Hispanic/Latino absence above age 85 is pretty noticeable here
# # 


reald.r30.table = reald.r30.table %>%
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
# NOTE: imputed zeros

head(reald.r30.table)


# ==============================================================
# Table 5: owners with >30% of income dedicated
# to housing, by race/ethnicity

reald.o30.table = pums %>%
  filter(own.pct %in% 'over30') %>%
  merge(weights, by.x = c('CBSERIAL', 'PERNUM'), by.y = c('cbserial', 'pernum'), all.x = TRUE) %>%
  group_by(tract, reald, age) %>%
  summarise(TOTAL = sum(alloc)) %>%
  ungroup() %>%
  mutate(tract = as.numeric(gsub('^\\d{4}\\_', '', tract)))

### For plotting:
reald.o30.table.plot = merge(multco.tracts, reald.o30.table)

ggplot(reald.o30.table.plot) +
  geom_sf(aes(fill = TOTAL)) +
  scale_fill_viridis_c() +
  facet_grid(reald ~ age) +
  theme(
    panel.background = element_blank(),
    axis.text = element_blank()
  )

reald.o30.table %>%
  mutate(total = cut(TOTAL, breaks = c(0, 10^(-1:4)), labels = c(-Inf, -1:3))) %>%
  count(age, reald, total) %>%
  pivot_wider(names_from = total, values_from = n) %>%
  arrange(reald, age) %>%
  print(n = nrow(.))
# Definitely worried about some of these cases (American Indians, NHPI over 85)


reald.o30.table = reald.o30.table %>%
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
# NOTE: imputed zeros

head(reald.o30.table)


# ==============================================================
# Collate all tables together


# [1] "reald.home.table"  "reald.inst.table"  "reald.o30.table"   
#     "reald.own.table"   "reald.r30.table"   "reald.ren.table"  
# [7] "reald.total.table"

table.out = rbind(
  reald.total.table %>% mutate(table = 'TOTAL2'),
  reald.inst.table  %>% mutate(table = 'INS'),
  reald.home.table  %>% mutate(table = 'HOM'),
  reald.ren.table   %>% mutate(table = 'REN'),
  reald.own.table   %>% mutate(table = 'OWN'),
  reald.r30.table   %>% mutate(table = 'REN30'),
  reald.o30.table   %>% mutate(table = 'OWN30')
) %>%
  pivot_wider(names_from = c(table, reald), values_from = A:E, names_glue = '{table}_{reald}_{.value}')

dim(table.out)

table.out %>% apply(1, \(x) sum(is.na(x))) %>% table()

write.csv(
  table.out, row.names = FALSE,
  'multnomah/04_aggregate_weights/phaseII_out/housing-cost_out.csv'
)
