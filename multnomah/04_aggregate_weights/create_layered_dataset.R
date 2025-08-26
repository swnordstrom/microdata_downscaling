library(ipumsr)
library(dplyr)
library(tidyr)

rm(list = ls())

write.bool = FALSE

# ==============================================================
# Read in raw PUMS

# Get PUMS datasets
pums.raw.language.lep = read_ipums_micro('multnomah/01_raw_data/usa_00062.xml')
pums.raw.poverty.stat = read_ipums_micro('multnomah/01_raw_data/usa_00053.xml')
pums.raw.disabilities = read_ipums_micro('multnomah/01_raw_data/usa_00051.xml')
pums.raw.vets.housing = read_ipums_micro('multnomah/01_raw_data/usa_00064.xml')

# Read in codebook for ACS tables
acs.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0044_csv.zip', file_select = 1) %>%
  filter(grepl('[Mm]ultno', COUNTY))
acs.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0044_csv.zip', file_select = 2) %>%
  filter(grepl('[Mm]ultno', COUNTY))


# ==============================================================
# Subset to relevant variables and combine

pums.language.lep = pums.raw.language.lep %>%
  select(
    CBSERIAL, PERNUM, PERWT, AGE, SEX,
    RACWHT, RACBLK, RACASIAN, RACAMIND, RACPACIS, RACOTHER, HISPAN,
    LANGUAGE, SPEAKENG, CITIZEN
  )

pums.poverty.stat = pums.raw.poverty.stat %>%
  select(CBSERIAL, PERNUM, PERWT, AGE, SEX, POVERTY)

pums.disabilities = pums.raw.disabilities %>%
  select(CBSERIAL, PERNUM, PERWT, AGE, SEX, OCC, GQ, starts_with('DIFF'))

pums.vets.housing = pums.raw.vets.housing %>%
  select(
    CBSERIAL, PERNUM, PERWT, AGE, SEX, OCC, GQ, starts_with('DIFF'), VETSTAT, 
    MARST, OWNERSHP, HHINCOME, OWNCOST, RENTGRS, RELATE, GCHOUSE, GCRESPON
  )

pums = merge(pums.language.lep, pums.poverty.stat) %>%
  merge(pums.disabilities) %>%
  merge(pums.vets.housing)

nrow(pums)
# Great

# ==============================================================
# Get case-use variables

pums.classified = pums %>%
  rename_with(.cols = c(CBSERIAL, PERNUM, PERWT), .fn = tolower) %>%
  mutate(
    # sex
    sex = tolower(as.character(as_factor(SEX))),
    # get race groupings
    race.white = ifelse(RACWHT > 1, 'is.white', 'not.white'),
    race.black = ifelse(RACBLK > 1, 'is.black', 'not.black'),
    race.asian = ifelse(RACASIAN > 1, 'is.asian', 'not.asian'),
    race.amind = ifelse(RACAMIND > 1, 'is.indigenous', 'not.indigenous'),
    race.pacis = ifelse(RACPACIS > 1, 'is.pacific.islander', 'not.pacific.islander'),
    race.other = ifelse(RACOTHER > 1, 'is.other.race', 'not.other.race'),
    hispanic   = ifelse(HISPAN > 0, 'is.hispanic', 'not.hispanic'),
    # Get age bins
    age = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE, labels = c('0-17', '18-54', '55-59', '60-84', '85+')),
    # Get poverty bins
    poverty.level = cut(POVERTY, breaks = c(0, 1, 100, 250, 400, Inf), right = FALSE, labels = c('poverty.not.recorded', '<100%', '101-250%', '251-400%', '401%+')),
    # Get languages
    language = case_match(
      # see lang12 in langx
      LANGUAGE,
      0 ~ 'no.language.recorded.too.young',
      1 ~ 'speak.only.english',
      12 ~ 'spanish',
      57 ~ 'arabic',
      43 ~ 'chinese',
      11 ~ 'french.including.haitian',
      2:4 ~ 'german.including.dutch',
      49 ~ 'korean',
      18:26 ~ 'russian.and.other.slavic',
      54 ~ 'tagalog',
      50 ~ 'vietnamese',
      .default = 'other'
    ),
    # English proficiency
    english.proficiency = case_when(
      !(LANGUAGE %in% 1) & SPEAKENG %in% 2:4 ~ 'speak.english.very.well',
      SPEAKENG %in% c(1, 5:6) ~ 'speak.english.less.than.very.well',
      LANGUAGE %in% 1 ~ 'not.applicable.english.first.language',
      AGE < 5 ~ 'not.applicable.too.young', 
      .default = NA
    ),
    # Immigrant yes/no
    immigrant = ifelse(CITIZEN %in% 2:5, 'is.immigrant', 'not.immigrant'),
    # Disability yes/no
    any.disability = case_when(
      (OCC %in% 9800:9850) | (GQ %in% 3) ~ 'not.in.disability.universe',
      (DIFFSENS > 1 | DIFFREM > 1 | DIFFPHYS > 1 | DIFFMOB > 1 | DIFFCARE > 1) ~ 'has.a.disability',
      .default = 'no.disability'
    ),
    # Sight difficulty
    visn.diff = case_match(DIFFEYE, 0 ~ 'vision.difficulty.not.reported', 1 ~ 'no.vision.difficulty', 2 ~ 'has.vision.difficulty'),
    visn.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'vision.difficulty.not.reported', visn.diff),
    # Hearing difficulty
    hear.diff = case_match(DIFFHEAR, 0 ~ 'hearing.difficulty.not.reported', 1 ~ 'no.hearing.difficulty', 2 ~ 'has.hearing.difficulty'),
    hear.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'hearing.difficulty.not.reported', hear.diff),
    # Cognitive difficulty (difficulty remembering)
    cogn.diff = case_match(DIFFREM, 0 ~ 'cognitive.difficulty.not.reported', 1 ~ 'no.cognitive.difficulty' , 2 ~ 'has.cognitive.difficulty'),
    cogn.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'cognitive.difficulty.not.reported', cogn.diff),
    # Ambulatory difficulty (physical difficulty)
    ambu.diff = case_match(DIFFPHYS, 0 ~ 'ambulatory.difficulty.not.reported', 1 ~  'no.ambulatory.difficulty', 2 ~ 'has.ambulatory.difficulty'),
    ambu.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'ambulatory.difficulty.not.reported', ambu.diff),
    # Self-care difficulty
    care.diff = case_match(DIFFCARE, 0 ~ 'selfcare.difficulty.not.reported', 1 ~ 'no.selfcare.difficulty' , 2 ~'has.selfcare.difficulty'),
    care.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'selfcare.difficulty.not.reported', care.diff),
    # Independent living difficulty (difficulty with mobility)
    indp.diff = case_match(DIFFMOB, 0 ~ 'independent.living.difficulty.not.reported', 1 ~ 'no.independent.living.difficulty', 2 ~ 'has.independent.living.difficulty'),
    indp.diff = ifelse(any.disability %in% 'not.in.disability.universe', 'independent.living.difficulty.not.reported', indp.diff),
    # Veteran status
    veteran = case_match(VETSTAT, 0:1 ~ 'nonveteran', 2 ~ 'veteran'),
    # Marital status
    marital.status = case_match(
      MARST,
      1:3 ~ 'now.married',
      4:5 ~ 'divorced.or.widowed',
      6 ~ 'never.married'
    ),
    # Housing tenure
    tenure = case_match(OWNERSHP, 0 ~ 'not.in.household', 1 ~ 'in.owned.household', 2 ~ 'in.rented.household'),
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
    # own.pct = cut(own.pct.num, breaks = c(0, 30, 100), right = FALSE, labels = c('not.cost.burdened', 'cost.burdened')),
    # Get 'not computed' housing costs for owners
    own.burden = case_when(
      !(OWNERSHP %in% 1) ~ 'not.in.owned.household',
      OWNERSHP %in% 1 & !(HHINCOME > 0 & OWNCOST > 0) ~ 'cost.not.computed',
      OWNERSHP %in% 1 & own.pct.num < 30 ~ 'not.cost.burdened',
      OWNERSHP %in% 1 & own.pct.num >= 30 ~ 'cost.burdened',
      .default = NA
    ),
    # ren.pct = cut(ren.pct.num, breaks = c(0, 30, 100), right = FALSE, labels = c('not.cost.burdened', 'cost.burdened')),
    # Get 'not computed' housing costs for renters
    ren.burden = case_when(
      !(OWNERSHP %in% 2) ~ 'not.in.rented.household',
      OWNERSHP %in% 2 & !(HHINCOME > 0 & RENTGRS > 0) ~ 'cost.not.computed',
      OWNERSHP %in% 2 & ren.pct.num < 30 ~ 'not.cost.burdened',
      OWNERSHP %in% 2 & ren.pct.num >= 30 ~ 'cost.burdened',
      .default = NA
    ),
    # flag for group quarters
    in.group.quarters = ifelse(GQ %in% (3:4), 'in.group.quarters', 'not.in.group.quarters'),
    # flag for living in institution
    in.institution = ifelse(GQ %in% 3, 'in.institution', 'not.in.institution'),
    # flag for grandparent raising grandchildren
    grandparents.raising = ifelse(GCRESPON > 1, 'grandparent.raising.grandchild', 'not.grandparent.raising.grandchild')
  ) %>%
  # Get some household details
  group_by(cbserial) %>%
  mutate(
    # Living alone
    living.alone = ifelse(n() < 2, 'living.alone', 'not.living.alone'),
    # Living in a family household
    family.household = case_when(
      GQ %in% 3:4 ~ 'not.in.a.household',
      any(RELATE[pernum > 1] <= 10) ~ 'in.family.household',
      .default = 'not.in.family.household'
    ),
    # Living in a multigenerational household
    multigen.household = ifelse(
      (any(GCHOUSE > 1) | (any(RELATE %in% 9)) | (any(RELATE %in% 3:4) & any(RELATE %in% 5:6))),
      'multigenerational',
      'not.multigenerational'
    )
  ) %>%
  ungroup() %>%
  select(-matches('\\.pct\\.num$')) %>%
  select(-matches('[A-Z]', ignore.case = FALSE))

nrow(pums.classified)
# Should be same number of rows as PUMS

pums.grouped = pums.classified %>%
  group_by(pick(sex:last_col())) %>%
  summarise(
    n.people = sum(perwt),
    n.record = n()
  ) %>%
  ungroup()

pums.grouped
# Neat, cool, etc.

nrow(pums.grouped)

# Looking at NAs
apply(pums.grouped, 2, \(x) sum(is.na(x)))
# neat!

sapply(pums.grouped, table, useNA = 'ifany')

# some things to fix up
# - disability universe exclusions
# - race?

pums.grouped = pums.grouped %>%
  select(
    # Age, sex, race/ethnicity
    age, sex, race.white, race.black, race.asian, race.pacis, race.amind, race.other, hispanic,
    # Estiamte group 1
    language, english.proficiency, immigrant, 
    # Estimate group 2
    any.disability, visn.diff, hear.diff, cogn.diff, ambu.diff, care.diff, indp.diff,
    # Estimate group 3
    poverty.level,
    # Estimate group 4
    veteran, marital.status, family.household, multigen.household, living.alone,
    grandparents.raising, in.group.quarters, in.institution, tenure, own.burden, ren.burden,
    # Counts
    n.people, n.record
  ) %>%
  rename_with(.fn = ~ gsub('\\.', '_', .))

pums.grouped

if (write.bool) {
  write.csv(
    pums.grouped, row.names = FALSE,
    'multnomah/04_aggregate_weights/phaseII_out/layered_dataset_2025-06-26.csv'
  )
}

# ==============================================================
# Compare results to the ACS tabular estimates for the county

acs.combined = merge(acs.raw1, acs.raw2) %>%
  select(STATE, COUNTY, matches('[A-Z0-9]{4}[EM]\\d{3}')) %>% 
  pivot_longer(-c(STATE, COUNTY), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    rbind(
      ipums_var_info(acs.raw1, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(acs.raw2, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
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
  separate_wider_delim(var_desc, delim = ' (universe: ', names = c('var_desc', 'univ'))

head(acs.combined)

# hmm... honestly, rather than automating this, I could just go through manually case-by-case
# I'll do that


##### Language #####

merge(
  pums.grouped %>%
    group_by(language) %>%
    summarise(n = sum(n_people)) %>%
    mutate(language = gsub('\\..+', '', language)),
  y = acs.combined %>% 
    filter(form %in% 'AS6E', !grepl('\\:', var_label)) %>%
    select(var_label, E, M) %>%
    mutate(var_label = gsub('\\,?\\s.+', '', var_label)),
  by.x = 'language', by.y = 'var_label', all = TRUE
) %>%
  mutate(e.lo = E - M, e.hi = E + M) %>%
  select(language, E, M, e.lo, n, e.hi)

7468+8502+13908

pums.grouped %>%
  filter(!grepl('too\\.young', language)) %>%
  summarise(n = sum(n_people))
acs.combined %>% filter(form %in% 'AS6E', var_label %in% 'total')
# off by ~1000... hmm... oh well

##### Limited proficiency #####

merge(
  pums.grouped %>%
    filter(grepl('less\\.than', english_proficiency)) %>%
    group_by(language) %>%
    summarise(n = sum(n_people)) %>%
    mutate(language = gsub('\\..+', '', language)),
  acs.combined %>% 
    filter(form %in% 'AS6E', grepl('less\\sthan', var_label)) %>%
    select(var_label, E, M) %>%
    mutate(var_label = gsub('[\\,\\:]?\\s.+', '', var_label)),
  by.x = 'language', by.y = 'var_label', all = TRUE
) %>%
  mutate(in.mar = E - n < M)
# nice

##### Immigrant #####

acs.combined %>% 
  filter(form %in% 'ASZX', (grepl('not\\s', var_label) | grepl('natural', var_label)))

58371 + 46467 # ~104K

pums.grouped %>% group_by(immigrant) %>% summarise(n = sum(n_people))
# 108K
# I think it's close enough

##### Disabilities #####

# ah right, this is broken down by age
acs.combined %>%
  filter(grepl('diff', var_label), grepl('with', var_label)) %>%
  group_by(var_desc) %>%
  summarise(E = sum(E)) %>%
  mutate(
    var_desc = gsub('sex\\sby\\sage\\sby\\s', '', var_desc),
    var_desc = gsub('\\sdifficulty', '', var_desc),
    var_desc = gsub('[\\-]', '', var_desc)
  )

diff.compare = merge(
  acs.combined %>%
    filter(grepl('diff', var_label)) %>%
    mutate(var_label = gsub('.+\\:\\s', '', var_label)) %>%
    group_by(var_label, var_desc) %>%
    summarise(E = sum(E), approxm = round(sqrt(sum(M^2)))) %>%
    mutate(
      var_desc = gsub('sex\\sby\\sage\\sby\\s', '', var_desc),
      var_desc = gsub('\\sdifficulty', '', var_desc),
      var_desc = gsub('self\\-', '', var_desc),
      var_desc = gsub('(\\D{2}).+', '\\1', var_desc),
      var_label = ifelse(grepl('with', var_label), 'with', 'without')
    ),
  pums.grouped %>%
    select(ends_with('diff'), n_people) %>%
    pivot_longer(-n_people, names_to = 'diff', values_to = 'status') %>%
    group_by(diff, status) %>%
    summarise(n = sum(n_people)) %>%
    mutate(
      status = gsub('has\\..+', 'with', status),
      status = gsub('no\\..+', 'without', status),
      status = gsub('.+\\.not\\.reported', 'not.reported', status),
      diff = gsub('(\\D{2}).+', '\\1', diff)
    ),
  by.x = c('var_desc', 'var_label'), by.y = c('diff', 'status'), all = TRUE
)

diff.compare %>% mutate(in.mar = abs(E - n) < approxm)
# sweet, much better
# one exception: still over-estimating the number of people with an independent living disability
# I think that's the one where there's an ACS discrepancy though
# hmm... it might be ideal to fix that in the table
# oh well

rm(diff.compare)


##### Poverty ##### 

pums.grouped %>% group_by(poverty_level) %>% 
  summarise(n = sum(n_people)) %>% 
  mutate(csn = cumsum(n) - 22222)

# ugh okay also grouped by age
# estimates here will be rough/noisy
acs.combined %>% filter(grepl('pov', var_desc))

acs.combined %>% 
  filter(form %in% 'AS75', grepl('\\:', var_label)) %>% 
  mutate(var_label = gsub('.+\\:\\s', '', var_label)) %>%
  group_by(var_label = gsub('under', '0.0 to', var_label)) %>%
  mutate(var_label = floor(as.numeric(gsub('\\s\\w+.+', '', var_label)))) %>%
  group_by(var_label) %>%
  summarise(E = sum(E), approxm = round(sqrt(sum(M^2)))) %>%
  ungroup() %>%
  mutate(cse = cumsum(E))

# Hmm... 99K below the poverty line in the ACS, but only 83K in our sample?
# not right...
# Also ~420K people at <400% of the poverty line in the ACS, only 403K for us...

# so poverty estimates are suspect...

# The PUMS has ~782K people for whom poverty is estimated, the ACS has ~789K
# although this doesn't explain why our estimates for below 100%FPL would be off by 15K...
# the ACS has 804K people in the county, poverty estimtaed for 789K, so 14K not recorded
# but the PUMS has 22K for whom poverty is not recorded...

# hmm... okay, well, don't think I can do anything about this, it's an issue with the PUMS


##### Living alone ##### 

acs.combined %>% filter(grepl('householder\\sliving\\salone', var_label))
# ~120K
pums.grouped %>% group_by(living_alone, in_group_quarters) %>% summarise(n = sum(n_people))
# ~120K alone in households, 21K alone elsewhere (probably in institutions)


##### Grandparents raising grandchildren #####

pums.grouped %>% group_by(grandparents_raising) %>% summarise(n = sum(n_people))
# 1845
acs.combined %>% filter(grepl('grandp', var_label), !grepl('\\:', var_label))
# off but within the MOE still


##### Family household #####

pums.grouped %>% group_by(family_household) %>% summarise(n = sum(n_people))
# ah you know what, this is households, not people


##### Marital status #####

pums.grouped %>% group_by(marital_status) %>% summarise(n = sum(n_people))

acs.combined %>% filter(grepl('marr?i', var_desc))
# grouped... bleh

acs.combined %>%
  filter(form %in% 'ASPP', !grepl('(fe)?male$', var_label)) %>%
  mutate(
    var_label = gsub('(fe)?male\\:\\s', '', var_label),
    var_label = ifelse(var_label %in% c('widowed', 'divorced'), 'divorced or widowed', var_label)
  ) %>%
  filter(!grepl('\\:', var_label)) %>%
  group_by(var_label) %>%
  summarise(E = sum(E), approxm = round(sqrt(sum(M^2))))

# Divorced+widowed is right (109K vs 110K, within MOE)
# Now married is pretty much dead on, within MOE
# the never married count is large, probably due to not in universe (this is fine by me)


##### Group quarters and institutions #####

pums.grouped %>% group_by(in_group_quarters) %>% summarise(n = sum(n_people))
acs.combined %>% filter(grepl('group', var_desc))

# 20923 in ACS, 21240 in the PUMS
# slightly larger than the MOE but that's okay

pums.grouped %>% group_by(in_institution) %>% summarise(n = sum(n_people))
# ah you know what, I don't think we can get an institutionalized count lol


##### Housing tenure #####

merge(
  pums.grouped %>% 
    group_by(tenure) %>% 
    summarise(n = sum(n_people)) %>%
    mutate(tenure = gsub('in\\.([a-z]+)ed\\.household', '\\1', tenure)),
  acs.combined %>% 
    filter(grepl('tenure', var_desc)) %>% 
    mutate(var_label = gsub('er\\soccupied', '', var_label)) %>%
    select(var_label, E, M),
  by.x = 'tenure', by.y = 'var_label', all = TRUE
) %>%
  mutate(inm = abs(E - n) < M)

# nice, in the MOE for both tenure types


##### Renter/owner cost burden #####

acs.combined %>% filter(form %in% c('ASVH', 'ASWA'))
# lol owner is subdivided by age, owner is not here
# okie dokie

# Renter first:
acs.combined %>% 
  filter(form %in% 'ASVH', grepl('\\d', var_label)) %>%
  mutate(var_label = gsub('less\\sthan', '0.0 to', var_label)) %>%
  mutate(var_label = as.numeric(gsub('\\s.+', '', var_label))) %>%
  group_by(burdened = ifelse(var_label < 30, 'not.burdened', 'burdened')) %>%
  summarise(E = sum(E), approxm = sqrt(sum(M^2)))
  
pums.grouped %>% group_by(ren_burden) %>% summarise(n = sum(n_people))
# ah wwait... this is also households instead of people
# ah well lol

##### Race and ethnicity #####

merge(
  pums.grouped %>%
    select(starts_with('race'), hispanic, n_people) %>%
    pivot_longer(-n_people, names_to = 'race', values_to = 'status') %>%
    filter(!grepl('^not', status)) %>%
    mutate(race = gsub('race\\_', '', race)) %>%
    group_by(race) %>%
    summarise(n = sum(n_people)),
  acs.combined %>%
    filter(form %in% paste0('ASN', 4:9) | form %in% 'ASOB' & grepl('^hisp', var_label)) %>%
    mutate(
      var_label = case_match(
        form,
        'ASN4' ~ 'white',
        'ASN5' ~ 'black',
        'ASN6' ~ 'amind',
        'ASN7' ~ 'asian', 
        'ASN8' ~ 'pacis',
        'ASN9' ~ 'other',
        'ASOB' ~ 'hispanic'
      ),
      M = ifelse(M < 0, 0, M),
    ) %>%
    select(var_label, E, M),
  by.x = 'race', by.y = 'var_label', all = TRUE
) %>%
  mutate(inm = abs(E - n) < M)

# hmm... Pacific Island and American Indian counts are off
# that's weird, wonder what's going on there
# hispanic count is also slightly more than reported but that's not the end of the world

