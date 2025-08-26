# Script for generating synthetic records for Multnomah County RELD+ancestry
# downscaling
# The 2019-2023 ACS PUMS is missing a few RELD-PUMA combinations, so estimates
# end up as zeros
# Here: getting those missing groups and creating synthetic PUMS for them.

library(ipumsr)
library(dplyr)
library(tidyr)

rm(list = ls())

# ==============================================================
# Read in reld assignments for ACS

reald = read.csv('multnomah/01_raw_data/5acs23_orwa_reldpri.csv') %>%
  # fix serial numbers
  mutate(
    serialno = gsub('GQ', '01', serialno),
    serialno = gsub('HU', '00', serialno)
  ) %>%
  # give me reald category
  mutate(
    realdcat = case_match(
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

# ==============================================================
# ==============================================================
# # Missing ancestry combos and other fix

# Read in disability PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16))) 

# Get ancestry for each individual
pums.ancestry = pums.raw %>%
  select(CBSERIAL, PERNUM, PUMA, PERWT, AGE, ANCESTR1, ANCESTR2) %>%
  mutate(
    # Ancestry (this one will be a doozy!)
    across(
      c(ANCESTR1, ANCESTR2), 
      ~ case_match(
        .,
        # Western European (present in b04006)
        # (including 122 German Russians in here...)
        c(1, 3, 5, 8:9, 11, 20:22, 24, 26, 32, 46, 
          49:51, 77:78, 82, 84, 88:89, 91, 97:99, 122, 183) ~ 'ReWestEur',
        # White North Americans (not REALD but still present in b04006)
        # Scots-Irish, PA Dutch, Canadian, French Canadian, Acadian/Cajun, American
        c(87, 929, 931, 935, 936, 940) ~ 'Other.northam',
        # Eastern European (present in b04006)
        c(100, 115, 120, 125, 128:129, 144, 431) ~ 'ReEastEur',
        # Slavic European (present in b04006)
        c(103, 109, 111, 130, 142, 148, 152:154, 171, 176, 178) ~ 'ReSlavic',
        # Other European (in b04006, not corresponding to reald)
        # Eastern European NEC, European
        c(190, 195) ~ 'Other.euro',
        # Caribbean (present in b04006)
        c(300:302, 308, 310, 314, 322, 335:337) ~ 'ReCaribbean',
        # Hispanic South American
        c(360, 370) ~ 'ReHispSou',
        # Other "Arab" (in b04006 but NOT consistent for reald)
        # Algerian, Libyan, North African, Saudi, Yemeni, Kurdish
        c(400, 404, 411, 427, 435, 442, 496) ~ 'Other.arab',
        # North African
        c(402, 406, 429) ~ 'ReNoAfr',
        # Middle eastern
        # (including 600 Afghans here)
        c(416:417, 419, 421, 425, 434, 465, 482, 495, 600) ~ 'ReMidEast',
        # African (in reald and in b04006)
        c(510, 529, 534, 541, 553, 564, 566, 570, 576, 587:588, 593) ~ 'ReAfrican',
        # African other (in b04006 but NOT consistent for reald)
        # e.g., because this is a catch-all it includes Eritrean (ReEthiopian)...
        # Cameroonian, Congolese, Eritrean, Gambian, Guinean, Togo, West African,
        # African, Other Subsaharan African
        c(508, 515, 523, 527, 530, 586, 595, 598:599)  ~ 'Other.african',
        # Ethiopian
        522 ~ 'ReEthiopian',
        # Somalian
        568 ~ 'ReSomalian',
        # ANZAs (in b04006 but NOT consistent for reald)
        c(800, 803) ~ 'Other.anza',
        # Other groups not present in b04006 (assigned to 'other groups')
        # Flemish, British Isles, Prussian, Sicilian, Belorussian, Cossack,
        # Bohemian, Rom, Moldov(i)an, Uzbek, Central Eur., Southern Eur., Western
        # Eur., Spanish, all Hispanic, Grenadian, St. Lucian, "Middle Eastern",
        # all non-Afghan Asians, all Pacific islanders, var. American ethnicities,
        # Mixture, Other
        c(9, 12, 40, 68, 102, 108, 112, 124, 146, 169, 181, 
          185, 187, 200:295, 329, 331, 490, 
          603:799, 808:870, 900:924, 983:995, 998) ~ 'Other',
        .default = NA
      )
  ),
  age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  ### Get ancestry group counts
  # this step is heinously slow, probably not programmed super well...
  # Pivot out to get ancestry and counts in two columns
  pivot_longer(cols = contains('ANC'), names_to = 'a12', values_to = 'ancestry') %>%
  # add a count for how many times each ancestry group is recorded for each respondent
  group_by(CBSERIAL, PERNUM, PUMA, ancestry) %>%
  add_count(name = 'n.ancestry') %>%
  # drop a12 because col it is unnecessary
  select(-a12) %>%
  # distinct will get rid of duplicates
  distinct(.keep_all = TRUE) %>%
  # Now, make some edits to the ancestry 
  group_by(CBSERIAL, PERNUM, PUMA) %>%
  mutate(
    n.ancestry = case_when(
      # Where there are NAs and non-NA ancestry counts, switch NA = 1 to NA = 0
      is.na(ancestry) & any(!is.na(ancestry)) ~ 0,
      # Where there are 2 NAs (i.e., no other ancestry counts) switch from NA = 2 to NA = 1
      is.na(ancestry) & !any(!is.na(ancestry)) ~ 1,
      .default = n.ancestry
    )
  ) %>%
  ungroup() %>%
  # Filtering out individuals with extraneous ancestry (NA = 0 where other
  # ancestry is assigned, see above)
  filter(n.ancestry > 0) %>%
  # Add in 'unidentified' or whatever for the missing ancestries
  mutate(ancestry = ifelse(is.na(ancestry), 'unclassified', ancestry)) %>%
  # # Now merge in reld
  merge(reald %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat))

# Get missing ancestries
missing.ancestry = pums.ancestry %>%
  count(PUMA, ancestry) %>%
  complete(PUMA, ancestry) %>% 
  group_by(ancestry) %>%
  filter(any(is.na(n))) %>%
  ungroup()
  
missing.ancestry

######## Use resampling from PUMS to get dummy individuals
# Workflow for doing this
# - Merge in the rows in this data frame where records are present (n is not NA) with the PUMS to get serial numbers of individuals in the race/age group
# - Drop the PUMA column from these records and merge them in with the PUMA-age-race combos that need records
#   (this will duplicate each record once for each PUMA we need a sample from)
# - Then sample ONE record for each age-race-puma combination

# Set seed for reproducibility
set.seed(979887)

reassigned.ancestry.serials = missing.ancestry %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get all records (serial numbers most importantly)
  merge(pums.ancestry, all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, ancestry, reald) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(missing.ancestry %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(ancestry, reald, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  select(-PERWT) %>%
  ungroup()

reassigned.ancestry.serials

### One more individual to add in:
# Need a Somali aged 18-54 not in GQ for PUMA 5103

# I looked, two options: 2019001346008_4 and 2021000377698_5
# I'm going with 2021

reassigned.ancestry.serials = reassigned.ancestry.serials %>%
  rbind(
    data.frame(
      ancestry = 'ReSomalian', CBSERIAL = 2019001346008, PERNUM = 4, 
      reald = 'Black', PUMA = 5103
    )
  )

reassigned.ancestry.serials
merge(pums.raw, reassigned.ancestry.serials %>% select(-PUMA), by = c('CBSERIAL', 'PERNUM'))


# ==============================================================
# ==============================================================
# # Disability data

# Read in disability PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

pums.disability = pums.raw %>%
  # Add some needed columns
  mutate(
    has.diff = (DIFFREM > 1) | (DIFFMOB > 1) | (DIFFPHYS > 1) | (DIFFEYE > 1) | (DIFFHEAR > 1) | (DIFFCARE > 1),
    in.disb.univ = !(OCC %in% 9800:9850) & !(GQ %in% 3),
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  # # Now merge in reld
  merge(reald %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat))

head(pums.disability)


# I'll include present counts here just because...
pums.missing.diff.reald = pums.disability %>%
  filter(has.diff, in.disb.univ) %>%
  count(PUMA, age, reald) %>%
  complete(PUMA, age, reald) %>%
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup()

pums.missing.diff.reald %>% filter(is.na(n)) %>% nrow()
pums.missing.diff.reald %>% pivot_wider(names_from = PUMA, values_from = n, names_prefix = 'p')
# Okay - entirely missing 'Other' age 85+, age 55-60, age 0-18
# Otherwise... only one NHPI 85+ to copy
# 57 missing combos on the whole.

# Workflow for doing this
# - Merge in the rows in this data frame where records are present (n is not NA) with the PUMS to get serial numbers of individuals in the race/age group
# - Drop the PUMA column from these records and merge them in with the PUMA-age-race combos that need records
#   (this will duplicate each record once for each PUMA we need a sample from)
# - Then sample ONE record for each age-race-puma combination

# Set seed for reproducibility
set.seed(221505)

reassigned.diff.reld.serials = pums.missing.diff.reald %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.disability %>% filter(has.diff) %>% select(-has.diff), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.diff.reald %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

# How does it look?
head(reassigned.diff.reld.serials)

# Merge to see that we got all we need
# (n.x and n.y should be the same)
merge(
  reassigned.diff.reld.serials %>% count(reald, age),
  pums.missing.diff.reald %>% filter(is.na(n)) %>% count(age, reald),
  by = c('reald', 'age'), all = TRUE
)

disability.records.out = rbind(
  reassigned.diff.reld.serials %>%
    select(CBSERIAL, PERNUM, PUMA),
  reassigned.ancestry.serials %>%
    select(CBSERIAL, PERNUM, PUMA)
) %>%
  distinct()

disability.records.out %>% print(n = nrow(.))

# write.csv(records.out, row.names = FALSE, 'multnomah/02_downscaling/phaseII/reld_disability_synthetic_records.csv')


# ==============================================================
# ==============================================================
# # Poverty status

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00053.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Get ancestry per pums
pums.pov = pums.raw %>%
  # Get ancestry data
  select(CBSERIAL, PERNUM, PUMA, AGE, POVERTY) %>%
  mutate(age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  # # Now merge in reld
  merge(reald %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat))

head(pums.pov)

# Most restrictive group here: want groups below the poverty line
# by each age group

### Get PUMA-reald-age combos below poverty line
pums.missing.reld.pov = pums.pov %>%
  # Filter all individuals with poverty available, below poverty line
  filter(POVERTY > 0, POVERTY < 100) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)

nrow(pums.missing.reld.pov)

pums.missing.reld.pov %>% filter(is.na(n)) %>% nrow()
pums.missing.reld.pov %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# Looks like many entirely-missing groups, particularly among older demos.

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(5520970)

reassigned.pov.reld.serials = pums.missing.reld.pov %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.pov %>% filter(POVERTY < 100, POVERTY > 0), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.reld.pov %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

head(reassigned.pov.reld.serials)
nrow(reassigned.pov.reld.serials)

# Merge to see that we got all we need
# (n.x and n.y should be the same)
merge(
  reassigned.pov.reld.serials %>% count(reald, age),
  pums.missing.reld.pov %>% filter(is.na(n)) %>% count(age, reald),
  by = c('reald', 'age'), all = TRUE
)

### Get PUMA-age combos within the poverty universe
pums.missing.reld.pov.univ = pums.pov %>%
  # Filter all individuals with poverty available, below poverty line
  filter(POVERTY > 0) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)

pums.missing.reld.pov.univ %>% filter(is.na(n)) %>% nrow()
pums.missing.reld.pov.univ %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# totally missing 85+ individuals but otherwise looks like we're good

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(8030315)

reassigned.reld.pov.univ.serials = pums.missing.reld.pov.univ %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.pov %>% filter(POVERTY > 0), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.reld.pov.univ %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

### Combine

poverty.records.out = rbind(
  reassigned.pov.reld.serials %>%
    select(CBSERIAL, PERNUM, PUMA),
  reassigned.reld.pov.univ.serials %>%
    select(CBSERIAL, PERNUM, PUMA),
  reassigned.ancestry.serials %>%
    select(CBSERIAL, PERNUM, PUMA)    
) %>%
  distinct()

poverty.records.out %>% print(n = nrow(.))

# write.csv(
#   records.out, row.names = FALSE,
#   'multnomah/02_downscaling/phaseII/reld_poverty_synthetic_records.csv'
# )


# ==============================================================
# ==============================================================
# # Veteran status and housing arrangement

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00056.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Get ancestry per pums
pums.vet.hou = pums.raw %>%
  # Get ancestry data
  select(
    CBSERIAL, PERNUM, PUMA, PERWT, AGE, 
    VETSTAT, starts_with('DIFF'), GQ, OCC, MARST, RELATE
  ) %>%
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
      # 2 ~ 'mar.spno',
      # 3 ~ 'separated',
      4 ~ 'divorced',
      5 ~ 'widowed',
      6 ~ 'nev.mar'
    ),
    # Flag for veterans universe (civilian and 18+)
    in.vet.univ = as.numeric(!(OCC %in% 9800:9850) & (AGE > 17)),
    # Flag for disability universe (civilian noninstitutionalized)
    in.dis.univ = as.numeric(!(OCC %in% 9800:9850) & !(GQ %in% 3)),
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
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
  # # Now merge in reld
  merge(reald %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat))


####### Get PUMA-reald-age combos

pums.missing.reld = pums.vet.hou %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.reld)

pums.missing.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(2214892)

reassigned.reld.serials = pums.missing.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou, all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PUMA) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.reld.serials


####### Get PUMA-reald-age combos in family household

pums.missing.fam.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(family.hous %in% 'fam.hou') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.fam.reld)

pums.missing.fam.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.fam.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(5161449)

reassigned.fam.reld.serials = pums.missing.fam.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(family.hous %in% 'fam.hou'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.fam.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.fam.reld.serials


####### Get PUMA-reald-age combos living alone

pums.missing.alo.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(lives.alone %in% 'liv.alone') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.alo.reld)

pums.missing.alo.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.alo.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
#  That's a lot... several of these will be empty

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(1825104)

reassigned.alo.reld.serials = pums.missing.alo.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(lives.alone %in% 'liv.alone'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.alo.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.alo.reld.serials


####### Get PUMA-reald-age combos in GQ

pums.missing.gq.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(GQ %in% (3:4)) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.gq.reld)

pums.missing.gq.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.gq.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# many combinations completely missing

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(122111)

reassigned.gq.reld.serials = pums.missing.gq.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(GQ %in% (3:4)), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.gq.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.gq.reld.serials


####### Get PUMA-reald-age combos in family household

pums.missing.grp.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(grandparent %in% 'witha.grandparent') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.grp.reld)

pums.missing.grp.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.grp.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# okay we're going to get very few samples from this...

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(1152147)

reassigned.grp.reld.serials = pums.missing.grp.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(grandparent %in% 'witha.grandparent'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.grp.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.grp.reld.serials


####### Get PUMA-reald-age combos in non-family households

pums.missing.nonfam.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(family.hous %in% 'nonfam.hou' & !(GQ %in% 3:4)) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.nonfam.reld)

pums.missing.nonfam.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.nonfam.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(76344031)

reassigned.nonfam.reld.serials = pums.missing.nonfam.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(family.hous %in% 'nonfam.hou' & !(GQ %in% 3:4)), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.nonfam.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.nonfam.reld.serials


####### Get PUMA-reald-age combos who are married

pums.missing.nowmar.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(mar %in% 'now.married') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.nowmar.reld)

pums.missing.nowmar.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.nowmar.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(3763210)

reassigned.nowmar.reld.serials = pums.missing.nowmar.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(mar %in% 'now.married'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.nowmar.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.nowmar.reld.serials


####### Get PUMA-reald-age combos for never-married 

pums.missing.nevmar.reld = pums.vet.hou %>%
  # Filter all individuals never married
  filter(mar %in% 'nev.mar', AGE > 14) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.nevmar.reld)

pums.missing.nevmar.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.nevmar.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(31556121)

reassigned.nevmar.reld.serials = pums.missing.nevmar.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(mar %in% 'nev.mar', AGE > 14), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.nevmar.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.nevmar.reld.serials


####### Get PUMA-reald-age combos for divorced/widowed 

pums.missing.divwid.reld = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(mar %in% c('divorced', 'widowed'), AGE > 14) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.nevmar.reld)

pums.missing.divwid.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.divwid.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(20140151)

reassigned.divwid.reld.serials = pums.missing.divwid.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(mar %in% c('divorced', 'widowed'), AGE > 14), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.divwid.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.divwid.reld.serials


####### Get PUMA-reald-age combos for never-married 

pums.missing.vet.reld = pums.vet.hou %>%
  # Filter all individuals never married
  filter(VETSTAT %in% 2) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.vet.reld)

pums.missing.vet.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.vet.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# lots missing (although I'm sure many of these are age)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(2686241)

reassigned.vet.reld.serials = pums.missing.vet.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(VETSTAT %in% 2), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.vet.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.vet.reld.serials

####### Get PUMA-age combos for veterans in group quarters

pums.missing.gq.vet = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(VETSTAT %in% 2, GQ %in% (3:4)) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(age, PUMA)


nrow(pums.missing.gq.vet)

pums.missing.gq.vet %>% filter(is.na(n)) %>% nrow()
pums.missing.gq.vet %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(9209)

reassigned.gq.vet.serials = pums.missing.gq.vet %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(VETSTAT %in% 2, GQ %in% (3:4)), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.gq.vet %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1/PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.gq.vet.serials


####### Get PUMA-age combos for veterans in a multigenerational household

pums.missing.grp.vet = pums.vet.hou %>%
  # Filter all individuals with poverty available, below poverty line
  filter(VETSTAT %in% 2, grandparent %in% 'witha.grandparent') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(age, PUMA)


nrow(pums.missing.grp.vet)

pums.missing.grp.vet %>% filter(is.na(n)) %>% nrow()
pums.missing.grp.vet %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(3122778)

reassigned.grp.vet.serials = pums.missing.grp.vet %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.vet.hou %>% filter(VETSTAT %in% 2, grandparent %in% 'witha.grandparent'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.grp.vet %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.grp.vet.serials

# (will export below)


# ==============================================================
# ==============================================================
# # Housing cost

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro('multnomah/01_raw_data/usa_00057.xml')

pums.cost = pums.raw %>%
  # remove detail columns
  select(-c(ANCESTR1D, ANCESTR2D)) %>%
  # Add and modify columns
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
    in.inst = GQ %in% 3,
    # Binned ages for output
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  # # Now merge in reld
  merge(reald %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat))



####### Get missing PUMA-reald-age combos in GQ (recycling code from above)

pums.missing.inst.reld = pums.cost %>%
  # Filter all individuals in gq
  filter(GQ %in% 3) %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.inst.reld)

pums.missing.inst.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.inst.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# many combinations completely missing

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(526610)

reassigned.inst.reld.serials = pums.missing.inst.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.cost %>% filter(GQ %in% 3), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.inst.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.inst.reld.serials


####### Get missing PUMA-reald-age combos for renters at >30% of income

pums.missing.r30.reld = pums.cost %>%
  # Filter all individuals in gq
  filter(ren.pct %in% 'over30') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.r30.reld)

pums.missing.r30.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.r30.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# a couple of cases with >85 or 55-60 comletely missing

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(2531752) # also using same seed as above

reassigned.r30.reld.serials = pums.missing.r30.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.cost %>% filter(ren.pct %in% 'over30'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.r30.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.r30.reld.serials

####### Get missing PUMA-reald-age combos for owners at >30% of income

pums.missing.o30.reld = pums.cost %>%
  # Filter all individuals in gq
  filter(own.pct %in% 'over30') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.o30.reld)

pums.missing.o30.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.o30.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# missing most cases with >85

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(7719045) # also using same seed as above

reassigned.o30.reld.serials = pums.missing.o30.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.cost %>% filter(own.pct %in% 'over30'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.o30.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.o30.reld.serials


### Just in case, it's probably a good idea to also do this for tenure
# (I know that *some* of these will be handled by the over-30, but we know from
# the Somali GQ example that having only one sample of rare combinations of
# cases can skew estimates)

####### Get missing PUMA-reald-age combos for renters

pums.missing.ren.reld = pums.cost %>%
  # Filter all individuals in gq
  filter(tenure %in% 'renter') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)

nrow(pums.missing.ren.reld)

pums.missing.ren.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.ren.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# everybody here except for 85+

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(6844512) # also using same seed as above

reassigned.ren.reld.serials = pums.missing.ren.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.cost %>% filter(tenure %in% 'renter'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.ren.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.ren.reld.serials


####### Get missing PUMA-reald-age combos for owners at >30% of income

pums.missing.own.reld = pums.cost %>%
  # Filter all individuals in gq
  filter(tenure %in% 'owner') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, reald, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, reald, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(reald, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(reald, age, PUMA)


nrow(pums.missing.own.reld)

pums.missing.own.reld %>% filter(is.na(n)) %>% nrow()
pums.missing.own.reld %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# everybody here except for 85+

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(22040751) # also using same seed as above

reassigned.own.reld.serials = pums.missing.own.reld %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.cost %>% filter(tenure %in% 'owner'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.own.reld %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.own.reld.serials


######## Collect into one data frame # THIS IS FOR REAL

vet.hou.records.out = rbind(
  reassigned.alo.reld.serials,
  reassigned.divwid.reld.serials,
  reassigned.fam.reld.serials,
  reassigned.gq.reld.serials,
  reassigned.gq.vet.serials,
  reassigned.grp.reld.serials,
  reassigned.grp.vet.serials,
  reassigned.nevmar.reld.serials,
  reassigned.nonfam.reld.serials,
  reassigned.nowmar.reld.serials,
  reassigned.reld.serials,
  reassigned.vet.reld.serials,
  reassigned.inst.reld.serials,
  reassigned.ren.reld.serials,
  reassigned.own.reld.serials,
  reassigned.r30.reld.serials,
  reassigned.o30.reld.serials
) %>%
  select(CBSERIAL, PERNUM, PUMA) %>%
  rbind(reassigned.ancestry.serials %>% select(CBSERIAL, PERNUM, PUMA)) %>%
  distinct()

nrow(vet.hou.records.out)

# write.csv(
#   records.out, row.names = FALSE,
#   'multnomah/02_downscaling/phaseII/reld_vet_hou_synthetic_records.csv'
# )


# ==============================================================
# ==============================================================
# # Housing cost

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro('multnomah/01_raw_data/usa_00060.xml')

pums.lang = pums.raw %>%
  filter(LANGUAGE > 1) %>%
  # Add and modify columns
  mutate(
    # Language
    language = case_match(
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
    proficiency = case_when(
      !(LANGUAGE %in% 1) & SPEAKENG %in% 2:4 ~ 'eng.very.well',
      SPEAKENG %in% c(1, 5:6) ~ 'less.than.very.well',
      .default = NA
    ),
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  )

####### Get missing PUMA-language-age combos

pums.missing.lang = pums.lang %>%
  # Get all PUMA-race-age combos present
  count(PUMA, language, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, language, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(language, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(language, age, PUMA)

pums.missing.lang
# lol... looks like a lot bleh...
pums.missing.lang %>% 
  group_by(language, age) %>% filter(any(!is.na(n))) %>% ungroup() %>% 
  filter(is.na(n)) %>% nrow()
# okay... 63 new records just to get all language-age combos

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(6650452)

reassigned.lang.serials = pums.missing.lang %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.lang, all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, language, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.lang %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(language, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.lang.serials

####### Get missing PUMA-language-proficiency combos

pums.missing.lang.prof = pums.lang %>%
  # subset to *only* low-proficiency individuals
  filter(proficiency %in% 'less.than.very.well') %>%
  # Get all PUMA-race-age combos present
  count(PUMA, language, age) %>%
  # Get all possible combinations (missing will be n == NA)
  complete(PUMA, language, age) %>%
  # give me any race-age combo missing from at least one PUMA
  group_by(language, age) %>%
  filter(any(is.na(n))) %>%
  ungroup() %>%
  arrange(language, age, PUMA)

pums.missing.lang.prof %>%
  group_by(language, age) %>% filter(any(!is.na(n))) %>% ungroup() %>% 
  filter(is.na(n)) %>% nrow()
# 100 new records... cool

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(3310) # also using same seed as above

reassigned.lang.prof.serials = pums.missing.lang.prof %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  merge(pums.lang %>% filter(proficiency %in% 'less.than.very.well'), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, PERWT, language, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.lang.prof %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(language, age, PUMA) %>%
  slice_sample(n = 1, weight_by = 1 / PERWT) %>%
  ungroup() %>%
  select(-PERWT)

reassigned.lang.prof.serials

### Combine

lang.records.out = rbind(
  reassigned.lang.serials,
  reassigned.lang.prof.serials
) %>%
  select(CBSERIAL, PERNUM, PUMA) %>%
  rbind(reassigned.ancestry.serials %>% select(CBSERIAL, PERNUM, PUMA)) %>%
  distinct()

# ==============================================================
# ==============================================================

# synthetic.poverty = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_poverty_synthetic_records.csv')
# synthetic.vet.hou = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_vet_hou_synthetic_records.csv')
# synthetic.disabil = read.csv('multnomah/02_downscaling/phaseII/reld_disability_synthetic_records.csv')
# synthetic.houcost = read.csv('multnomah/02_downscaling/phaseII/reld_housing-cost_synthetic_records.csv')

all.records.out = rbind(
  disability.records.out, 
  poverty.records.out, 
  vet.hou.records.out,
  lang.records.out
) %>%
  distinct()

nrow(all.records.out)
# lol

write.csv(
  all.records.out,
  'multnomah/02_downscaling/phaseII/reld_all_synthetic_records.csv',
  row.names = FALSE
)



# # ==============================================================
# # ==============================================================
# 
# 
# # See if there are any extra combos we can make using 2022 data...
# 
# # ==============================================================
# # Read in reld assignments from 2022 ACS
# 
# reald22 = read.csv('multnomah/01_raw_data/5acs22_orwa_reldpri.csv') %>%
#   filter(grepl('^2018', serialno)) %>%
#   # fix serial numbers
#   mutate(
#     serialno = gsub('GQ', '01', serialno),
#     serialno = gsub('HU', '00', serialno)
#   ) %>%
#   # give me reald category
#   mutate(
#     realdcat = case_match(
#       reldpri,
#       c('HisMex', 'HisCen', 'HisOth', 'HisSou') ~ 'HispLat',
#       c('NHPIoth', 'Cham', 'Samoan', 'COFA', 'Marshall', 'NatHaw') ~ 'NHPI',
#       c('WestEur', 'WhiteOth', 'Slavic', 'EastEur') ~ 'White',
#       c('AmInd', 'AlaskNat', 'LatInd') ~ 'AmInd',
#       c('AfrAm', 'African', 'Caribbean', 'Ethiopian', 'Somali') ~ 'Black',
#       c('MidEast', 'NoAfr') ~ 'MENA',
#       c('Chinese', 'Filipino', 'Cambodian', 'Vietnamese', 'Korean', 
#         'Japanese', 'Myanmar', 'AsianInd', 'SoAsian', 'Hmong', 
#         'Laotian', 'AsianOth') ~ 'Asian',
#       'RaceOth' ~ 'Other'
#     )
#   )
# 
# 
# # ==============================================================
# # Read in 2018-2022 ACS PUMS
# 
# pums22 = read_ipums_ddi('multnomah/01_raw_data/usa_00058.xml') %>%
#   # Read in data
#   read_ipums_micro() %>%
#   filter(MULTYEAR < 2019) %>%
#   # Fix PUMA
#   mutate(PUMA = ifelse(PUMA > 5100, PUMA, PUMA + (5100-1300))) %>%
#   merge(reald22 %>% select(CBSERIAL = serialno, PERNUM = sporder, reald = realdcat)) %>%
#   mutate(age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
#   # Get rid of some unnecessary columns
#   select(-c(ANCESTR1D, ANCESTR2D, VETSTATD, COUNTYICP, STRATA, SERIAL)) %>%
#   # Get living alone and/or multigeneration flags (need to do this on whole
#   # dataset) %>%
#   group_by(CBSERIAL) %>%
#   mutate(
#     lives.alone = ifelse(n() < 2, 'liv.alone', 'not.liv.alone'),
#     family.hous = ifelse(any(RELATE[PERNUM > 1] <= 10), 'fam.hou', 'nonfam.hou'),
#     grandparent = ifelse(
#       ((any(RELATE %in% 9)) | (any(RELATE %in% 3:4) & any(RELATE %in% 5:6))),
#       'witha.grandparent',
#       'without.grandparent'
#     )
#   ) %>%
#   ungroup()
# 
# rm(missing.ancestry)
# 
# # ==============================================================
# # Get non-reld misses and reconcile with PUMS
# 
# misses.non.reld = ls() %>% 
#   grep('missing', ., value = TRUE) %>% 
#   grep('rea?ld', ., value = TRUE, invert = TRUE) %>%
#   as.list() %>% 
#   lapply(FUN = \(x) get(x) %>% mutate(table.is = deparse(x)))
# 
# misses.non.reld = misses.non.reld %>% 
#   do.call(what = rbind) %>%
#   mutate(table.is = gsub('\\"(.+)\\"', '\\1', table.is)) %>%
#   group_by(age, table.is) %>%
#   filter(all(is.na(n)))
# 
# # ah these are both because there are no under-18 vets...
# 
# 
# # ==============================================================
# # Get eld misses and reconcile with PUMS
# 
# misses.reld = ls() %>% 
#   grep('missing', ., value = TRUE) %>% 
#   grep('rea?ld', ., value = TRUE) %>%
#   as.list() %>% 
#   lapply(FUN = \(x) get(x) %>% mutate(table.is = deparse(x)))
# 
# misses.reld = misses.reld %>% 
#   do.call(what = rbind) %>%
#   mutate(table.is = gsub('\\"(.+)\\"', '\\1', table.is)) %>%
#   group_by(age, reald, table.is) %>%
#   filter(all(is.na(n))) %>%
#   distinct(age, reald, table.is) %>%
#   arrange(reald, age) %>%
#   ungroup()
# 
# # Looks like a lot!
# 
# # Merge this with the pums22 to see if we can fish anything out...
# 
# in.old.pums = merge(pums22, misses.reld, by = c('age', 'reald'))
# # Hmm... okay let's see what happens
# 
# # Okay I think we'll need to make a lot of new columns...
# 
# in.old.pums %>%
#   mutate(
#     is.vet = VETSTAT > 1 & grepl('vet', table.is),
#     nowmar = MARST %in% 1:3 & grepl('nowmar', table.is),
#     divwid = MARST %in% 4 &   grepl('divwid', table.is),
#     in.ins = GQ %in% 3 &      grepl('inst', table.is),
#     nonfam = family.hous %in% 'nonfam.hou' & grepl('nonfam', table.is),
#     in.grq = GQ %in% 3:4 & grepl('gq', table.is),
#     is.alo = lives.alone %in% 'liv.alone' & grepl('alo', table.is),
#     is.dis = ((DIFFREM > 1) | (DIFFPHYS > 1) | (DIFFMOB > 1) | (DIFFCARE > 1) | (DIFFEYE > 1) | (DIFFHEAR > 1)) &
#       !(OCC %in% 9800:9850) & !(GQ %in% 3) & grepl('diff', table.is)
#   ) %>%
#   filter(if_any(where(is.logical)))
# 
# # grand total of four records...
# 
# in.old.pums %>% filter(grepl('vet', table.is)) %>% count(VETSTAT)
# in.old.pums %>% filter(grepl('nowmar', table.is)) %>% count(MARST)
# in.old.pums %>% filter(grepl('divwid', table.is)) %>% count(MARST)
# in.old.pums %>% filter(grepl('nonfam', table.is)) %>% count(family.hous)
# in.old.pums %>% filter(grepl('alo', table.is)) %>% count(lives.alone)
# in.old.pums %>% filter(grepl('diff', table.is))
# 
# # Hmm... lol