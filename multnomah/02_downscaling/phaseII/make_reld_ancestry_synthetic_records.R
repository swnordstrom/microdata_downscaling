# Script for generating synthetic records for Multnomah County RELD+ancestry
# downscaling
# The 2019-2023 ACS PUMS is missing a few RELD-PUMA combinations, so estimates
# end up as zeros
# Here: getting those missing groups and creating synthetic PUMS for them.

library(ipumsr)
library(dplyr)
library(tidyr)

rm(list = ls())

# -------------------------------------------------------------------
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
# # Disability data

# Read in disability PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# -------------------------------------------------------------------
# Get a data frame with ancestry(s) and reld category assigned for each record

# also need to get the disability status in this table.
# (and age, lol)

pums.ancestry = pums.raw %>%
  # Get ancestry data
  select(CBSERIAL, PERNUM, PUMA, AGE, GQ, OCC, ANCESTR1, ANCESTR2, starts_with('DIFF')) %>%
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
    has.diff = (DIFFREM > 1) | (DIFFMOB > 1) | (DIFFPHYS > 1) | (DIFFEYE > 1) | (DIFFHEAR > 1) | (DIFFCARE > 1),
    in.disb.univ = !(OCC %in% 9800:9850) & !(GQ %in% 3),
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  select(-c(starts_with('DIFF'), AGE, GQ, OCC)) %>%
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

head(pums.ancestry)

# -------------------------------------------------------------------
# Assess missing combos

missing.ancestry = pums.ancestry %>%
  count(PUMA, ancestry) %>%
  complete(PUMA, ancestry) %>% 
  filter(is.na(n))

# I'll include present counts here just because...
missing.age.reald.diff = pums.ancestry %>%
  filter(has.diff, in.disb.univ) %>%
  count(PUMA, age, reald) %>%
  complete(PUMA, age, reald) %>%
  group_by(age, reald) %>%
  filter(any(is.na(n))) %>%
  ungroup()

missing.age.reald.diff %>% filter(is.na(n)) %>% nrow()
missing.age.reald.diff %>% pivot_wider(names_from = PUMA, values_from = n, names_prefix = 'p')
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

reassigned.serials = missing.age.reald.diff %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.ancestry %>% filter(has.diff) %>% select(-has.diff), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(missing.age.reald.diff %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

# How does it look?
head(reassigned.serials)

# Merge to see that we got all we need
# (n.x and n.y should be the same)
merge(
  reassigned.serials %>% count(reald, age),
  missing.age.reald.diff %>% filter(is.na(n)) %>% count(age, reald),
  by = c('reald', 'age'), all = TRUE
)

records.out = rbind(
  reassigned.serials %>%
    select(CBSERIAL, PERNUM, PUMA, reald, age) %>%
    mutate(ancestry = NA),
  missing.ancestry %>%
    mutate(CBSERIAL = NA, PERNUM = NA, reald = NA, age = NA) %>%
    select(CBSERIAL, PERNUM, PUMA, reald, age, ancestry)
)

records.out %>% print(n = nrow(.))

write.csv(records.out, row.names = FALSE, 'multnomah/02_downscaling/phaseII/reld_ancestry_synthetic_records.csv')


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
pums.anc.pov = pums.raw %>%
  # Get ancestry data
  select(CBSERIAL, PERNUM, PUMA, AGE, ANCESTR1, ANCESTR2, POVERTY) %>%
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

head(pums.anc.pov)

# Most restrictive group here: want groups below the poverty line
# by each age group

### Get PUMA-reald-age combos below poverty line
pums.missing.pov = pums.anc.pov %>%
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

nrow(pums.missing.pov)

pums.missing.pov %>% filter(is.na(n)) %>% nrow()
pums.missing.pov %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# Looks like many entirely-missing groups, particularly among older demos.

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(5520970)

reassigned.pov.serials = pums.missing.pov %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.anc.pov %>% filter(POVERTY < 100, POVERTY > 0), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.pov %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

head(reassigned.pov.serials)
nrow(reassigned.pov.serials)

# Merge to see that we got all we need
# (n.x and n.y should be the same)
merge(
  reassigned.pov.serials %>% count(reald, age),
  pums.missing.pov %>% filter(is.na(n)) %>% count(age, reald),
  by = c('reald', 'age'), all = TRUE
)

### Get PUMA-age combos within the poverty universe
pums.missing.univ = pums.anc.pov %>%
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

pums.missing.univ %>% filter(is.na(n)) %>% nrow()
pums.missing.univ %>% pivot_wider(names_from = PUMA, names_prefix = 'p', values_from = n)
# totally missing 85+ individuals but otherwise looks like we're good

### Get sample of dummy PUMS serial numbers, use workflow from above
set.seed(8030315)

reassigned.univ.serials = pums.missing.univ %>%
  # Getting just the age-race combos we have records for
  filter(!is.na(n)) %>%
  select(-n) %>%
  # Merge to get records (serial numbers most importantly)
  # (also make sure to sample only from the records of people with a disability)
  merge(pums.anc.pov %>% filter(POVERTY > 0), all.x = TRUE) %>%
  # Remove unnecessary columns (importantly, the PUMA column)
  select(CBSERIAL, PERNUM, reald, age) %>%
  # Now, merge back in with the list of PUMA-age-race columns we need
  # (this is the step where records get duplicated)
  merge(pums.missing.pov %>% filter(is.na(n)) %>% select(-n)) %>%
  # Now sample one record per PUMA-age-race column
  group_by(reald, age, PUMA) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Get missing ancestries (probably same as above)
missing.ancestry = pums.anc.pov %>%
  count(PUMA, ancestry) %>%
  complete(PUMA, ancestry) %>% 
  filter(is.na(n))

records.out = rbind(
  reassigned.pov.serials %>%
    select(CBSERIAL, PERNUM, PUMA, reald, age) %>%
    mutate(ancestry = NA),
  reassigned.univ.serials %>%
    select(CBSERIAL, PERNUM, PUMA, reald, age) %>%
    mutate(ancestry = NA)
) %>%
  distinct() %>%
  rbind(
    missing.ancestry %>%
      mutate(CBSERIAL = NA, PERNUM = NA, reald = NA, age = NA) %>%
      select(CBSERIAL, PERNUM, PUMA, reald, age, ancestry)
  )

records.out %>% print(n = nrow(.))

write.csv(
  records.out, row.names = FALSE, 
  'multnomah/02_downscaling/phaseII/reld_ancestry_poverty_synthetic_records.csv'
)
