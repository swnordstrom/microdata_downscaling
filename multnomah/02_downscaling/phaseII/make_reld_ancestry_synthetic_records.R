# Script for generating synthetic records for Multnomah County RELD+ancestry
# downscaling
# The 2019-2023 ACS PUMS is missing a few RELD-PUMA combinations, so estimates
# end up as zeros
# Here: getting those missing groups and creating synthetic PUMS for them.

library(ipumsr)
library(dplyr)
library(tidyr)

rm(list = ls())

# ----------------------
# Data readins

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml') %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

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

# -------------------------------------------------------------------
# Get a data frame with ancestry(s) and reld category assigned for each record

# also need to get the disability status in this table.
# (and age, lol)

pums.ancestry = pums.raw %>%
  # Get ancestry data
  select(CBSERIAL, PERNUM, PUMA, AGE, ANCESTR1, ANCESTR2, starts_with('DIFF')) %>%
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
    age = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  select(-c(starts_with('DIFF'), AGE)) %>%
  # Convert to long form and collapse ancestries
  pivot_longer(starts_with('ANCESTR'), names_to = 'varb', values_to = 'ancestry') %>%
  filter(!is.na(ancestry)) %>%
  select(-varb) %>%
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
missing.age.reald = pums.ancestry %>%
  filter(has.diff) %>%
  count(PUMA, age, reald) %>%
  complete(PUMA, age, reald) %>%
  group_by(age, reald) %>%
  filter(any(is.na(n))) %>%
  ungroup()

missing.age.reald %>% filter(is.na(n)) %>% nrow()
missing.age.reald %>% pivot_wider(names_from = PUMA, values_from = n, names_prefix = 'p')
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

reassigned.serials = missing.age.reald %>%
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
  merge(missing.age.reald %>% filter(is.na(n)) %>% select(-n)) %>%
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
  missing.age.reald %>% filter(is.na(n)) %>% count(age, reald),
  by = c('reald', 'age'), all = TRUE
)

# Don't have Other individuals (not present anywhere in the PUMS)
# but that's not the end of the world...

# missing.reld = pums.ancestry %>%
#   count(PUMA, reld) %>%
#   complete(PUMA, reld) %>%
#   filter(is.na(n))
# 
# missing.reld %>% arrange(PUMA)
# missing.ancestry %>% arrange(PUMA)
# # Not sure what COFA is
# pums.ancestry %>% filter(reld %in% 'COFA') %>% merge(pums.raw)
# # ancestr1: 8[257]0
# ipums_val_labels(pums.raw, 'ANCESTR1') %>% filter(val %in% (800 + c(20, 50, 70)))
# # okay... Micronesian (plus some strays?)
# 
# merge(missing.reld %>% select(reld), pums.ancestry) %>% distinct(reld, ancestry)
# # ah... okay yeah this won't work super well lol
# # basically all of these are `other`...
# 
# # Make a data frame getting all PUMS records in any of the missing relds
# pums.missing.reld = merge(missing.reld %>% select(reld), pums.ancestry) %>%
#   distinct(CBSERIAL, PERNUM, reld) %>%
#   merge(pums.raw)
# 
# # How many of each RELD do we have
# pums.missing.reld %>% count(reld)
# # okay well that's good at least
# 
# pums.missing.reld %>%
#   select(reld, matches('RAC[^ED]'), ANCESTR1, ANCESTR2) %>%
#   mutate(
#     # Ancestry (this one will be a doozy!)
#     across(
#       c(ANCESTR1, ANCESTR2), 
#       ~ case_match(
#         .,
#         # Western European (present in b04006)
#         # (including 122 German Russians in here...)
#         c(1, 3, 5, 8:9, 11, 20:22, 24, 26, 32, 46, 
#           49:51, 77:78, 82, 84, 88:89, 91, 97:99, 122, 183) ~ 'ReWestEur',
#         # White North Americans (not REALD but still present in b04006)
#         # Scots-Irish, PA Dutch, Canadian, French Canadian, Acadian/Cajun, American
#         c(87, 929, 931, 935, 936, 940) ~ 'Other.northam',
#         # Eastern European (present in b04006)
#         c(100, 115, 120, 125, 128:129, 144, 431) ~ 'ReEastEur',
#         # Slavic European (present in b04006)
#         c(103, 109, 111, 130, 142, 148, 152:154, 171, 176, 178) ~ 'ReSlavic',
#         # Other European (in b04006, not corresponding to reald)
#         # Eastern European NEC, European
#         c(190, 195) ~ 'Other.euro',
#         # Caribbean (present in b04006)
#         c(300:302, 308, 310, 314, 322, 335:337) ~ 'ReCaribbean',
#         # Hispanic South American
#         c(360, 370) ~ 'ReHispSou',
#         # Other "Arab" (in b04006 but NOT consistent for reald)
#         # Algerian, Libyan, North African, Saudi, Yemeni, Kurdish
#         c(400, 404, 411, 427, 435, 442, 496) ~ 'Other.arab',
#         # North African
#         c(402, 406, 429) ~ 'ReNoAfr',
#         # Middle eastern
#         # (including 600 Afghans here)
#         c(416:417, 419, 421, 425, 434, 465, 482, 495, 600) ~ 'ReMidEast',
#         # African (in reald and in b04006)
#         c(510, 529, 534, 541, 553, 564, 566, 570, 576, 587:588, 593) ~ 'ReAfrican',
#         # African other (in b04006 but NOT consistent for reald)
#         # e.g., because this is a catch-all it includes Eritrean (ReEthiopian)...
#         # Cameroonian, Congolese, Eritrean, Gambian, Guinean, Togo, West African,
#         # African, Other Subsaharan African
#         c(508, 515, 523, 527, 530, 586, 595, 598:599)  ~ 'Other.african',
#         # Ethiopian
#         522 ~ 'ReEthiopian',
#         # Somalian
#         568 ~ 'ReSomalian',
#         # ANZAs (in b04006 but NOT consistent for reald)
#         c(800, 803) ~ 'Other.anza',
#         # Other groups not present in b04006 (assigned to 'other groups')
#         # Flemish, British Isles, Prussian, Sicilian, Belorussian, Cossack,
#         # Bohemian, Rom, Moldov(i)an, Uzbek, Central Eur., Southern Eur., Western
#         # Eur., Spanish, all Hispanic, Grenadian, St. Lucian, "Middle Eastern",
#         # all non-Afghan Asians, all Pacific islanders, var. American ethnicities,
#         # Mixture, Other
#         c(9, 12, 40, 68, 102, 108, 112, 124, 146, 169, 181, 
#           185, 187, 200:295, 329, 331, 490, 
#           603:799, 808:870, 900:924, 983:995, 998) ~ 'Other',
#         .default = NA
#       )
#     )
#   ) %>%
#   distinct() %>%
#   arrange(reld)
# 
# # Hmm... okay
# # - AlaskaNat: RACAMIND, Ancestry Other
# # - COFA: RACPACIS, Ancestry Other
# # - Cham: RACPACIS, Ancestry Other
# # - Marshall: RACPACIS, Ancestry Other
# # - Samoan: RACPACIS, Ancestry Other
# # - Somali: RACBLK, Ancestry Somalian
# 
# # For missing ancestry groups:
# # - Already have Somali covered (see above)
# # - Other is `Other.arab`
# 
# pums.ancestry %>% filter(ancestry %in% 'Other.arab') %>% merge(pums.raw)
# # lol... okay some MidEast some NoAfr (guess it doesn't matter for our purposes though)
# # they are all white... not sure anything else is needed though!

# -------------------------------------------------------------------
# Make records

# records.out = rbind(
#   missing.reld %>% 
#     # Assign race and ancestry for each pseudo-record
#     # (these won't actually be accessed by code... just here as a reminder when
#     # assembling matrices)
#     mutate(
#       ancestry = ifelse(reld %in% 'Somali', 'ReSomalian', 'Other'),
#       race = case_match(
#         reld,
#         c('Marshall', 'Cham', 'COFA', 'Samoan') ~ 'pacis',
#         'Somali' ~ 'blk',
#         'AlaskNat' ~ 'amind'
#       )
#     ) %>%
#     select(-n),
#   missing.ancestry %>%
#     # Assign race and reld for each pseudo-record
#     # (race won't actually be accessed by code... just here as a reminder when
#     # assembling matrices)
#     mutate(
#       reld = ifelse(ancestry %in% 'ReSomalian', 'Somali', NA),
#       race = ifelse(ancestry %in% 'ReSomalian', 'blk', 'wht')
#     ) %>%
#     select(PUMA, reld, ancestry, race)
# ) %>%
#   distinct()

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
