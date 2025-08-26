library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# ==============================================================
# Read in raw and curated data

# Get PUMS ddi
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00064.xml')

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro(pums.raw.ddi)

# Read in tabular data
acs.tab.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0043_csv.zip', file_select = 1) %>%
  filter(grepl('Multno', COUNTY))
acs.tab.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0043_csv.zip', file_select = 2) %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')

# Get synthetic records (PUMS and dummy)
synthetic.records = read.csv('multnomah/02_downscaling/phaseII/reld_all_synthetic_records.csv')

# ==============================================================
# Merge data products together and do some pre-processing

### Format ACS tables and merge in to get PUMAs for each tract

acs.tab = merge(
  acs.tab.raw1 %>% select(TRACTA, matches('[EM]\\d{3}$')),
  acs.tab.raw2 %>% select(TRACTA, matches('[EM]\\d{3}$'))
) %>%
  # Rename tract column for merge
  mutate(tract = as.numeric(TRACTA)) %>%
  select(-TRACTA) %>%
  # Merge to get PUMAs
  merge(puma.tract %>% filter(puma > 5000) %>% select(tract, puma)) %>%
  # Do some label name/constraint massaging 
  pivot_longer(-c(puma, tract), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    rbind(
      ipums_var_info(acs.tab.raw1, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(acs.tab.raw2, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
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
  separate_wider_delim(var_desc, delim = ' (universe: ', names = c('var_desc', 'univ')) %>%
  # Change age labels ('Under' to '00 to', and add preceding zero where needed)
  mutate(
    # Handle age bins (no decimal)
    var_label = gsub('under\\s(\\d)', '00 to \\1', var_label),
    var_label = gsub('^(\\d{1})\\s', '0\\1 ', var_label),
    var_label = gsub('\\:\\s(\\d{1})\\s', ': 0\\1 ', var_label),
    # Handle percentages
    var_label = gsub('less\\sthan\\s(\\d)', '00.0 to \\1', var_label)
  ) %>%
  # Get rid of some duplicate cases
  filter(
    # Hispanic total (same as sex/age total)
    !(form %in% 'ASOB' & var_label %in% 'total'),
    # Get rid of any "spouse present/absent" sub-cases of "now married"
    !(form %in% 'ASPP' & grepl('spouse', var_label)),
    # Get rid of sub-cases of family households
    !(form %in% 'ASOW' & grepl('^family\\shouseholds?\\:', var_label)),
    # # Get rid of the finer-grained estimates on grandparents
    # !(form %in% 'AS4J' & grepl('\\:', var_label)),
    # # Household total (duplicarted between grandparents and household type table)
    # !(form %in% 'AS4J' & var_label %in% 'total'),
    # # # Get rid of households with/without grandparents (covered by separate dataset)
    !(form %in% 'AS4J'),
    # Getting rid of extra info (e.g., age breakdowns) on grandparents raising grandchildren
    !(form %in% 'AS32' & grepl('\\:', var_label))
  ) %>%
  # Change 'total' label to race for race columns
  mutate(
    var_label = ifelse(
      form %in% paste0('ASN', 4:9),
      gsub('^(.+)\\salone.+', '\\1', var_desc),
      var_label
    )
  )

# Format the ancestry table

ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) %>%
  mutate(
    form = NA, var_desc = 'ancestry re-groupings', univ = 'people reporting ancestry'
  ) %>%
  select(names(acs.tab))

# Combine the acs tables into one:
acs.tab = rbind(acs.tab, ancestry.tab)

### Format PUMS
# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, COUNTYICP,
      STRATA, HISPAND, ANCESTR1D, ANCESTR2D, VETSTATD, HHWT, 
      RELATED, OWNERSHPD
    )
  )


# ==============================================================
# Get matrices/vectors for PMED-M

# acs.tab %>% distinct(var_label) %>% filter(grepl('\\d', var_label))

y.tab = acs.tab %>%
  mutate(
    # various totals
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    is.hou.total = form %in% 'ASOW' & var_label %in% 'total',
    is.vet.total = form %in% 'ASSH' & var_label %in% 'total',
    is.mar.total = form %in% 'ASPP' & var_label %in% 'total',
    is.dis.total = form %in% 'AS78' & var_label %in% 'total',
    # grandparent total
    is.gpa.total = form %in% 'AS32' & var_label %in% 'total',
    # housing tenure total
    is.ten.total = form %in% 'ASTM' & var_label %in% 'total',
    # renter total
    is.ren.total = form %in% 'ASVH' & var_label %in% 'total',
    # owner total
    is.own.total = form %in% 'ASWA' & var_label %in% 'total',
    # is veteran
    is.vet = form %in% 'ASSH' & grepl('vet', var_label) & !grepl('\\:', var_label),
    # sex subtotals (for various universes lol)
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    is.dissex = form %in% 'AS78' & grepl('(fe)?male$', var_label),
    is.marsex = form %in% 'ASPP' & grepl('(fe)?male$', var_label),
    is.vetsex = form %in% 'ASSH' & grepl('(fe)?male$', var_label),
    # age-sex
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    is.sex.disage = form %in% 'AS78' & grepl('\\d', var_label) & !grepl('disab', var_label),
    is.sex.vetage = form %in% 'ASSH' & grepl('\\d', var_label) & !grepl('vet', var_label),
    # sex-marital status
    is.sex.mar = form %in% 'ASPP' & grepl('male\\:[^\\:]*$', var_label),
    # sex-veteran status
    is.sex.vet = form %in% 'ASSH' & grepl('(fe)?male\\:\\s(non)?veteran', var_label),
    # sex-age-disability
    is.sex.age.dis = form %in% 'AS78' & grepl('disab', var_label),
    # sex-age-veteran
    is.sex.age.vet = form %in% 'ASSH' & grepl('vet', var_label) & grepl('\\d', var_label),
    # race-ethnicity
    is.rac = form %in% paste0('ASN', 4:9),
    is.hsp = form %in% 'ASOB',
    # household type
    is.fam.hou = form %in% 'ASOW' & grepl('households?$', var_label),
    # household type, more detail
    is.fam.hou.type = form %in% 'ASOW' & grepl('households?\\:', var_label),
    # # grandparents present/absent
    # is.gpa.hh  = form %in% 'AS4J' & grepl('grandp', var_label),
    # grandparent responsibility
    is.gpa.rsp = form %in% 'AS32' & grepl('responsible', var_label),
    # housing tenure
    is.ten = form %in% 'ASTM' & grepl('occupied$', var_label),
    # renter percentage
    is.ren.pct = form %in% 'ASVH' & !grepl('total', var_label), # grepl('percent', var_label),
    # renter percentage (NA)
    # is.ren.pct.nc = form %in% 'ASVH' & grepl('not\\scomput', var_label),
    # renter other/NA?
    # householder by age
    is.own.age = form %in% 'ASWA' & grepl('years', var_label) & !grepl('\\:', var_label),
    # householder by age by percent
    is.own.age.pct = form %in% 'ASWA' & grepl('\\:', var_label), # grepl('\\:\\s\\d', var_label),
    # householder by age by percent (NA)
    # is.own.age.pct.nc = form %in% 'ASWA' & grepl('not\\scomput', var_label),
    # group quarters
    is.gquart = form %in% 'ATFB',
    # ancestry
    is.anc = grepl('ancestry', var_desc)
  ) %>%
  # Arrange rows:
  arrange(
    ### Arranging among group types:
    # Arrange totals
    desc(is.total), desc(is.vet.total), desc(is.mar.total), desc(is.dis.total), desc(is.gpa.total),
    desc(is.hou.total), desc(is.ten.total), desc(is.ren.total), desc(is.own.total),
    # Arrange group quarters
    desc(is.gquart),
    # Arrange race and hispanic ethnicity
    desc(is.rac), desc(is.hsp),
    # Arrange age-sex (whole universe)
    desc(is.sex), desc(is.sex.age),
    # Arrange veterans variables
    desc(is.vetsex), desc(is.vet), desc(is.sex.vet), desc(is.sex.vetage), desc(is.sex.age.vet),
    # Arrange marriage variables
    desc(is.marsex), desc(is.sex.mar),
    # Arrange disability variables
    desc(is.dissex), desc(is.sex.disage), desc(is.sex.age.dis),
    # Arrange housing variables
    desc(is.fam.hou), desc(is.fam.hou.type),
    # Arrange grandparents
    # desc(is.gpa.hh), desc(is.gpa.rsp),
    # Keeping this in backwards order to match X
    desc(is.gpa.rsp),
    # Arrange housing tenure
    desc(is.ten),
    # Arrange renter cost column
    desc(is.ren.pct), # desc(is.ren.pct.nc),
    # Arrange ownership cost
    desc(is.own.age), desc(is.own.age.pct), # desc(is.own.age.pct.nc),
    # Arrange ancestry variables
    desc(is.anc),
    ### Arrange within group types:
    var_label, tract
  ) %>%
  # distinct(form, var_label, var_desc, .keep_all = TRUE) %>% # slice(101:102)
  # select(where(is.logical)) %>% apply(1, sum) %>% (\(x) which(x != 1))
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

### Get age bins

ages.fine = acs.tab %>%
  filter(form %in% 'ASNQ', grepl('\\d', var_label)) %>%
  distinct(var_label) %>%
  mutate(age = gsub('(fe)?male\\:\\s(\\d{2}).*', '\\2', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()
# that's a lot

ages.disb = acs.tab %>%
  filter(form %in% 'AS78', grepl('\\d', var_label)) %>%
  distinct(var_label) %>%
  mutate(age = gsub('(fe)?male\\:\\s(\\d{2}).*', '\\2', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

ages.vets = acs.tab %>%
  filter(form %in% 'ASSH', grepl('\\d', var_label)) %>%
  distinct(var_label) %>%
  mutate(age = gsub('(fe)?male\\:\\s(\\d{2}).*', '\\2', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

ages.owns = y.tab %>%
  filter(vartype %in% 'is.own.age') %>%
  distinct(var_label) %>%
  mutate(age = gsub('householder\\s(\\d{2}).*', '\\1', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

pcts.ren = y.tab %>% 
  filter(vartype %in% 'is.ren.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('^(\\d{2}).+', '\\1', var_label)) %>%
  filter(grepl('^\\d{2}$', pct)) %>%
  distinct(pct) %>% arrange() %>% pull()

pcts.own = y.tab %>% 
  filter(vartype %in% 'is.own.age.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('.+\\:\\s(\\d{2}).+', '\\1', var_label)) %>%
  filter(grepl('^\\d{2}$', pct)) %>%
  distinct(pct) %>% arrange() %>% pull()

pcts.own
pcts.ren
ages.owns
ages.disb


### Format X

x.tab = pums %>%
  mutate(
    # Recode ages and sex
    age.fine = cut(AGE, c(as.numeric(ages.fine), Inf), right = FALSE, labels = ages.fine),
    age.disb = cut(AGE, c(as.numeric(ages.disb), Inf), right = FALSE, labels = ages.disb),
    age.vets = cut(AGE, c(as.numeric(ages.vets), Inf), right = FALSE, labels = ages.vets),
    age.owns = cut(AGE, c(as.numeric(ages.owns), Inf), right = FALSE, labesl = ages.owns),
    sex = ifelse(SEX > 1, 'female', 'male'),
    # Get disability
    has.diff = ifelse(
      (DIFFEYE > 1) | (DIFFHEAR > 1) | (DIFFMOB > 1) | (DIFFPHYS > 1) | (DIFFCARE > 1) | (DIFFREM > 1),
      'with.disb',
      'no.disb'
    ),
    # Change race variables to 0/1
    across(starts_with('RAC'), ~ . - 1),
    # Hispanic ethnicity variable
    hispanic = ifelse(HISPAN > 0, 'hisp', 'nonhisp'),
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
    # housing tenure
    tenure = case_match(
      OWNERSHP,
      1 ~ 'owner',
      2 ~ 'renter',
      .default = NA
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
    own.pct = cut(own.pct.num, breaks = c(as.numeric(pcts.own), Inf), right = FALSE, labels = pcts.own),
    # Get 'not computed' housing costs for owners
    own.pct = ifelse(
      (OWNERSHP %in% 1 & !(HHINCOME > 0 & OWNCOST > 0)),
      'NC',
      as.character(own.pct)
    ),
    ren.pct = cut(ren.pct.num, breaks = c(as.numeric(pcts.ren), Inf), right = FALSE, labels = pcts.ren),
    # Get 'not computed' housing costs for renters
    ren.pct = ifelse(
      (OWNERSHP %in% 2 & !(HHINCOME > 0 & RENTGRS > 0)),
      'NC',
      as.character(ren.pct)
    ),
    # Flag for veterans universe (civilian and 18+)
    in.vet.univ = as.numeric(!(OCC %in% 9800:9850) & (AGE > 17)),
    # Flag for disability universe (civilian noninstitutionalized)
    in.dis.univ = as.numeric(!(OCC %in% 9800:9850) & !(GQ %in% 3)),
    in.mar.univ = as.numeric(AGE > 14),
    # in.gq   = (GQ %in% 3:4),
    in.inst = GQ %in% 3,
    # grandparent = ifelse(GCHOUSE %in% 2, 'is.grandparent', 'is.not.grandparent'),
    g.raising.g = case_when(
      (GCHOUSE %in% 2) & GCRESPON %in% 2 ~ 'responsible',
      (GCHOUSE %in% 2) & GCRESPON %in% 1 ~ 'not.responsible',
      .default = NA
    )
  ) %>%
  # Add household stats
  group_by(CBSERIAL) %>%
  mutate(
    lives.alone = ifelse(n() < 2, 'liv.alone', 'not.liv.alone'),
    family.hous = ifelse(any(RELATE[PERNUM > 1] <= 10), 'fam.hou', 'nonfam.hou'),
    # multigen    = case_(any(GCHOUSE > 1) & n() > 1, 'is.multigen', 'is.not.multigen')
  ) %>%
  ungroup() %>%
  # Now, get ancestry:
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
    )
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
  # Get rid of some unnecessary columns
  select(-c(starts_with('DIFF', ignore.case = FALSE)))

# Doing the matrix formation in two steps
# (because otherwise there's a memory overflow)

x.mat.vet.hous = x.tab %>%
  select(-c(ancestry, n.ancestry)) %>%
  # Get one row per individual only
  distinct(CBSERIAL, PERNUM, .keep_all = TRUE) %>%
  # Arranging rows here (which will reorder columns below)
  complete(
    sex, nesting(age.fine, age.vets, age.disb), hispanic,
    mar, veteran, has.diff, nesting(family.hous, lives.alone),
    g.raising.g,
    nesting(tenure, age.owns), nesting(own.pct, ren.pct)
  ) %>%
  arrange(
    sex, age.fine, age.vets, age.disb, hispanic, 
    mar, veteran, has.diff, family.hous, 
    g.raising.g,
    tenure, ren.pct, age.owns, own.pct
  ) %>%
  # Start pivots
  mutate(
    total = 1,
    ### Universe totals
    # Total for veterans universe
    total.vet = in.vet.univ,
    # Total for marriage universe
    total.mar = in.mar.univ,
    # Total for disability universe
    total.dis = in.dis.univ,
    # Total number of grandparents
    total.grp = as.numeric(GCHOUSE > 1),
    # Household total (only count `1` for household head)
    total.hou = as.numeric(PERNUM < 2 & !(GQ %in% 3:4)),
    # Tenure total (people in occupied housing units)
    total.ten = as.numeric(!(GQ %in% 3:4)),
    # Renter total (renter-occupied housing units)
    total.ren = as.numeric(OWNERSHP %in% 2 & PERNUM < 2),
    # Owner total (owner-occupied housing units)
    total.own = as.numeric(OWNERSHP %in% 1 & PERNUM < 2),
    ### Group quarters (not pivoted)
    in.gq = as.numeric(GQ %in% 3:4),
    ### Race columns
    rac.amind = RACAMIND,
    rac.asian = RACASIAN,
    rac.black = RACBLK,
    rac.pacis = RACPACIS,
    rac.other = RACOTHER,
    rac.white = RACWHT,
    ### Hispanic ethnicity
    hsp.hisp = hispanic,
    hsp.ones = 1,
    ### Sex-age (whole universe)
    # Sex
    sex.cols = sex,
    sex.ones = 1,
    # Sex-age
    sex.age.sex = sex,
    sex.age.age = age.fine,
    sex.age.ones = 1,
    ### Veterans
    # Sex for veterans universe (is.vetsex)
    sex.for.vet.cols = sex,
    sex.for.vet.ones = total.vet,
    # Veteran status (is.vet)
    vet.cols = veteran,
    vet.ones = total.vet,
    # Veterans-sex (is.vet.sex)
    sex.vet.sex = sex,
    sex.vet.vet = veteran,
    sex.vet.one = total.vet,
    # Sex-age (veterans universe)
    sex.vetage.sex = sex,
    sex.vetage.age = age.vets,
    sex.vetage.one = total.vet,
    # Sex-age-vet status
    sex.age.vet.sex = sex,
    sex.age.vet.age = age.vets,
    sex.age.vet.vet = veteran,
    sex.age.vet.one = total.vet,
    ### Marital status
    # Sex for marriage universe
    sex.for.mar.cols = sex,
    sex.for.mar.ones = total.mar,
    # Sex-marriage status (is.sex.mar)
    sex.mar.sex = sex,
    sex.mar.mar = mar,
    sex.mar.one = total.mar,
    ### Disability status
    # Sex for disability universe (is.dissex)
    sex.for.dis.cols = sex,
    sex.for.dis.ones = total.dis,
    # Sex-age (is.sex.disage)
    sex.disage.sex = sex,
    sex.disage.age = age.disb,
    sex.disage.one = total.dis,
    # Sex-age-disability status (is.sex.age.dis)
    sex.age.dis.sex = sex,
    sex.age.dis.age = age.disb,
    sex.age.dis.dis = has.diff,
    sex.age.dis.one = total.dis,
    ### Household type
    # Family household type
    hou.cols = family.hous,
    hou.ones = total.hou,
    # Household type x living alone
    hou.alo.hou = family.hous,
    hou.alo.alo = lives.alone,
    hou.alo.one = total.hou,
    # ### Household with/without grandparent
    # gpa.cols = is.grpar.hh,
    # gpa.ones = total.hou,
    ### Grandparent-grandchild responsibility
    gpa.cols = g.raising.g,
    gpa.ones = total.grp,
    ### Tenure
    ten.cols = tenure,
    ten.ones = total.ten,
    ### Renter cost percentages
    ren.cols = paste0('ren.', ren.pct),
    ren.ones = total.ren,
    ### Owner cost table
    # Owner ages
    own.age.cols = paste0('own.', age.owns),
    own.age.ones = total.own,
    # Owner age by cost
    own.age.age = paste0('own.', age.owns),
    own.age.own = paste0(own.pct),
    own.age.one = total.own,
  ) %>%
  ######### PIVOT ############
  # Pivot out hispanic total
  pivot_wider(names_from = hsp.hisp, values_from = hsp.ones, values_fill = 0) %>%
  ### Sex-age table (whole universe)
  # Pivot out whole-universe sex
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  # Sex-age totals (whole universe)
  pivot_wider(names_from = c(sex.age.sex, sex.age.age), values_from = sex.age.ones, values_fill = 0) %>%
  ### Veteran table
  # Pivot out sex totals (vet universe)
  pivot_wider(names_from = sex.for.vet.cols, values_from = sex.for.vet.ones, values_fill = 0, names_prefix = 'v.') %>%
  # Pivot out veteran totals
  pivot_wider(names_from = vet.cols, values_from = vet.ones, values_fill = 0) %>%
  # Pivot out veteran-sex totals
  pivot_wider(names_from = c(sex.vet.sex, sex.vet.vet), values_from = sex.vet.one, values_fill = 0) %>%
  # Sex-age totals (vet universe)
  pivot_wider(
    names_from = c(sex.vetage.sex, sex.vetage.age), values_from = sex.vetage.one, 
    values_fill = 0, names_prefix = 'v.'
  ) %>%
  select(-c(contains('_NA', ignore.case = FALSE), contains('NA_', ignore.case = FALSE))) %>%
  # Pivot out sex-age-veteran status counts
  pivot_wider(
    names_from = c(sex.age.vet.sex, sex.age.vet.age, sex.age.vet.vet),
    values_from = sex.age.vet.one, values_fill = 0
  ) %>%
  # Get rid of extraneous columns
  select(-c(contains('_NA', ignore.case = FALSE), contains('NA_', ignore.case = FALSE))) %>%
  ### Marital status table
  # Pivot out sex totals (marriage universe)
  pivot_wider(names_from = sex.for.mar.cols, values_from = sex.for.mar.ones, values_fill = 0, names_prefix = 'm.') %>%
  # Sex-marriage status
  pivot_wider(names_from = c(sex.mar.sex, sex.mar.mar), values_from = sex.mar.one, values_fill = 0) %>%
  # Get rid of extraneous columns
  select(-c(contains('_NA', ignore.case = FALSE), contains('NA_', ignore.case = FALSE))) %>%
  ### Disability status table
  # Pivot out sex totals for disability universe
  pivot_wider(names_from = sex.for.dis.cols, values_from = sex.for.dis.ones, values_fill = 0, names_prefix = 'd.') %>%
  # Sex-age totals (disability universe)
  pivot_wider(
    names_from = c(sex.disage.sex, sex.disage.age), values_from = sex.disage.one, 
    values_fill = 0, names_prefix = 'd.'
  ) %>%
  # Pivot out sex-age-disability counts
  pivot_wider(
    names_from = c(sex.age.dis.sex, sex.age.dis.age, sex.age.dis.dis),
    values_from = sex.age.dis.one, values_fill = 0
  ) %>%
  # Get rid of extraneous columns
  select(-c(contains('_NA', ignore.case = FALSE), contains('NA_', ignore.case = FALSE))) %>%
  ### Household type
  # Housing type (family/nonfamily)
  pivot_wider(names_from = hou.cols, values_from = hou.ones, values_fill = 0) %>%
  # Housing x living alone
  pivot_wider(names_from = c(hou.alo.hou, hou.alo.alo), values_from = hou.alo.one, values_fill = 0) %>%
  # Remove 'family-living alone'
  select(-matches('^fam.+liv')) %>%
  # ### Pivot out grandparents
  pivot_wider(names_from = gpa.cols, values_from = gpa.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  ### Pivot out tenure 
  pivot_wider(names_from = ten.cols, values_from = ten.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  ### Pivot out rental cost percentages
  pivot_wider(names_from = ren.cols, values_from = ren.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  # Pivot out homeowner-age columns
  pivot_wider(names_from = own.age.cols, values_from = own.age.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  # Pivot out homeowner-age-cost columns
  pivot_wider(names_from = c(own.age.age, own.age.own), values_from = own.age.one, values_fill = 0) %>%
  ### Get rid of extraneous columns
  select(-c(contains('_NA', ignore.case = FALSE), contains('NA_', ignore.case = FALSE))) %>%
  select(-matches('^NA$')) %>%
  ### Final processing steps
  # Filter out dummy records made from complete()
  filter(!is.na(CBSERIAL)) %>%
  # Sort rows by serial number
  # arrange(CBSERIAL, PERNUM) %>%
  select(CBSERIAL, PERNUM, PERWT, PUMA, total:last_col())

x.mat.ancestry = x.tab %>%
  arrange(ancestry) %>%
  mutate(total = 1) %>%
  pivot_wider(names_from = ancestry, values_from = n.ancestry, values_fill = 0) %>%
  select(CBSERIAL, PERNUM, PERWT, PUMA, total:last_col())
  

# Combine with a merge
x.mat = merge(x.mat.vet.hous, x.mat.ancestry)

nrow(x.mat)
ncol(x.mat)
head(x.mat)


# Make duplicated synthetic records
x.mat.synthetic = merge(
  synthetic.records %>% select(CBSERIAL, PERNUM, PUMA),
  x.mat %>% select(-PUMA)
) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1, PERWT = 1)


x.mat = rbind(x.mat, x.mat.synthetic) %>%
  arrange(CBSERIAL, PERNUM, PUMA)

nrow(x.mat) # aw yea

ncol(x.mat) - 4
y.tab %>% count(tract)

slicey = 1:50
slicey = slicey + 50

# slicey = 1:215

data.frame(
  x = names(x.mat)[-(1:4)][slicey],
  y = y.tab %>% filter(tract %in% 101) %>% select(var_label, vartype) %>% slice(slicey)
)


# ==============================================================
# Press go button!!!

### Run downscale
fit.list = map2(
  .x = split(x.mat, ~ PUMA),
  .y = split(y.tab, ~ puma),
  function(x, y) {
    
    # # # define variables
    # total universe size
    N = y %>% filter(vartype %in% 'is.total') %>% pull(E) %>% sum()
    # PUMS sample size
    n = nrow(x)
    # Number of counties
    J = y %>% distinct(tract) %>% nrow()
    
    # # # Prepare response and margins of error
    # Response
    Y = y$E / N
    # Error matrix
    v = (y$M * n / (N^2)) %>%
      .sparseDiagonal(n = length(.)) %>% 
      as('generalMatrix')
    
    # # # Prepare PUMS data
    # Prepare X matrix
    # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED
    X = kronecker(t(as.matrix(x[, -(1:4)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    # CHECK HHWT vs. PERWT
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, opt = list(tr_rad = 2)))
    
  }
)


# ==============================================================
# Evaluate fits

### Extract constraints
con.list = map2_df(
  .x = fit.list,
  .y = split(y.tab, ~ puma),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'is.total']))
) 

# Okay let's see what happens.

con.list %>% 
  count(puma, in.mar = abs(E - pred) < M) %>%
  pivot_wider(names_from = in.mar, values_from = n, names_prefix = 'in.mar', values_fill = 0)
# still quite bad!

con.list %>% 
  count(vartype, in.mar = abs(E - pred) < M) %>%
  pivot_wider(names_from = in.mar, values_from = n, names_prefix = 'in.mar', values_fill = 0) %>%
  print(n = nrow(.))

con.list %>% 
  filter(abs(E - pred) > M) %>%
  count(puma, vartype) %>%
  pivot_wider(names_from = puma, values_from = n, names_prefix = 'p', values_fill = 0) %>%
  print(n = nrow(.))

con.list %>%
  filter(abs(E - pred) > M) %>%
  ggplot(aes(x = E, y = pred, fill = vartype)) +
  geom_point(shape = 21, size = 3)

# y.puma.totals = y.tab %>%
#   group_by(tract) %>%
#   mutate(cons.no = 1:n()) %>%
#   group_by(puma, cons.no, form, var_label, vartype) %>% 
#   summarise(e = sum(E))
# 
# x.totals = x.mat %>%
#   mutate(across(-c(CBSERIAL, PERNUM, PERWT, PUMA), ~ PERWT * .)) %>%
#   group_by(PUMA) %>%
#   summarise(across(-c(CBSERIAL, PERNUM, PERWT), sum)) %>%
#   ungroup()
# 
# x.totals
# 
# compare.totals = cbind(
#   y.puma.totals %>% arrange(cons.no) %>% pivot_wider(names_from = puma, values_from = e, names_prefix = 'py.'),
#   x.totals %>% pivot_longer(-PUMA, names_to = 'x.cons', values_to = 'val') %>% pivot_wider(names_from = PUMA, values_from = val, names_prefix = 'px.')
# ) %>%
#   pivot_longer(matches('^p[xy]'), names_to = 'pp', values_to = 'val') %>%
#   separate_wider_delim(pp, names = c('source', 'puma'), delim = '.') %>%
#   pivot_wider(names_from = source, values_from = val)
# 
# compare.totals %>%
#   ggplot(aes(x = px, y = py, colour = vartype)) +
#   geom_segment(aes(xend = py, yend = py)) +
#   geom_point(size = 3) # +
# # coord_fixed() +
# # facet_wrap(~ vartype, scales = 'free')
# # okay I'm not sure what's going on here...
# 
# compare.totals %>% 
#   arrange(desc(abs(px - py) / (px + py)))
# 
# compare.totals %>%
#   filter(vartype %in% 'is.fam.hou.type') %>%
#   ggplot(aes(x = px, y = py, colour = var_label)) +
#   annotate('segment', x = 1, y = 1, xend = 40000, yend = 40000, linetype = 2, colour = 'gray55') +
#   geom_point(size = 3) +
#   theme(legend.position = 'top')
# 
# compare.totals %>% 
#   filter(grepl('hou', vartype)) %>% 
#   ggplot(aes(x = px, y = py)) + 
#   geom_segment(aes(xend = py, yend = py)) +
#   geom_point(aes(colour = vartype), size = 3) +
#   labs(x = 'from PUMS', y = 'from ACS tables')
# 
# # over-counting non-family households where the householder is living alone...
# # maybe because GQs are being counted?
# 
# compare.totals %>% 
#   filter(grepl('is.fam.hou.type', vartype)) %>% 
#   ggplot(aes(x = px, y = py)) + 
#   geom_segment(aes(xend = py, yend = py)) +
#   geom_point(aes(colour = var_label), size = 3) +
#   labs(x = 'from PUMS', y = 'from ACS tables')
# # living alone counts still look like undercounts
# 
# compare.totals %>% 
#   filter(grepl('is.fam.hou$', vartype)) %>% 
#   ggplot(aes(x = px, y = py)) + 
#   geom_segment(aes(xend = py, yend = py)) +
#   geom_point(aes(colour = var_label), size = 3) +
#   labs(x = 'from PUMS', y = 'from ACS tables')
# # non-family households are being way over-counted
# # family households are being way under-counted
# 
# compare.totals %>% 
#   filter(grepl('is.grandp', vartype)) %>% 
#   ggplot(aes(x = px, y = py)) + 
#   geom_segment(aes(xend = py, yend = py)) +
#   geom_point(aes(colour = var_label), size = 3) +
#   labs(x = 'from PUMS', y = 'from ACS tables')
# 
# # looks like we're off for households without grandparents...
# 
# # See this link (p. 88-89) for definitions:
# # https://www2.census.gov/programs-surveys/acs/tech_docs/subject_definitions/2023_ACSSubjectDefinitions.pdf
# # And this code:
# x.tab %>% 
#   distinct(CBSERIAL, PERNUM, .keep_all = TRUE) %>% 
#   filter(family.hous %in% 'family.hou' | lives.alone %in% 'not.liv.alone') %>% 
#   arrange(CBSERIAL, PERNUM) %>% 
#   select(CBSERIAL, PERNUM, FAMUNIT, FAMSIZE, SEX, AGE, MARST, GQ, family.hous)
# # figure out what is going on such that a lot of non-family households with
# # people living together are getting coded as family households (should be a few
# # thousand)
# 
# x.tab %>% 
#   distinct(CBSERIAL, PERNUM, .keep_all = TRUE) %>% 
#   filter(family.hous %in% 'family.hou' | lives.alone %in% 'not.liv.alone') %>% 
#   arrange(CBSERIAL, PERNUM) %>% 
#   select(CBSERIAL, PERNUM, FAMUNIT, FAMSIZE, SEX, AGE, MARST, GQ, family.hous) %>%
#   group_by(CBSERIAL) %>%
#   filter(sum(FAMUNIT %in% 1) < 2)
# 
# # okay there is one (ONE) issue here...
# # GQ for one PUMA (5102) being under-counted in the PUMS
# # I wonder if it has to do with the weird GQ == 5 place...


# ==============================================================
# Assign weights and export

allo.weights = map2(
  .x = split(x.mat, ~ PUMA),
  .y = fit.list,
  .f =  function(x.data, mod.fit) {
    # x.data: ACS matrix used for extraction
    # mod.fit: model object (i.e. what is returned pmedm)
    
    matr = matrix(
      data = mod.fit$p, nrow = nrow(x.data), byrow = TRUE,
      # NOTE the year column here... maybe it would be a good idea to make a
      # flexible 'id' column
      dimnames = list(with(x.data, paste(CBSERIAL, PERNUM, sep = '_')), NULL)
    )
    
    return(matr)
  }
)

allo.weights = map2(
  .x = allo.weights,
  .y = split(y.tab, ~ puma),
  function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(tract) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$puma[1], tracts, sep = '_')
    
    # Normalize by population size
    pop.size = y.data %>% filter(vartype %in% 'is.total') %>% pull(E) %>% sum()
    p.matrix = p.matrix * pop.size
    
    # Return matrix
    return(p.matrix)
  }
)

allo.weights = allo.weights %>%
  lapply(
    FUN = function(m) {
      data.frame(m) %>%
        mutate(id = row.names(.)) %>%
        pivot_longer(-id, names_to = 'tract', values_to = 'alloc')
    }
  ) %>%
  do.call(what = rbind) %>%
  mutate(tract = gsub('X', '', tract)) %>%
  separate_wider_delim(id, delim = '_', names = c('cbserial', 'pernum')) %>%
  mutate(across(c(cbserial, pernum), as.numeric)) 

write.csv(allo.weights, row.names = FALSE, file = 'multnomah/03_downscale_out/vets-and-housing_all_weights.csv')
