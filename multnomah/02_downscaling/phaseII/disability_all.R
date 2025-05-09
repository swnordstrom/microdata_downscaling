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
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00051.xml')

# Read in PUMS and subset to MultCo
pums.raw = pums.raw.ddi %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Read in tabular data
acs.tab.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0032_csv.zip', file_select = 1) %>%
  filter(grepl('Multno', COUNTY))
acs.tab.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0032_csv.zip', file_select = 2) %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')

# Read in synthetic records
synthetic.records = read.csv('multnomah/02_downscaling/reld_ancestry_synthetic_records.csv') %>%
  mutate(
    CBSERIAL = -1 * (1:nrow(.)),
    PERNUM = 1,
    PERWT = 1
  )

# ==============================================================
# Combine and massage data (before formatting for data)

acs.tab = merge(
  acs.tab.raw1 %>% select(TRACTA, matches('[EM]\\d{3}$')),
  acs.tab.raw2 %>% select(TRACTA, matches('[EM]\\d{3}$')),
  by = 'TRACTA'
) %>%
  mutate(tract = as.numeric(TRACTA)) %>%
  select(-TRACTA) %>%
  merge(puma.tract %>% filter(puma > 5000) %>% select(tract, puma))

head(acs.tab)

acs.tab = acs.tab %>%
  pivot_longer(-c(puma, tract), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    rbind(
      ipums_var_info(acs.tab.raw1, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(acs.tab.raw2, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
    )
  ) %>%
  mutate(var_label = sub('^[^\\:]+\\:\\s', '', tolower(var_label))) %>%
  separate_wider_position(var_name, widths = c(form = 4, em = 1, 3)) %>%
  pivot_wider(names_from = em, values_from = var_value) %>%
  mutate(
    var_desc = gsub('^table\\s[A-Za-z0-9]{4}\\:\\s', '', tolower(var_desc)),
    var_desc = gsub('\\)$', '', var_desc)
  ) %>%
  separate_wider_delim(var_desc, delim = ' (universe: ', names = c('var_desc', 'univ')) %>%
  # Change age labels ('Under' to '00 to', and add preceding zero where needed)
  mutate(
    var_label = gsub('under', '00 to', var_label),
    var_label = gsub('\\s(\\d{1})\\s', ' 0\\1 ', var_label)
  )
  
# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, 
       STRATA, RACE, RACED, HISPAND, ANCESTR1D, ANCESTR2D, DIFFSENS
    )
  ) %>%
  mutate(PUMA = PUMA + ifelse(PUMA < 5000, (5100-1300), 0))

nrow(pums)
table(pums$PUMA)

# Format the ancestry table
ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) 

# something in here needs fixing - tract labels (removing the period - keep as character?)

# ==============================================================
# Now start formatting data

# all.vars = acs.tab %>% distinct(form, var_label, var_desc, univ)

y.tab = acs.tab %>%
  # Filter out some cases
  filter(
    # Total in Hispanic/Latino (ASOB) table is a duplicate
    !(var_label %in% 'total' & form %in% 'ASOB'),
    # Total noninstitutionalized civilian population - 
    # get rid of universe grand and sex total duplicates (all ages and ages 5+)
    !(!grepl('diff', var_label) & form %in% paste0('AS8', c('J', 'L', 'M')))
  ) %>%
  # Classify 
  mutate(
    # Get totals (tract-level total is in ASNQ)
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    # Whole disability universe (civilian and noninstitutionalized)
    is.total.disb.univ = form %in% 'AS8I' & var_label %in% 'total',
    # Disability universe 5+
    is.total.disb.univ.5p = form %in% 'AS8K' & var_label %in% 'total',
    # Disability universe 18+
    is.total.disb.univ.18p = form %in% 'AS8N' & var_label %in% 'total',
    # Total sex
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    # Total sex in disb universes
    is.sex.disb.univ = form %in% 'AS8I' & grepl('(fe)?male$', var_label),
    is.sex.disb.univ.5p = form %in% 'AS8K' & grepl('(fe)?male$', var_label),
    is.sex.disb.univ.18p = form %in% 'AS8N' & grepl('(fe)?male$', var_label),
    # Sex-age (whole population)
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    # Sex-age disability universes
    is.sex.age.disb.univ = form %in% 'AS8I' & grepl('\\d', var_label) & !grepl('diff', var_label),
    is.sex.age.disb.univ.5p = form %in% 'AS8K' & grepl('\\d', var_label) & !grepl('diff', var_label),
    is.sex.age.disb.univ.18p = form %in% 'AS8N' & grepl('\\d', var_label) & !grepl('diff', var_label),
    # Sex-age-disability combinations (one for each disability - will make sorting easier)
    # Hearing
    is.sex.age.diff.hear = form %in% 'AS8I' & grepl('diff', var_label),
    # Vision
    is.sex.age.diff.visn = form %in% 'AS8J' & grepl('diff', var_label),
    # Cognitive
    is.sex.age.diff.cogn = form %in% 'AS8K' & grepl('diff', var_label),
    # Ambulatory
    is.sex.age.diff.ambu = form %in% 'AS8L' & grepl('diff', var_label),
    # Self-care
    is.sex.age.diff.care = form %in% 'AS8M' & grepl('diff', var_label),
    # Independent living
    is.sex.age.diff.livn = form %in% 'AS8N' & grepl('diff', var_label),
    # Race totals
    is.race = form %in% paste0('ASN', 4:9),
    # Hispanic ethnicity
    is.hisp = form %in% 'ASOB'
  ) %>%
  # Change the var_label to universe for the race (var_label is currently 'total')
  mutate(var_label = ifelse(is.race, univ, var_label)) %>%
  # Now, arrange rows
  arrange(
    # Grand total + disability universe total
    desc(is.total), desc(is.total.disb.univ), desc(is.total.disb.univ.5p), desc(is.total.disb.univ.18p),
    # Race and Hispanic ethnicity
    desc(is.race), desc(is.hisp),
    # Sex totals (grand and disability universe)
    desc(is.sex), desc(is.sex.disb.univ), desc(is.sex.disb.univ.5p), desc(is.sex.disb.univ.18p),
    # Sex-age (grand and disability universe)
    desc(is.sex.age), desc(is.sex.age.disb.univ), 
    desc(is.sex.age.disb.univ.5p), desc(is.sex.age.disb.univ.18p),
    # Sex-age-disability combos
    desc(is.sex.age.diff.hear), desc(is.sex.age.diff.visn), desc(is.sex.age.diff.cogn),
    desc(is.sex.age.diff.ambu), desc(is.sex.age.diff.care), desc(is.sex.age.diff.livn),
    # Now sort within constraints
    var_label,
    # sort geographies
    tract
  ) %>%
  # Pivot the constraint label columns
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

y.tab = rbind(
  y.tab,
  ancestry.tab %>%
    mutate(
      form = NA,
      var_desc = 'ancestry',
      univ = 'total population',
      vartype = 'is.ancestry'
    ) %>%
    arrange(var_label, tract) %>%
    select(names(y.tab))
)

head(y.tab)
tail(y.tab)

# y.tab %>% select(where(is.logical)) %>% apply(1, sum) %>% table()

# y.tab %>% filter(tract %in% 7500) %>% nrow()

# Extract age groupings

# Fine-grained age groupings
age.fine = y.tab %>% 
  filter(vartype %in% 'is.sex.age') %>% distinct(var_label) %>%
  mutate(age = sub('^[^\\:]+\\:\\s(\\d{2}).+', '\\1', var_label)) %>% 
  distinct(age) %>% pull()

# Age for disabilities
age.disb = y.tab %>% 
  filter(vartype %in% 'is.sex.age.disb.univ') %>% distinct(var_label) %>% 
  mutate(age = sub('^[^\\:]+\\:\\s(\\d{2}).+', '\\1', var_label)) %>% 
  distinct(age) %>% pull()

# Now... let's get x ready

# # Grand total + disability universe total
# desc(is.total), desc(is.total.disb.univ), desc(is.total.disb.univ.5p), desc(is.total.disb.univ.18p),
# # Sex totals (grand and disability universe)
# desc(is.sex), desc(is.sex.disb.univ), desc(is.sex.disb.univ.5p), desc(is.sex.disb.univ.18p),
# # Sex-age (grand and disability universe)
# desc(is.sex.age), desc(is.sex.age.disb.univ), 
# desc(is.sex.age.disb.univ.5p), desc(is.sex.age.disb.univ.18p),
# # Sex-age-disability combos
# desc(is.sex.age.diff.hear), desc(is.sex.age.diff.visn), desc(is.sex.age.diff.cogn),
# desc(is.sex.age.diff.ambu), desc(is.sex.age.diff.care), desc(is.sex.age.diff.livn),
# # Race and Hispanic ethnicity
# desc(is.race), desc(is.hisp),

# First step is to get disability info
x.tab = pums %>%
  # (we won't worry about ancestry for now)
  select(-c(ANCESTR1, ANCESTR2)) %>%
  # Add and modify columns
  mutate(
    # change sex to character (for sorting)
    sex = as.character(as_factor(SEX)),
    # get age bins
    age.fine = cut(AGE, c(as.numeric(age.fine), Inf), right = FALSE, labels = age.fine),
    age.disb = cut(AGE, c(as.numeric(age.disb), Inf), right = FALSE, labels = age.disb),
    # in civilian noninstitutionalized population (disability universe)
    in.disb.univ = !(OCC %in% 9800:9850) & !(GQ %in% 3),
    # change race to 0/1
    across(starts_with('RAC'), ~ . - 1),
    # set individuals under 18 with independent living disabilities equal to NA
    # (not in universe of ACS table)
    DIFFMOB = ifelse(AGE < 18, 0, DIFFMOB),
    # recode disability columns
    across(
      starts_with('DIFF'), 
      ~ case_match(
        .x,
        0 ~ NA,
        1 ~ 'no.',
        2 ~ 'with.'
      )
    ),
    # recode hispanic ethnicity here
    HISPAN = ifelse(HISPAN %in% 1:4, 'hisp', 'not.hisp'),
  )

x.disb.mat = x.tab %>%
  ### Arrange column names
  # First needs a complete()
  complete(
    RACAMIND, RACASIAN, RACBLK, RACPACIS, RACOTHER, RACWHT, HISPAN,
    sex, age.fine,
    nesting(age.disb, DIFFHEAR, DIFFEYE, DIFFREM, DIFFPHYS, DIFFCARE, DIFFMOB)
  ) %>%
  arrange(
    HISPAN, sex, age.fine, age.disb,
    DIFFHEAR, DIFFEYE, DIFFREM, DIFFPHYS, DIFFCARE, DIFFMOB,
    RACAMIND, RACASIAN, RACBLK, RACPACIS, RACOTHER, RACWHT, HISPAN,
    CBSERIAL, PERNUM
  ) %>%
  # Start making some columns for pivoting
  mutate(
    # in total
    total = 1,
    # in disability universe (age groups)
    disb.univ = as.numeric(in.disb.univ),
    disb.univ.5p = as.numeric(in.disb.univ & (AGE > 4)),
    disb.univ.18p = as.numeric(in.disb.univ & (AGE > 17)),
    # Race columns
    rac.amind = RACAMIND,
    rac.asian = RACASIAN,
    rac.black = RACBLK,
    rac.nhpac = RACPACIS,
    rac.other = RACOTHER,
    rac.white = RACWHT,
    # Hispanic ethnicity
    hisp.cols = HISPAN,
    hisp.ones = 1,
    # sex (total)
    sex.cols = sex,
    sex.ones = 1,
    # sex (disability universe)
    sex.disb.univ.sex = paste0('cpop', sex),
    sex.disb.univ.ones = disb.univ,
    sex.disb.univ.5p.sex = paste0('cpop5', sex),
    sex.disb.univ.5p.ones = disb.univ.5p,
    sex.disb.univ.18p.sex = paste0('cpop18', sex),
    sex.disb.univ.18p.ones = disb.univ.18p,
    # sex-age (total + disability univ)
    sex.age.sex  = sex,
    sex.age.age  = age.fine,
    sex.age.ones = 1,
    sex.age.disb.univ.sex  = paste0('cpop', sex),
    sex.age.disb.univ.age  = age.disb,
    sex.age.disb.univ.ones = disb.univ,
    sex.age.disb.univ.5p.sex  = paste0('cpop5', sex),
    sex.age.disb.univ.5p.age  = ifelse(AGE > 4, as.character(age.disb), NA),
    sex.age.disb.univ.5p.ones = disb.univ.5p,
    sex.age.disb.univ.18p.sex  = paste0('cpop18', sex),
    sex.age.disb.univ.18p.age  = ifelse(AGE > 17, as.character(age.disb), NA),
    sex.age.disb.univ.18p.ones = disb.univ.18p,
    ## sex-age-disability (hearing, vision, cognitive, ambulatory, self-care, living)
    # Hearing disability
    hear.sex.age.disb.sex  = sex,
    hear.sex.age.disb.age  = age.disb,
    hear.sex.age.disb.disb = paste0(DIFFHEAR, 'hear.disb'),
    hear.sex.age.disb.ones = disb.univ,
    # Vision disability
    visn.sex.age.disb.sex  = sex,
    visn.sex.age.disb.age  = age.disb,
    visn.sex.age.disb.disb = paste0(DIFFEYE, 'visn.disb'),
    visn.sex.age.disb.ones = disb.univ, 
    # Cognitive disability
    cogn.sex.age.disb.sex  = sex,
    cogn.sex.age.disb.age  = age.disb,
    cogn.sex.age.disb.disb = paste0(DIFFREM, 'cogn.disb'),
    cogn.sex.age.disb.ones = disb.univ.5p,
    # Ambulatory disability
    ambu.sex.age.disb.sex  = sex,
    ambu.sex.age.disb.age  = age.disb,
    ambu.sex.age.disb.disb = paste0(DIFFPHYS, 'ambu.disb'),
    ambu.sex.age.disb.ones = disb.univ.5p,
    # Self-care disability
    self.sex.age.disb.sex  = sex,
    self.sex.age.disb.age  = age.disb,
    self.sex.age.disb.disb = paste0(DIFFCARE, 'self.disb'),
    self.sex.age.disb.ones = disb.univ.5p,
    # Living-independently disability
    indp.sex.age.disb.sex  = sex,
    indp.sex.age.disb.age  = age.disb,
    indp.sex.age.disb.disb = paste0(DIFFMOB, 'indp.disb'),
    indp.sex.age.disb.ones = disb.univ.18p
  ) %>%
  ### Pivot time
  # Pivot Hispanic ethnicity (doen before sex this time)
  pivot_wider(names_from = hisp.cols, values_from = hisp.ones, values_fill = 0) %>%
  # Pivot sex
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  # Pivot sex (disability universe)
  pivot_wider(names_from = sex.disb.univ.sex, values_from = sex.disb.univ.ones, values_fill = 0) %>%
  pivot_wider(names_from = sex.disb.univ.5p.sex, values_from = sex.disb.univ.5p.ones, values_fill = 0) %>%
  pivot_wider(names_from = sex.disb.univ.18p.sex, values_from = sex.disb.univ.18p.ones, values_fill = 0) %>%
  # Pivot sex-age
  pivot_wider(names_from = c(sex.age.sex, sex.age.age), values_from =  sex.age.ones, values_fill = 0) %>%
  # Pivot sex-age (disability universe)
  pivot_wider(names_from = c(sex.age.disb.univ.sex, sex.age.disb.univ.age), values_from = sex.age.disb.univ.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(sex.age.disb.univ.5p.sex, sex.age.disb.univ.5p.age), values_from = sex.age.disb.univ.5p.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(sex.age.disb.univ.18p.sex, sex.age.disb.univ.18p.age), values_from = sex.age.disb.univ.18p.ones, values_fill = 0) %>%
  ## Pivot disabilities
  # Pivot hearing
  pivot_wider(names_from = c(hear.sex.age.disb.sex, hear.sex.age.disb.age, hear.sex.age.disb.disb), values_from = hear.sex.age.disb.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(visn.sex.age.disb.sex, visn.sex.age.disb.age, visn.sex.age.disb.disb), values_from = visn.sex.age.disb.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(cogn.sex.age.disb.sex, cogn.sex.age.disb.age, cogn.sex.age.disb.disb), values_from = cogn.sex.age.disb.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(ambu.sex.age.disb.sex, ambu.sex.age.disb.age, ambu.sex.age.disb.disb), values_from = ambu.sex.age.disb.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(self.sex.age.disb.sex, self.sex.age.disb.age, self.sex.age.disb.disb), values_from = self.sex.age.disb.ones, values_fill = 0) %>%
  pivot_wider(names_from = c(indp.sex.age.disb.sex, indp.sex.age.disb.age, indp.sex.age.disb.disb), values_from = indp.sex.age.disb.ones, values_fill = 0) %>%
  # Get rid of pseudo-records used for sorting
  filter(!is.na(CBSERIAL)) %>%
  # Remove anything with an NA
  select(-matches('\\_NA', ignore.case = FALSE)) %>%
  select(-matches('NA\\_', ignore.case = FALSE)) %>%
  # Get rid of unnecessary columns
  select(
    -c(
      sex, age.fine, age.disb, starts_with('DIFF'), starts_with('RAC', ignore.case = FALSE),
      GQ, OCC, AGE, SEX, HISPAN, HHWT, in.disb.univ
      )
  ) %>%
  select(PUMA, CBSERIAL, PERNUM, PERWT, everything())

# this takes a few seconds

# Ancestry data:
# Borrow some code from this that is elsewhere
# (also takes some time to run)

x.anc.mat = pums %>%
  # Get ancestry data
  select(CBSERIAL, PERNUM, PUMA, ANCESTR1, ANCESTR2) %>%
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
  # Alphabetize rows to ensure columns are in correct order
  arrange(ancestry) %>%
  # Pivot out to make matrix
  pivot_wider(names_from = ancestry, values_from = n.ancestry, values_fill = 0)

dim(x.anc.mat)

# Create synthetic records
x.mat.synthetic = synthetic.records %>%
  # Get disability, race, etc. columns
  cbind(
    x.disb.mat %>% slice(1:nrow(synthetic.records)) %>% select(total:last_col()) %>% mutate(across(everything(), ~ 0))
  ) %>%
  # Get ancestry columns
  cbind(
    x.anc.mat %>% slice(1:nrow(synthetic.records)) %>% select(Other:unclassified) %>% mutate(across(everything(), ~ 0))
  ) %>%
  # Add in fields for hitting constraints
  mutate(
    # Race
    rac.amind = as.numeric(race  %in% 'amind'),
    rac.black = as.numeric(race %in% 'blk'),
    rac.nhpac = as.numeric(race %in% 'pacis'),
    rac.white = as.numeric(race %in% 'wht'),
    # Ancestry
    Other = as.numeric(ancestry %in% 'Other'),
    ReSomalian = as.numeric(ancestry %in% 'ReSomalian'),
    Other.arab = as.numeric(ancestry %in% 'Other.arab'),
    # Count towards total
    total = 1,
    # Count towards disability universe
    disb.univ = 1,
    disb.univ.5p = 1,
    disb.univ.18p = 1,
    # Assume all are non-hispanic
    not.hisp = 1
  ) %>%
  # Deselect columns
  select(-c(race, ancestry, reld))

x.mat.synthetic[,-(1:4)] %>% apply(1, sum)
x.mat.synthetic[,-(1:4)] %>% apply(2, sum) %>% (\(x) x[x>0])
    
# Combine into a single x-matrix
x.mat = merge(x.disb.mat, x.anc.mat) %>%
  arrange(CBSERIAL, PERNUM) %>%
  rbind(x.mat.synthetic)

head(x.mat)
nrow(x.mat)

### Make sure all rows and columns are properly aligned

ncol(x.mat) - 4
y.tab

# slicey = 1:50
# slicey = slicey + 50

data.frame(
  x = names(x.mat)[-(1:4)], # [slicey],
  y = y.tab %>% filter(tract %in% 101) %>% select(var_label, vartype) # %>% slice(slicey)
)


# ==============================================================
# Do the downscaling

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
    # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED (correct here)
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
# Evaluate fit against constraints

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
# nice!!

con.list %>% select(-univ) %>% filter(abs(E - pred) >= M)
# Interesting... not sure what is going on here...
# all disability constraints, in two pumas, in older age groups
# three: pumas 5101, over-estimating in each case (!)
# males 65-74... 

x.tab %>% filter(PUMA %in% 5101, age.disb %in% '65', sex %in% 'Male') # %>%
  # filter(DIFFREM %in% 'with.' | DIFFPHYS %in% 'with.'| DIFFCARE %in% 'no.')
# Okay... we have ~240 records here.

eval.men = con.list %>% 
  mutate(tract = factor(tract)) %>%
  filter(puma %in% 5101, grepl('is\\.sex\\.age\\.diff', vartype), grepl('^male\\:\\s65', var_label)) %>%
  mutate(
    var_label = !grepl('\\sno\\s', var_label),
    var_desc = gsub('sex\\sby\\sage\\sby\\s(\\w+)\\s.+', '\\1', gsub('\\-', '', var_desc))
  ) 

eval.men %>%
  ggplot(aes(x = tract, colour = var_label)) +
  geom_point(aes(y = E, group = var_label), position = position_dodge(width = 0.5)) +
  geom_point(aes(y = pred, group = var_label), shape = 21, position = position_dodge(width = 0.5)) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~ var_desc) +
  theme(axis.text.x = element_text(angle = 90))

eval.men %>%
  mutate(varb = paste0(tract, var_label)) %>%
  ggplot(aes(x = varb, colour = var_label)) +
  geom_segment(aes(xend = varb, y = E-M, yend = E+M, colour = var_label)) +
  geom_point(aes(y = E)) +
  geom_point(aes(y = pred), shape = 21) +
  facet_wrap(~ var_desc) +
  theme(axis.text.x = element_text(angle = 90))
# Hmm... it does look like there are a nursing home in tract 3502?
# Lots of individuals with difficulty living alone or exercising self-care but not others?
# notably this is the one tract that has independent +/ self-care difficulties in high numbers...
# There is an assisted living facility in (near) this tract. 
# but there is neither a correspondingly high number of women, or disabled
# people over age 75 in the tract... or anywhere nearby...
# So is there a men's shelter here?

eval.tract = con.list %>% 
  filter(tract %in% '3502') %>%
  filter(grepl('is\\.sex\\.age\\.diff', vartype)) %>%
  mutate(
    has.diff = !grepl('\\sno\\s', var_label),
    age.grp = gsub('.+\\:\\s(\\d{2}).+', '\\1', var_label),
    sex = ifelse(grepl('^f', var_label), 'female', 'male'),
    var_desc = gsub('sex\\sby\\sage\\sby\\s(\\w+)\\s.+', '\\1', gsub('\\-', '', var_desc))
  ) 

eval.tract %>%
  ggplot(aes(x = age.grp, colour = has.diff)) +
  geom_line(aes(y = E, group = interaction(has.diff, sex))) + 
  geom_point(aes(y = E, shape = sex), size = 3) +
  scale_shape_manual(values = c(1, 19)) +
  facet_wrap(~ var_desc)
# Hmm... 
# Very strange.
# These aren't what we're over-allocating though... the over-allocation is occurring elsewhere
# probably to allow for a lot of men in this age group in general...
# This tract looks to have a disproportionate number of men age 67-69 only...?

# Now look at 75+ women being overallocated to tract 201

eval.women = con.list %>% 
  mutate(tract = factor(tract)) %>%
  filter(puma %in% 5103, grepl('is\\.sex\\.age\\.diff', vartype), grepl('^female\\:\\s75', var_label)) %>%
  mutate(
    var_label = !grepl('\\sno\\s', var_label),
    var_desc = gsub('sex\\sby\\sage\\sby\\s(\\w+)\\s.+', '\\1', gsub('\\-', '', var_desc))
  ) 

eval.women %>%
  mutate(varb = paste0(tract, var_label)) %>%
  ggplot(aes(x = varb, colour = var_label)) +
  geom_segment(aes(xend = varb, y = E-M, yend = E+M, colour = var_label)) +
  geom_point(aes(y = E)) +
  geom_point(aes(y = pred), shape = 21) +
  facet_wrap(~ var_desc) +
  theme(axis.text.x = element_text(angle = 90))
# Tract does look somewhat high in impaired women over age 75...
# surprised though that there are zero women in here with self-care difficulty (acc'd to table)
# the no vision difficulty is pretty close to the MOE
# adjacent tract to the W has a higher density of 75-80 aged women

# Oh well. Not horrible.

# Look at bias
con.list %>%
  # Remove anything with zero MOE
  filter(M > 0) %>%
  # For each puma and constraint type, assess mean bais
  group_by(puma, vartype) %>%
  summarise(bias = mean((E - pred) / M)) %>%
  pivot_wider(names_from = puma, names_prefix = 'p', values_from = bias)
# Huh that's funny
# slightly over-allocating race characteristics, underestimating universe sizes (lol)
# disabilty variables overestimated by 1-2% of the MOE as well

# Ah okay we're missing Other.arabs in PUMS for PUMA 5101, 5103
# also something happening with somalis in one tract
con.list %>% 
  filter(grepl('Somal', var_label), puma  %in% 5103) %>% 
  arrange(desc(abs(E - pred)/M)) %>%
  print(n = nrow(.))
# hmm... wonder what's up with that
# only one Somali in the PUMS?

x.mat %>% 
  filter(ReSomalian > 0, PUMA %in% 5103) %>% 
  select(CBSERIAL, PERNUM) %>% 
  merge(pums)
# okay there is only one Somali in the PUMA in the PUMS
# and she has a person weight of 4
# guess I could copy a synthetic record over

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
  mutate(across(c(cbserial, pernum), as.numeric)) # %>%
  # pivot_wider(names_from = tract, values_from = alloc)

write.csv(allo.weights, row.names = FALSE, file = 'multnomah/03_downscale_out/disability_all_weights_redone.csv')
