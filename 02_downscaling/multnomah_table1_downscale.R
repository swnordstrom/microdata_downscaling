# Script for downscaling PUMAs in Multnomah county
# Tables of interest: Race, Age, Sex, POVPIP (income as percentage of poverty
# level), Hispanic origin
# init SN 25 Oct 2024, pushed a working version30 Oct 2024

# BUT: CURRENTLY  CODES HISPANIC ETHNICITY WRONG IN Y DATASET
# should remove the hispFALSE label because this isn't ALL non-hispanics but is
# instead ONLY NON-HISPANIC WHITES
# or, alternatively, could adjust the hispFALSE in X to truly be NH white
# note I think both approaches would remove the property we've had so far of
# each record contributing to the same number of constraints...

####### == Setup
####### ==
####### ==

# Load packages
library(PMEDMrcpp)
library(ipumsr)
library(ggplot2)
library(dplyr)
library(tidyr)
# library(purrr)

# Clear namespace
rm(list = ls())

### Read in data

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
pum.m = read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  select(-STATEFP, COUNTYFP)

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00009.xml')
# Read in sample and 
acs.m   = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Oregon
  filter(PUMA %in% unique(pum.m$PUMA5CE)) %>%
  # Remove unnecessary columns
  select(PUMA, SEX, AGE, RACE, RACED, HISPAN, HISPAND, US2022A_POVPIP)

nrow(acs.m)

tab.raw = read_nhgis('01_raw_data/nhgis0005_csv.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')
pop.raw = read_nhgis('01_raw_data/2018-2022_ACS_tract_population_sizes.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')

tab.m = merge(
  tab.raw %>% select(TRACTA, starts_with('AQ')),
  pop.raw %>% select(TRACTA, starts_with('AQ'))
) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(pum.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

### Picking a PUMA

# Picking 5105

acs.5 = acs.m %>% filter(PUMA %in% 5105)
tab.5 = tab.m %>% filter(PUMA %in% 5105)

# BUT I'll keep the PUMA tab in here because it will allow us to split and lapply

####### == Format and reconcile data
####### ==
####### ==

### Format y (tabular) data

y.5 = tab.5 %>%
  # Pivot variable columns to rows and merge in variable information
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  # Merging in variable info
  merge(
    rbind(
      ipums_var_info(tab.raw, starts_with('AQ')) %>% select(-val_labels),
      ipums_var_info(pop.raw, starts_with('AQ')) %>% select(-val_labels)
    )
  ) %>%
  # Convert variable info to lowercase for easier regexing
  mutate(across(c(var_label, var_desc), tolower)) %>%
  # Split the variable name up so that we can pivot out estimates and margins
  # for same variable
  separate_wider_position(var_name, c(form = 4, me = 1, code = 3)) %>%
  mutate(
    var_label = gsub('estimates:\\s', '', var_label),
    var_label = gsub('margins\\sof\\serror\\:\\s', '', var_label)
  ) %>%
  pivot_wider(names_from = me, values_from = value) %>%
  # Scrape out universe information
  mutate(
    universe = gsub('.*(\\(universe.*\\))', '\\1', var_desc),
    universe = gsub('\\(universe\\:\\s(people\\swho\\sare\\s)*', '', universe),
    universe = gsub('(\\salone)*\\)', '', universe)
  ) %>%
  # Age-poverty total column is superfluous
  filter(!(grepl('total', var_label) & grepl('poverty', universe))) %>%
  ### Make grand total column
  mutate(is.grand.total = grepl('total', var_label) & grepl('total', universe)) %>%
  ### Format age+poverty columns
  mutate(agepovpip = ifelse(grepl('pov', var_desc), var_label, NA)) %>%
  # Separate into age and pov components
  separate_wider_delim(
    col = agepovpip, delim = ': ',
    names = c('age.pov', 'pov'),
    too_few = 'align_start' # no colon is age-bucket total
  ) %>% 
  # Format ages into age ranges
  mutate(
    age.pov = gsub('[Uu]nder', '0 to', age.pov),
    age.pov = gsub('\\syears(.)*$', '', age.pov),
    age.pov = gsub('\\sto\\s', ',', age.pov)
  ) %>% 
  # Format poverty ranges
  mutate(
    pov = gsub('[Uu]nder', '0.00 to', pov),
    pov = gsub('[^0-9]+$', '', pov),
    pov = gsub('\\s[a-z]+\\s', ',', pov)
  ) %>%
  ### Format age+sex+race columns
  # (Note: for the age-sex columns, race is stored in the universe col)
  mutate(sexage = ifelse(grepl('age', var_desc) & grepl('sex', var_desc), var_label, NA)) %>% 
  # Separate into sex and age columns 
  separate_wider_delim(
    col = sexage, delim = ': ',
    names = c('sex', 'age.sex'),
    too_few = 'align_start' # no column is sex total
  ) %>%
  # Format ages into age ranges
  mutate(
    age.sex = gsub('[Uu]nder', '0 to', age.sex),
    age.sex = gsub('[^0-9]+$', '', age.sex),
    age.sex = gsub('\\s[a-z]+\\s', ',', age.sex)
  ) %>%
  ### Hispanic yes/no column
  mutate(
    hsp = case_when(
      grepl('hisp', var_desc) & !grepl('not', universe) ~ 'hisp',
      grepl('hisp', var_desc) &  grepl('not', universe) ~ 'nhwh',
      .default = NA
    )
  ) %>%
#       grepl('hisp', var_desc), !grepl('not', universe), NA)) %>%
  ### Race column
  # race was extracted and put into the 'universe' column
  mutate(rac = ifelse(grepl('sex', var_desc) & !grepl('hisp', var_desc), universe, NA)) %>%
  # Need to combine the asian and pacific islander columns
  mutate(
    rac = ifelse(
      grepl('([Aa]sian)', rac) | grepl('[Pp]acific', rac), 
      'AANHPI', 
      rac
    )
  ) %>%
  group_by(TRACT, is.grand.total, age.pov, pov, sex, age.sex, rac, hsp) %>%
  summarise(E = sum(E), M = sqrt(sum(M^2))) %>%
  ungroup()

# Need to arrange now...
y.5 = y.5 %>%
  mutate(
    age.pov.sort = gsub('^(\\d{1,2})\\,\\d{1,2}', '\\1', age.pov) %>% as.numeric(),
    pov.sort     = gsub('^(\\d*\\.*\\d{0,2})\\,\\d*\\.\\d{1,2}', '\\1', pov) %>% as.numeric(),
    age.sex.sort = gsub('^(\\d{1,2})\\,\\d{1,2}', '\\1', age.sex) %>% as.numeric()
  ) %>%
  # Ugh. Christ this is annoying
  # Going to change the sorting ordering to get totals first
  # setting numeric totals to -Inf (lmao) and character totals to 'all' (alphabetical)
  # this column sorting is such a pain in the neck...
  mutate(
    age.sex.sort = ifelse(is.na(age.sex) & (!is.na(rac) | !is.na(hsp)) & !is.na(sex), -Inf, age.sex.sort),
    pov.sort = ifelse(!is.na(age.pov) & is.na(pov), -Inf, pov.sort),
    sex = ifelse(sex %in% 'total', 'all', sex)
  ) %>%
  arrange(
    desc(is.grand.total), 
    # age and poverty sorting
    pov.sort, age.pov.sort,
    # age sex and race/ethnicity sorting
    # age.sex.sort, sex, rac, hsp, TRACT
    !is.na(hsp), age.sex.sort, sex, rac, hsp, TRACT
  ) %>%
  # group_by(age)
  # below filter call is JUST FOR DEBUGGING
  # filter(is.na(age.pov.sort), is.na(pov.sort), !is.grand.total, TRACT %in% 1101) %>% print(n = 50)
  select(-ends_with('sort'))

# # Check for sub-totals
# y.5 %>% 
#   filter(
#     if_any(c(age.pov, pov), ~ !is.na(.)), 
#     if_any(c(age.pov, pov), ~ is.na(.))
#   ) %>%
#   distinct(age.pov, pov)
# # We have age(-pov) totals, no pov totals
# 
# y.5 %>% 
#   filter(
#     if_any(c(age.sex, sex, rac), ~ !is.na(.)), 
#     if_any(c(age.sex, sex, rac), ~ is.na(.))
#   ) %>%
#   distinct(rac, hsp, age.sex, sex) %>% print(n = nrow(.))
# # we have sex-race/eth totals, no age(-race)


# Get cuts for age and poverty

pov.breaks = y.5 %>%
  distinct(pov) %>%
  filter(grepl('\\d', pov)) %>% 
  mutate(pov = gsub('^(\\d{0,1}\\.{0,1}\\d{0,2}).*', '\\1', pov)) %>%
  mutate(pov = as.numeric(pov)) %>%
  arrange(pov) %>%
  pull(pov)

age.s.breaks = y.5 %>%
  distinct(age.sex) %>%
  filter(grepl('\\d', age.sex)) %>%
  mutate(age.sex = gsub('^(\\d{1,2}).*', '\\1', age.sex)) %>%
  mutate(age.sex = as.numeric(age.sex)) %>%
  arrange(age.sex) %>%
  pull(age.sex)

age.p.breaks = y.5 %>%
  distinct(age.pov) %>%
  filter(grepl('\\d', age.pov)) %>%
  mutate(age.pov = gsub('^(\\d{1,2}).*', '\\1', age.pov)) %>%
  mutate(age.pov = as.numeric(age.pov)) %>%
  arrange(age.pov) %>%
  pull(age.pov)

### Format x data

x.5.all = acs.5 %>%
  select(-PUMA) %>%
  filter(
    # get rid of missing sexes if they exist
    !(SEX > 2),
    # get rid of missing hispanic codings
    !(HISPAN > 5),
    # get rid of missing POVPIV entries
    !grepl('[A-Za-z]', US2022A_POVPIP)
  ) %>%
  # Format some data
  mutate(
    rac = case_match(
      RACE,
      1 ~ 'White',
      2 ~ 'Black or African American',
      3 ~ 'American Indian and Alaska Native',
      4:6 ~ 'AANHPI',
      8:9 ~ 'Two or more races',
      .default = as_factor(RACE)
    ) %>% tolower(),
    pov = as.numeric(US2022A_POVPIP) / 100,
    # (as character because factor coding has male first)
    sex = as.character(as_factor(SEX)) %>% tolower(),
    age.pov = cut(AGE, breaks = c(age.p.breaks, Inf), right = FALSE),
    age.sex = cut(AGE, breaks = c(age.s.breaks, Inf), right = FALSE),
    pov     = cut(pov, breaks = c(pov.breaks, Inf), right = FALSE),
    hsp = case_when(
      HISPAN > 0 ~ 'hisp',
      RACE %in% 1 ~ 'nhwh',
      .default = 'nhnw'
    )
  ) %>%
  mutate(xidx = 1:nrow(.)) %>%
  select(xidx, everything())

x.5 = x.5.all %>%
  select(xidx, pov, age.pov, age.sex, sex, rac, hsp) %>%
  rbind(
    expand.grid(
      xidx = 0,
      age.pov = unique(.$age.pov),
      pov = unique(.$pov),
      age.sex = unique(.$age.sex),
      sex = unique(.$sex),
      rac = unique(.$rac),
      hsp = unique(.$hsp)
    )
  ) %>%
  # (this step might not be needed...)
  arrange(pov, age.pov, age.sex, sex, rac, hsp) %>%
  # Format group labels for tabular readability
  mutate(
    age.pov = gsub('^\\[', 'agepov', age.pov),
    pov     = gsub('^\\[', 'pov', pov),
    age.sex = gsub('^\\[', 'age.sex', age.sex),
    across(c(age.pov, pov, age.sex), ~ gsub('\\,', '_', .x)),
    across(c(age.pov, pov, age.sex), ~ gsub('\\)$', '', .x))
  ) %>%
  # Add total columns and start pivoting
  # First pivot poverty's age totals
  mutate(
    total = 1,
    age.pov.age.ones = 1,
    age.pov.age.tota = age.pov
  ) %>%
  pivot_wider(names_from = age.pov.age.tota, values_from = age.pov.age.ones, values_fill = 0) %>%
  # Next pivot out age-by-poverty data
  mutate(age.pov.ones = 1) %>%
  pivot_wider(
    names_from = c(age.pov, pov), values_from = age.pov.ones, 
    names_sep = '__', values_fill = 0
  ) %>%
  # Next: race- and hispanic-total columns
  mutate(
    race.total.ones = 1,
    race.total = rac
  ) %>%
  pivot_wider(names_from = race.total, values_from = race.total.ones, values_fill = 0) %>%
  # Next: get sex-race and sex-ethnicity combos
  mutate(
    race.sex.ones = 1,
    race.sex.rac = rac,
    race.sex.sex = sex,
  ) %>%
  pivot_wider(
    names_from = c(race.sex.rac, race.sex.sex), values_from = race.sex.ones, 
    names_sep = '__', values_fill = 0
  ) %>%
  # Next pivot out race-age-sex info
  mutate(
    race.age.sex.ones = 1,
    # Duplicating these columns because they will also be used for the hispanic columns
    race.age.sex.age = age.sex,
    race.age.sex.sex = sex
  ) %>%
  pivot_wider(
    names_from = c(rac, race.age.sex.sex, race.age.sex.age), values_from = race.age.sex.ones,
    names_sep = '__', values_fill = 0
  ) %>%
  mutate(
    hisp.total.ones = 1,
    hisp.total = hsp
  ) %>%
  pivot_wider(names_from = hisp.total, values_from = hisp.total.ones, values_fill = 0) %>%
  mutate(
    hisp.sex.ones = 1,
    hisp.sex.hsp = hsp,
    hisp.sex.sex = sex
  ) %>%
  pivot_wider(
    names_from = c(hisp.sex.hsp, hisp.sex.sex), values_from = hisp.sex.ones,
    names_sep = '__', values_fill = 0
  ) %>%
  # Finally, pivot out hisp-age-sex info
  mutate(hisp.age.sex.ones = 1) %>%
  pivot_wider(
    names_from = c(hsp, sex, age.sex), values_from = hisp.age.sex.ones,
    names_sep = '__', values_fill = 0
  ) %>%
  filter(xidx > 0) %>%
  select(-contains('nhnw')) %>%
  arrange(xidx)
 
data.frame(
  in.x = names(x.5)[-1],
  in.y = y.5 %>% 
    distinct(is.grand.total, age.pov, pov, rac, hsp, sex, age.sex) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), '', .)))
) # %>% select(-in.y.is.grand.total) %>% sample_n(15)
# Looks good

# Constraints each individual should be contributing to (not in order)
# 1. grand total
# 2. age(-pov) total
# 3. age-pov
# 4. total race
# 5. total hisp
# 6. sex-race
# 7. sex-hisp
# 8. sex-race-age
# 9. sex-hisp-age
# note that non-hispanic nonwhites (NHNW) don't contribute to 5, 7, 9

####### == Run PMEDM
####### ==
####### ==

(NN = sum(y.5$E[y.5$is.grand.total]))
(nn = nrow(x.5))
(JJ = length(unique(y.5$TRACT)))
(kk = nrow(y.5) / JJ)

yy = y.5$E / NN
vv = (y.5$M * nn / (NN^2)) %>%
  .sparseDiagonal(n = length(.)) %>% 
  as('generalMatrix')

xx = kronecker(t(as.matrix(x.5[, -1])), .sparseDiagonal(n = JJ)) %>%
  t() %>%
  as('dgCMatrix')

dd = matrix(1 / (nn * JJ), ncol = 1, nrow = nn * JJ)

p.5 = PMEDM_solve(xx, yy, vv, dd)

####### == Evaluate PMEDM output
####### ==
####### ==

plot(data.frame(observed = y.5$E, predicted = p.5$pred * NN)) # NICE
abline(a = 0, b = 1)

y.fit = y.5 %>% mutate(pred = p.5$pred * NN)

head(y.fit)

# Looking at output
y.fit %>%
  ggplot(aes(x = E, y = pred)) +
  annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
  geom_point(aes(colour = hsp, shape = pred > E)) +
  scale_shape_manual(values = c(1, 19)) +
  coord_fixed()



