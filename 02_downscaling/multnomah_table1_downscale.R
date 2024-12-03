# Script for downscaling PUMAs in Multnomah county
# Tables of interest: Race, Age, Sex, POVPIP (income as percentage of poverty
# level), Hispanic origin
# init SN 25 Oct 2024, pushed a working version 30 Oct 2024
# semi-final 14 Nov 2024

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
  select(PUMA, SEX, AGE, RACE, RACED, HISPAN, HISPAND, US2022A_POVPIP, PERWT)

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
  # grepl('hisp', var_desc), !grepl('not', universe), NA)) %>%
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
) %>% select(-in.y.is.grand.total) %>% sample_n(15)
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

dd = matrix(rep(x.5.all$PERWT, each = JJ), ncol = 1)
dd = dd / sum(dd)

p.5 = PMEDM_solve(xx, yy, vv, dd)
# p.5b = PMEDM_solve(xx, yy, vv, d0)
# identical(p.5a, p.5b)

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

y.fit %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 6400, y = 0, yend = 6400, linetype = 2) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.25) +
  geom_point(aes(y = pred, fill = (pred < E + M & pred > E - M)), shape = 21, size = 2)


y.fit %>%
  mutate(in.mar = pred < E + M & pred > E - M) %>%
  summarise(mm = mean(in.mar))

y.fit %>% filter(pred > E + M | pred < E - M) %>% print(n = nrow(.))

y.fit %>%
  mutate(in.mar = pred < E + M & pred > E - M) %>%
  summarise(mm = mean(in.mar))

y.fit %>%
  mutate(xer = (E - pred) / M) %>%
  ggplot(aes(x = xer)) +
  geom_histogram() +
  annotate('segment', x = -1, xend = -1, y = 0, yend = 10000, linetype = 2) +
  annotate('segment', x =  1, xend =  1, y = 0, yend = 10000, linetype = 2)

# This is good... except for...
y.fit %>%
  mutate(xer = (E - pred) / M) %>%
  arrange(desc(abs(xer)))
# These are looking like cases where tabular zeros are returning non-zero
# results (outside M of course)


####### == Let's re-do this with a factorial data frame just to see what it
####### == looks like
####### ==

# # Need to do this in two steps because this won't play nice with do.call
# # https://stackoverflow.com/questions/71632661/expand-all-columns-using-column-names
# x.5.fac =  do.call(
#   what = expand,
#   args = c(list(x.5.fac), lapply(names(x.5.fac), as.symbol)),
# ) %>%

x.5.fac = x.5.all %>% 
  # Get factorial data frame
  select(rac, pov, sex, hsp) %>% 
  # get rid of nhnw
  # filter(!hsp %in% 'nhnw') %>%
  distinct() %>%
  # handy dplyr analog to expand.grid 
  expand(rac, pov, sex, hsp) %>% 
  merge(data.frame(age = 4:85), by = NULL) %>%
  mutate(
    age.pov = cut(age, breaks = c(age.p.breaks, Inf), right = FALSE),
    age.sex = cut(age, breaks = c(age.s.breaks, Inf), right = FALSE)
  ) %>%
  distinct(rac, pov, sex, hsp, age.pov, age.sex)

# Code below cuts out some rows and I'm not sure why
# rows are lost in the it's in the (age.pov, pov) pivot
# not sure why
# the colsums at this point - some are 48, some are 24 - gotta be because some
# agepovs overlap with multiple agesexes right?
# any chance that has something to do with it? hmm... probably not...
# THIS IS THE MYSTERY TO SOLVE NEXT

x.5.fac.mat = x.5.fac %>%
  ## ## ## processing pipeline
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
  mutate(
    age.pov.ones = 1,
    age.pov.age = age.pov,
    pov.pov = pov
  ) %>%
  # IT HAPPENS HERE
  # hypothesis: this is happening because of the duplicates in age columns?
  pivot_wider(
    names_from = c(age.pov.age, pov.pov), values_from = age.pov.ones, 
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
    rac.sex.age = rac,
    # Duplicating these columns because they will also be used for the hispanic columns
    race.age.sex.age = age.sex,
    race.age.sex.sex = sex
  ) %>%
  pivot_wider(
    names_from = c(rac.sex.age, race.age.sex.sex, race.age.sex.age), values_from = race.age.sex.ones,
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
    hisp.sex.sex = sex,
    hisp.age.sex = hsp,
  ) %>%
  pivot_wider(
    names_from = c(hisp.sex.hsp, hisp.sex.sex), values_from = hisp.sex.ones,
    names_sep = '__', values_fill = 0
  ) %>%
  # Finally, pivot out hisp-age-sex info
  mutate(
    hisp.age.sex.ones = 1,
    hisp.sex.sex = sex,
    hisp.hsp.hsp = hsp,
    hisp.age.sex = age.sex
  ) %>%
  pivot_wider(
    names_from = c(hisp.hsp.hsp, hisp.sex.sex, hisp.age.sex), values_from = hisp.age.sex.ones,
    names_sep = '__', values_fill = 0
  ) %>%
  select(-contains('nhnw'))

dim(x.5.fac.mat)

# Inspecting output
data.frame(
  in.x = names(x.5.fac.mat)[-(1:6)],
  in.y = y.5 %>% 
    distinct(is.grand.total, age.pov, pov, rac, hsp, sex, age.sex) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), '', .)))
) %>%
  sample_n(10)

# Appears fine

# Run the PMEDM

(Nfac = sum(y.5$E[y.5$is.grand.total]))
(nfac = nrow(x.5.fac.mat))
(Jfac = length(unique(y.5$TRACT)))
(kfac = nrow(y.5) / Jfac)

yfac = y.5$E / Nfac
vfac = (y.5$M * nfac / (Nfac^2)) %>%
  .sparseDiagonal(n = length(.)) %>% 
  as('generalMatrix')

xfac = kronecker(t(as.matrix(x.5.fac.mat[, -(1:6)])), .sparseDiagonal(n = Jfac)) %>%
  t() %>%
  as('dgCMatrix')

dfac = matrix(1 / (nfac * Jfac), ncol = 1, nrow = nfac * Jfac)

p.5.fac = PMEDM_solve(xfac, yfac, vfac, dfac)

p.5.constraints = cbind(y.5, pred = p.5.fac$pred * Nfac)

p.5.constraints %>%
  ggplot(aes(x = E, y = pred)) +
  geom_point(aes(colour = hsp))
# Okay once again it's the hispanic data...
# Looks very similar to before.

# Wondering if there's a problem with the non-white non-hispanic grouping in
# constructing the factorial matrix.
# YES THIS IS IT
# YOU NEED THIS DATA FOR EVERYTHING TO RUN CORRECTLY
# I don't know why
# REMEMBER TO REMOVE THE COLUMNS THOUGH

p.5.fac.out = cbind(
  x.5.fac.mat[,(1:6)],
  matrix(p.5.fac$p, nrow = nfac, ncol = Jfac) 
) %>%
  pivot_longer(matches('^\\d+$'), names_to = 'tract.idx', values_to = 'p.ij') %>%
  merge(data.frame(tract.idx = 1:Jfac, tract.no = unique(y.5$TRACT))) %>%
  select(-tract.idx) %>%
  mutate(alloc = p.ij * NN)

p.5.fac.out %>%
  filter(tract.no %in% c(1302, 2502), rac %in% 'white') %>%
  ggplot(aes(x = age.sex, y = pov, fill = alloc)) +
  geom_tile() +
  facet_wrap(hsp ~ tract.no)

p.5.fac.out %>%
  filter(
    tract.no %in% c(1301, 7600), 
    grepl('black', rac),
    hsp %in% 'nhnw'
  ) %>%
  ggplot(aes(x = age.sex, y = pov, fill = alloc)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(tract.no ~ sex) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# cool!

p.5.fac.out %>%
  filter(
    tract.no %in% c(1301, 7600),
    !(rac %in% 'white')
  ) %>%
  ggplot(aes(x = age.sex, y = pov, fill = alloc)) +
  geom_tile() +
  scale_fill_viridis_c() +
  facet_wrap(tract.no + sex ~ rac) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1))

# # Some remaining questions
# - Do priors matter
# - How to quantify differences among allocations
# - I guess distributions of continuous ACS variables within constraint bins?


# Okay maybe this is less useful than I thought...
# 
# 
# ####### == Look at output data with just the observed ACS data.
# ####### == 
# ####### ==
# 
p.5.p = matrix(p.5$p, nrow = nn, ncol = JJ, byrow = TRUE) %>%
  as.data.frame() %>%
  mutate(xidx = 1:nrow(.)) %>%
  pivot_longer(-xidx, names_to = 'tract.let', values_to = 'prob') %>%
  merge(data.frame(tract.let = paste0('V', 1:JJ), tract.no = unique(y.5$TRACT))) %>%
  select(-tract.let) %>%
  mutate(allocate = prob * NN)  %>%
  merge(x.5.all, by = 'xidx')
# 
# head(p.5.p)
# dim(p.5.p)
# 
# p.5.p = p.5.p %>%
#   group_by(xidx) %>%
#   mutate(
#     p.ij = prob /sum(prob),
#     p.mx = max(p.ij)
#   ) %>%
#   ungroup()
# 
# head(p.5.p)
# 
# p.5.p.i = p.5.p %>% distinct(xidx, AGE, SEX, RACE, HISPAN, US2022A_POVPIP, p.mx) 
# 
# p.5.p.i %>% ggplot(aes(x = p.mx)) + geom_histogram()
# # fuckin lmao..
# # would it be different if we assigned different prior weights? who knows man
# 
# p.5.p.i %>%
#   mutate(
#     SEX = as.factor(SEX),
#     US2022A_POVPIP = as.numeric(US2022A_POVPIP) / 100,
#     AGE = as.numeric(AGE)
#   ) %>%
#   ggplot(aes(x = US2022A_POVPIP, y = p.mx)) +
#   annotate('segment', x = 0, xend = 5, y = 1/JJ, yend = 1/JJ, linetype = 2) +
#   geom_point(aes(colour = AGE), size = 3) +
#   scale_colour_viridis_c() +
#   facet_wrap(~ RACE)
# 
# p.5.p.i %>%
#   mutate(
#     SEX = as.factor(SEX),
#     US2022A_POVPIP = as.numeric(US2022A_POVPIP) / 100,
#     AGE = as.numeric(AGE)
#   ) %>%
#   ggplot(aes(x = US2022A_POVPIP, y = p.mx)) +
#   annotate('segment', x = 0, xend = 5, y = 1/JJ, yend = 1/JJ, linetype = 2) +
#   geom_point(size = 3, alpha = 0.5) +
#   scale_colour_viridis_c()
# 
# # Hmm...
# # That's so much uncertainty dawg.
# # Not good!
# # White respondents - higher income rates are not that much better than random.
# 
# # Okay what is our actual data coverage here
# 
# p.5.coverage = x.5.all %>%
#   group_by(AGE, RACE, HISPAN, pov = as.numeric(US2022A_POVPIP) / 100) %>%
#   summarise(n = n()) %>%
#   ungroup()
# 
# # Yeah good okay.
# 
# p.5.coverage %>%
#   ggplot(aes(x = AGE, y = n)) +
#   geom_point(position = position_jitter(height = 1), size = 4, alpha = 0.5) +
#   facet_wrap(~ RACE)
# 
# # Nearly all of this is quite sparse.
# # Maybe that doesn't matter though?
# 
# p.5.summ = p.5.p %>%
#   group_by(AGE, RACE, HISPAN, pov = as.numeric(US2022A_POVPIP) / 100, tract.no) %>%
#   summarise(allocate = sum(allocate))
# 
# head(p.5.summ)
# 
# hist(p.5.summ$allocate)
# 
# p.5.summ %>% 
#   filter(RACE %in% 1, HISPAN %in% 0, tract.no %in% c(1301, 2502)) %>% 
#   ggplot(aes(x = AGE, y = pov, fill = allocate)) + geom_tile() + facet_wrap(~ tract.no)
# 
# # Not useful lmao
# 
# p.5.summ %>% 
#   filter(RACE %in% 1, HISPAN %in% 0, tract.no %in% c(1301, 2502)) %>% 
#   ggplot(aes(x = AGE, y = pov, colour = allocate)) + 
#   geom_point(size = 4) +
#   scale_colour_viridis_c() +
#   facet_wrap(~ tract.no)
# 
# # Hmm... okay, looking like using the bins is the way to go? ugh
# 
# p.5.summ = p.5.p %>%
#   group_by(age.sex, RACE, HISPAN, pov, tract.no) %>%
#   summarise(allocate = sum(allocate))
# 
# p.5.summ %>%
#   filter(tract.no %in% c(1301, 2502), allocate > 1) %>%
#   mutate(lallocate = log(allocate, base = 10)) %>%
#   ggplot(aes(x = age.sex, y = pov, fill = lallocate)) +
#   geom_tile() +
#   facet_wrap(RACE ~ tract.no)
# 
# # Ugh this coverage is not useful...
# # I think I'm going to need the factorial.

x.5.all %>%
  mutate(pov.cont = as.numeric(US2022A_POVPIP) / 100) %>%
  ggplot(aes(x = SEX, y = pov.cont)) +
  geom_point(position = position_jitter(width = 0.125), alpha = 0.5, size = 3)

x.5.all %>%
  filter(!grepl('^5', US2022A_POVPIP)) %>%
  mutate(pov.cont = as.numeric(US2022A_POVPIP) / 100) %>%
  ggplot(aes(x = pov.cont)) +
  geom_histogram() +
  facet_wrap(~ pov, scales = 'free')

# Okay... how much is this actually deviating from uniformity?
# aside from the zeros obviously
# (jesus some of these levels are sparse)

x.5.all %>%
  filter(!grepl('^5', US2022A_POVPIP)) %>%
  mutate(pov.cont = as.numeric(US2022A_POVPIP) / 100) %>%
  ggplot(aes(x = pov.cont, group = hsp, fill = hsp)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  facet_wrap(~ pov, scales = 'free')

x.5.all %>%
  filter(!grepl('^5', US2022A_POVPIP)) %>%
  mutate(pov.cont = as.numeric(US2022A_POVPIP) / 100) %>%
  ggplot(aes(x = pov.cont, group = rac, fill = rac)) +
  geom_histogram(position = 'identity', alpha = 0.5) +
  facet_wrap(~ pov, scales = 'free')

# Really not seeing anything interesting in here.
# Throwing in more variables is just going to be small sample size effects

x.5.all %>%
  ggplot(aes(x = AGE, fill = sex, group = sex)) +
  geom_bar(position = position_dodge()) +
  facet_wrap(~ age.sex, scales = 'free')

# ehhhhhh
# I just really am not convinced there's anything interesting in these
# not sure why we can't just break down age bins from the factorial approach using 

# even if there *was* some sort of departure from uniformity (or global
# distributions) that you wanted to capture correlations with I don't think this
# dataset is going to be helpful for those
# like if e.g. there were a lot more 18 year old black men in a certain stratum
# compared to 19 year olds, I don't think we will have the data to really pick
# up on any signal

# what this data *could* be used for is I guess re-binning to specified
# dimensions as requested by client?
# or would it be better to just use global distributions for that? (global
# distributions though... probably hierarchical process would be better here)

# what would happen if 

### Looking at what the weights are actually doing

#  old uniform prior # 
d0 = matrix(1 / (nn * JJ), ncol = 1, nrow = nn * JJ)

p.5.unif = PMEDM_solve(xx, yy, vv, d0)

plot(p.5$p, p.5.unif$p)

p5.mat = merge(
  cbind(
    xidx = x.5$xidx,
    matrix(
      p.5$p, nrow = nn, ncol = JJ, 
      dimnames = list(NULL, paste0('a.', unique(sort(y.5$TRACT))))
    )
  ) %>% as.data.frame(),
  cbind(
    xidx = x.5$xidx,
    matrix(
      p.5.unif$p, nrow = nn, ncol = JJ,
      dimnames = list(NULL, paste0('u.', unique(sort(y.5$TRACT))))
    ) 
  ) %>% as.data.frame(),
  by = 'xidx'
)

head(p5.mat)

p5.compare = p5.mat %>%
  pivot_longer(-xidx, names_to = 'au.tract', values_to = 'p') %>%
  separate_wider_delim('au.tract', names = c('model', 'tract'), delim = '.') %>%
  pivot_wider(names_from = model, values_from = p) %>%
  merge(x.5.all)

p5.compare %>%
  ggplot(aes(x = a, y = u)) +
  geom_point(aes(fill = log(PERWT, base = 10)), shape = 21) +
  scale_fill_viridis_c() +
  facet_wrap(~ rac)

p5.compare %>%
  ggplot(aes(x = xidx, y = u - a)) +
  geom_point(aes(colour = PERWT)) +
  scale_colour_viridis_c()

# The weight actually doesn't seem to be associated with error...

p5.compare %>%
  filter(xidx < 10) %>%
  ggplot(aes(x = a, y = u)) +
  geom_point(shape = 21) +
  scale_fill_viridis_c() +
  facet_wrap(~ xidx)

p5.compare %>%
  filter(as.numeric(tract) < 1602) %>%
  ggplot(aes(x = a, y = u)) +
  geom_point(shape = 21) +
  scale_fill_viridis_c() +
  facet_wrap(~ tract)

p5.compare %>%
  group_by(tract) %>%
  summarise(p.a.u = mean(a > u)) %>%
  ggplot(aes(x = p.a.u)) +
  geom_histogram(binwidth = 0.05)

p5.compare %>%
  ggplot(aes(x = tract, y = u - a)) +
  geom_point(aes(colour = u > a), size = 3, alpha = 0.1)

p5.c.test = p5.compare %>%
  group_by(xidx) %>%
  mutate(mean.u.a = mean(u - a)) %>%
  ungroup() %>%
  distinct(xidx, .keep_all = TRUE)

p5.compare %>% 
  ggplot(aes(x = xidx, y = tract, fill = NN * (u - a))) +
  geom_tile() +
  scale_fill_gradient2(low = 'blue', high = 'red', mid = 'white', midpoint = 0)

# man I just have no fucking idea what the weights are doing here...

p5.c.test %>%
  ggplot(aes(x = xidx, y = NN * mean.u.a)) +
  geom_point(aes(colour = age.pov)) +
  scale_colour_viridis_d() +
  facet_wrap(~ pov)

# I have no fucking idea what is happening.

# maybe it's just the middle of the distribution... repeated individuals with low weights
# next thing to try I guess

# ####### == Single year of age (SYOA) estimates...
# ####### == 
# ####### ==

x.5.preds = merge(
  cbind(
    xidx = x.5$xidx,
    matrix(
      p.5$p, nrow = nn, ncol = JJ, byrow = TRUE,
      dimnames = list(NULL, paste0('a.', unique(sort(y.5$TRACT))))
    )
  ) %>% as.data.frame(),
  x.5.all,
  by = 'xidx'
) %>%
  pivot_longer(matches('^a.\\d'), names_to = 'tract', values_to = 'alloc') %>%
  mutate(alloc = alloc * NN)

head(x.5.preds)

x.5.preds

x.5.preds %>%
  filter(grepl('1302', tract)) %>%
  ggplot(aes(x = AGE, y = alloc, colour = rac)) +
  geom_point(alpha = 0.5, size = 3, position = position_jitter(width = 1/4))

x.5.preds %>%
  group_by(AGE, tract) %>%
  summarise(suma = sum(alloc)) %>%
  ggplot(aes(x = AGE, y = suma, group = tract)) +
  geom_point() +
  geom_line()

age.summ = x.5.preds %>%
  group_by(AGE, tract) %>%
  summarise(suma = sum(alloc)) %>%
  ungroup()

age.p.summ =  age.summ %>%
  mutate(age.p = cut(AGE, breaks = c(age.p.breaks, Inf), right = FALSE)) %>%
  group_by(age.p, tract) %>%
  summarise(n = sum(suma))

age.s.summ =  age.summ %>%
  mutate(age.s = cut(AGE, breaks = c(age.s.breaks, Inf), right = FALSE)) %>%
  group_by(age.s, tract) %>%
  summarise(n = sum(suma))

# extracting 
# age.p.summ %>% 
#   distinct(age.p, tract) %>%
#   mutate(
#     tract = gsub('a\\.', '', tract),
#     age.p = gsub('^\\[(\\d{1,2})\\,[[:alnum:]]*\\)', '\\1', age.p)
#   )

age.p.summ = merge(
  age.p.summ %>% 
    mutate(
      tract = gsub('a\\.', '', tract),
      age.p = gsub('^\\[(\\d{1,2})\\,[[:alnum:]]*\\)', '\\1', age.p)
    ),
  # this should be just age totals
  y.5 %>% 
    filter(!is.na(age.pov), is.na(pov)) %>%
    mutate(age.pov = gsub('(\\d{1,2})\\,[[:alnum:]]*', '\\1', age.pov)),
  by.x = c('tract', 'age.p'), by.y = c('TRACT', 'age.pov')
)

# age.p.summ %>% 
#   mutate(age.p = as.numeric(age.p)) %>%
#   ggplot(aes(fill = age.p)) + 
#   geom_segment(aes(x = E - M, xend = E + M, y = E, yend = E, colour = age.p)) +
#   geom_point(aes(x = n, y = E), size = 3, shape = 21) +
#   scale_colour_viridis_c() +
#   scale_fill_viridis_c()

x.5.preds %>%
  group_by(tract = gsub('a.', '', tract)) %>%
  summarise(n = sum(alloc)) %>%
  merge(
    y = y.5 %>% filter(is.grand.total) %>% filter(is.grand.total) %>% select(TRACT, E, M),
    by.x = 'tract', by.y = 'TRACT'
  ) %>%
  ggplot(aes(x = tract)) +
  geom_segment(aes(xend = tract, y = E - M, yend = E + M)) +
  geom_point(aes(y = n))
# sweet

age.p.summ %>%
  ggplot(aes(x = tract)) +
  geom_segment(aes(xend = tract, y = E - M, yend = E + M)) +
  geom_point(aes(y = n)) +
  facet_wrap(~ age.p)
# Great!

ggsave('~/Desktop/puma5105_ageconstraint.png', width = 12, height = 9)

merge(
  age.summ, 
  expand.grid(AGE = 0:max(age.summ$AGE), tract = unique(age.summ$tract)), 
  all.y = TRUE
) %>%
  mutate(suma = ifelse(is.na(suma), 0, suma)) %>%
  mutate(tract = gsub('a\\.', '', tract)) %>%
  ggplot(aes(x = AGE, y = suma)) +
  geom_point(aes(colour = !suma)) +
  geom_line() +
  scale_colour_manual(values = c('black', 'orange')) +
  facet_wrap(~ tract) +
  theme(legend.position = 'none')

ggsave('~/Desktop/puma5105_syoa_all.png', width = 12, height = 9)

age.sex.summ = x.5.preds %>%
  group_by(AGE, sex, tract) %>%
  summarise(suma = sum(alloc)) %>%
  ungroup()

age.sex.summ = merge(
  age.sex.summ, 
  expand.grid(
    AGE = 0:max(age.sex.summ$AGE), 
    sex = c('female', 'male'), 
    tract = unique(age.sex.summ$tract)
  ), 
  all.y = TRUE
) 

age.sex.summ %>%
  mutate(suma = ifelse(is.na(suma), 0, suma)) %>%
  mutate(tract = gsub('a\\.', '', tract)) %>%
  ggplot(aes(x = AGE, y = suma)) +
  geom_line(aes(colour = sex)) +
  geom_point(aes(colour = sex, fill = !suma), size = 2, shape = 21) +
  scale_fill_manual(values = c('black', 'orange')) +
  scale_colour_manual(values = c('magenta', 'dodgerblue')) +
  facet_wrap(~ tract) +
  theme(legend.position = 'none')

ggsave('~/Desktop/puma5105_syoa_sex.png', width = 12, height = 9)

# Example of allocation

x.5.preds %>%
  filter(grepl('1301', tract) | grepl('76', tract)) %>%
  mutate(tract = gsub('a\\.', '', tract)) %>%
  filter(rac %in% 'white', hsp %in% 'nhwh', grepl('^[012]', US2022A_POVPIP), AGE < 81) %>%
  group_by(AGE, tract) %>%
  summarise(alloc = sum(alloc)) %>%
  ungroup() %>%
  merge(expand.grid(AGE = 0:80, tract = c('1301', '7600')), all.y = TRUE) %>%
  mutate(alloc = ifelse(is.na (alloc), 0, alloc)) %>%
  mutate(tract = paste0('tract ', tract)) %>%
  ggplot(aes(x = AGE, y = alloc)) +
  geom_segment(aes(xend = AGE, yend = 0)) +
  geom_point() +
  labs(x = 'age', y = 'estimated number of individuals') +
  theme(
    panel.background = element_blank(),
    strip.background = element_rect(fill = 'white')
  ) +
  facet_wrap(~ tract)

ggsave('02_downscaling/draft_figs/2024-12-mod1-estimates.png', width = 5, height = 3)

### ---

