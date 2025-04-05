# Script for doing *single PUMA* downscaling of PUMS data (Multnomah County) to
# obtain veteran-by-age counts

library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
tra.m = read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  select(-STATEFP, COUNTYFP)

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00011.xml')

# Read in sample and 
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Oregon
  filter(PUMA %in% unique(tra.m$PUMA5CE)) %>%
  # Remove unnecessary columns
  select(PUMA, SERIAL, PERNUM, PERWT, AGE, SEX, VETSTAT)

nrow(acs.m)

# Read in NHGIS tabular data
tab.raw = read_nhgis('01_raw_data/nhgis0010_csv.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')
pop.raw = read_nhgis('01_raw_data/2018-2022_ACS_tract_population_sizes.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')

# Merge together NHGIS tabular data (merge population sizes with vet status)
tab.m = merge(
  tab.raw %>% select(TRACTA, starts_with('AQ')),
  pop.raw %>% select(TRACTA, starts_with('AQ'))
) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

head(tab.m)
names(tab.m)

### Picking a PUMA

# Picking 5105

acs.5 = acs.m %>% filter(PUMA %in% 5105)
tab.5 = tab.m %>% filter(PUMA %in% 5105)

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Now: begin data processing

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
  ### Establish total columns
  mutate(
    is.grand.total = grepl('total', var_label) & grepl('total', universe),
    total.18p = grepl('total', var_label) & !grepl('total', universe),
    total.sex = grepl('(fe)?male$', var_label),
    total.vet = grepl('^(non)*vet', var_label),
    sex.vet = grepl('^(fe)?male\\:\\s(non)?vet', var_label),
    sex.age = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label),
    sex.age.vet = grepl('\\:.*\\:', var_label)
  ) %>%
  # Get labels/groupings for sex, age, vet status
  mutate(
    sex = case_when(
      total.sex ~ var_label,
      sex.vet | sex.age | sex.age.vet ~ gsub('\\:.*', '', var_label),
      .default = NA
    ),
    age = case_when(
      sex.age ~ gsub('.+\\:\\s', '', var_label),
      sex.age.vet ~ gsub('.+\\:\\s(.+)\\:.+', '\\1', var_label),
      .default = NA
    ),
    vet = case_when(
      total.vet ~ var_label,
      sex.vet ~ gsub('(fe)*male\\:\\s', '', var_label),
      sex.age.vet ~ gsub('.+\\:.+\\:\\s', '', var_label),
      .default = NA
    )
  ) %>% # distinct(var_label, sex, age, vet) %>% print(n = nrow(.))
  # Format age column
  mutate(
    # below is maybe easier with \\w
    age = gsub('years\\sand\\sover', 'to Inf', age),
    age = gsub('\\syears$', '', age),
    age = gsub('\\sto\\s', ',', age)
  ) # %>% # distinct(age)

  
# Okay. Now need to start pivoting...

y.5 = y.5 %>%
  # select(-c(form, code, TRACT, PUMA)) %>%
  # distinct(var_label, .keep_all = TRUE) %>%
  arrange(
    desc(is.grand.total), desc(total.18p), desc(total.sex), desc(total.vet), 
    desc(sex.vet), desc(sex.age), desc(sex.age.vet), sex, age, desc(vet),
    TRACT
  ) # %>% distinct(var_label) %>% print(n = nrow(.))
# 40 attributes here

# Get age breaks
age.breaks = y.5 %>%
  filter(!is.na(age)) %>%
  distinct(age) %>%
  pull(age) %>%
  gsub('(\\d{1,2})\\,\\w+', '\\1', .) %>%
  as.numeric()

# View all attributes in order:
y.5 %>% 
  distinct(
    is.grand.total, total.18p, total.sex, total.vet, 
    sex.vet, sex.age, sex.age.vet, sex, age, vet
  ) %>% 
  print(n = nrow(.))

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Do the X (ACS/PUMS data)

# Some notes:
# - VETSTAT = 0 are all individuals under age of 16 (not 18)
# acs.5 %>% filter(VETSTAT == 0) %>% arrange(desc(AGE))
# - No missing data
# acs.5 %>% filter(if_any(c(AGE, SEX, VETSTAT), is.na))

x.5.all = acs.5 %>%
  select(-PUMA) %>%
  # Try this: the 17 year olds aren't included 
  arrange(SERIAL, PERNUM) %>%
  # Bin ages
  mutate(
    age = cut(AGE, breaks = c(0, age.breaks, Inf), right = FALSE),
    # re-format for readability
    age = gsub('[\\[|\\)]', '', age),
    vet = case_match(
      VETSTAT,
      0 ~ 'tooyoung',
      1 ~ 'nonveteran',
      2 ~ 'veteran'
    ),
    sex = ifelse(SEX %in% 1, 'male', 'female')
  ) 

x.5 = x.5.all %>%
  select(SERIAL, PERNUM, PERWT, age, vet, sex) %>%
  # do we want this? are there extraneous combos?
  # shouldn't be any nonvet/vet age <18...
  complete(age, vet, sex) %>%
  arrange(sex, age, desc(vet)) %>%
  # Start filling in ones
  # NOTE: the veteran data is all for universe 18+
  # so they should ONLY COUNT TOWARDS THE GRAND TOTAL
  # and for everything else they count as zero
  mutate(
    # grand total
    total = 1,
    # for 18+ total
    total.18p = as.numeric(!(age %in% '0,18')),
    # for sex
    sex.ones = as.numeric(!(age %in% '0,18')),
    sex.cols = sex,
    # for vet status
    vet.ones = as.numeric(!(age %in% '0,18')),
    vet.cols = vet
  ) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(names_from = vet.cols, values_from = vet.ones, values_fill = 0) %>%
  # Get combinations
  mutate(
    # sex-by-vet
    sex.vet.ones = as.numeric(!(age %in% '0,18')),
    sex.vet.sex.cols = sex,
    sex.vet.vet.cols = vet,
    # sex-by-age
    sex.age.ones = as.numeric(!(age %in% '0,18')),
    sex.age.sex.cols = sex,
    sex.age.age.cols = age,
    # sex-age-vet status
    sex.age.vet.ones = as.numeric(!(age %in% '0,18')),
    sex.age.vet.sex.cols = sex,
    sex.age.vet.age.cols = age,
    sex.age.vet.vet.cols = vet
  ) %>%
  pivot_wider(
    names_from = c(sex.vet.sex.cols, sex.vet.vet.cols), names_sep = '_', 
    values_from = sex.vet.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.sex.cols, sex.age.age.cols), 
    names_sep = '_', values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.vet.sex.cols, sex.age.vet.age.cols, sex.age.vet.vet.cols),
    names_sep = '_', values_from = sex.age.vet.ones, values_fill = 0
  ) %>%
  filter(!is.na(SERIAL) | !is.na(PERNUM)) %>%
  arrange(SERIAL, PERNUM) %>%
  select(-contains('tooyoung')) %>%
  select(-contains('0,18'))
  
data.frame(
  in.x = names(x.5)[-(1:6)],
  in.y = y.5 %>% filter(TRACT %in% 2001) %>% select(is.grand.total, total.18p, sex, age, vet)
)
# holy shit did I nail it on the first try? lmao

as.matrix(x.5[,-(1:6)]) %>% apply(1, sum) # some 4s, some 2s?
# OH... male/females under 18 counted somehow...?

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Do the PMEDM

# total PUMA population size
NN5 = y.5 %>% filter(is.grand.total) %>% pull(E) %>% sum()
# PUMS sample size
nn5 = nrow(x.5)
# number of tracts
JJ5 = y.5 %>% distinct(TRACT) %>% nrow()
# number of constraints
kk5 = y.5 %>% filter(TRACT %in% 2002) %>% nrow()

# response (constraints)
yy5 = y.5$E / NN5
# margins of error
vv5 = (y.5$M * nn5 / (NN5^2)) %>%
  .sparseDiagonal(n = length(.)) %>% 
  as('generalMatrix')

# prior design weights
qq5 = x.5 %>%
  pull(PERWT) %>%
  rep(each = JJ5) %>%
  (\(x) x / sum(x)) %>%
  matrix(ncol = 1)

# PUMS data
xx5 = kronecker(t(as.matrix(x.5[, -(1:6)])), .sparseDiagonal(n = JJ5)) %>%
  t() %>%
  as('dgCMatrix')

# Model
pp5 = PMEDM_solve(xx5, yy5, vv5, qq5)
# whoa

data.frame(
  obsv = y.5$E,
  pred = round(pp5$pred * NN5, 3)
) %>%
  plot()

# hoo yeah!!

y.5.p.vartype = y.5 %>%
  mutate(pred = pp5$pred * NN5) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>%
  select(-tf)
  

y.5.p.vartype %>%
  mutate(in.mar = pred < E + M & pred > E - M) %>%
  # mutate(i = 1:nrow(.)) %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
  geom_point(aes(y = pred, colour = vartype, shape = in.mar), size = 3) +
  scale_shape_manual(values = c(21, 19))

# heck yeah

y.5.p.vartype %>% 
  filter(pred > E + M | pred < E - M) %>%
  select(-c(form, code, universe, var_desc)) %>%
  select(E, M, pred, everything()) 
# wow - 100% in the margins of error!!
# amazing!

pred.allocate = matrix(
  pp5$p, byrow = TRUE, nrow = nn5, ncol = JJ5, 
  dimnames = list(paste(x.5$SERIAL, x.5$PERNUM, sep = '.'), unique(y.5$TRACT))
) %>%
  as.data.frame() %>%
  mutate(i = row.names(.)) %>%
  pivot_longer(-i, names_to = 'tract', values_to = 'n') %>%
  mutate(n = n * NN5) %>%
  separate_wider_delim(i, names = c('SERIAL', 'PERNUM'), delim = '.') %>%
  merge(y = x.5.all %>% select(SERIAL, PERNUM, AGE, vet, sex))

pred.allocate %>%
  group_by(AGE, vet, sex, tract) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = AGE, y = n, group = interaction(vet, sex))) +
  geom_point(aes(colour = interaction(vet, sex))) +
  facet_wrap(~ tract)

pred.allocate %>%
  mutate(age.bin = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  filter(vet %in% 'veteran') %>%
  group_by(tract, age.bin) %>%
  summarise(n = sum(n)) %>%
  ggplot(aes(x = age.bin, y = n)) +
  geom_col() +
  facet_wrap(~ tract)

# - NEXT STEPS
# - - Look at what happens in this model if removing the under-18s and how that changes other estiamte (old code on old model below)
# - - Figure out how to split on PUMA and perform PUMAs separately (or try all pumas at once? Not entirely sure how this would work...)
# - - non-white veteran data

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Try removing the grand total columns to see what happens

# Format y
y.5.z = y.5 %>% filter(!is.grand.total) %>% select(-is.grand.total)

# Format x
x.5.z = x.5 %>% select(-total) %>% filter(!(age %in% '0,18'))

# total PUMA population size
NZ5 = y.5.z %>% filter(total.18p) %>% pull(E) %>% sum()
# PUMS sample size
nz5 = nrow(x.5.z)
# number of tracts
JZ5 = y.5.z %>% distinct(TRACT) %>% nrow()
# number of constraints
kz5 = y.5.z %>% filter(TRACT %in% 2002) %>% nrow()

# response (constraints)
yz5 = y.5.z$E / NZ5
# margins of error
vz5 = (y.5.z$M * nz5 / (NZ5^2)) %>%
  .sparseDiagonal(n = length(.)) %>% 
  as('generalMatrix')

# prior design weights
qz5 = x.5.z %>%
  pull(PERWT) %>%
  rep(each = JZ5) %>%
  (\(x) x / sum(x)) %>%
  matrix(ncol = 1)

# PUMS data
xz5 = kronecker(t(as.matrix(x.5.z[, -(1:6)])), .sparseDiagonal(n = JZ5)) %>%
  t() %>%
  as('dgCMatrix')

# Model
pz5 = PMEDM_solve(xz5, yz5, vz5, qz5)

y.5.compare = y.5 %>%
  mutate(pred.y = pp5$pred) %>%
  filter(!is.grand.total) %>%
  mutate(pred.z = pz5$pred) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>%
  select(-tf)

y.5.compare %>%
  ggplot(aes(x = pred.y * NN5, y = pred.z * NZ5)) +
  annotate('segment', x = 0, xend = 5000, y = 0, yend = 5000, linetype = 2) +
  geom_point(aes(colour = vartype), size = 4)
# oh?
# are they equal...?

with(y.5.compare, pred.y - pred.z)

with(y.5.compare, (NN5 * pred.y) - (NZ5 * pred.z))
# alrighty then, basically the same, pred.y is smaller but by <.1

y.5.compare %>%
  mutate(pred.y = pred.y * NN5, pred.z = pred.z * NZ5) %>%
  ggplot(aes(x = pred.y - pred.z)) +
  geom_histogram()
# a very small number of cases where there is a difference of 1-3
# and overall constraint does seem to be negative so pred.y is slightly smaller

# Oh wait... we're more interested in the the allocations themselves than the
# constraints being met...

x.5.compare = merge(
  x = matrix(
    pp5$p, nrow = nn5, ncol = JJ5, byrow = TRUE,
    dimnames = list(with(x.5, paste(SERIAL, PERNUM, sep = '_')), unique(y.5$TRACT))
  ) %>%
    as.data.frame() %>%
    mutate(id = row.names(.)) %>%
    pivot_longer(-id, names_to = 'tract', values_to = 'w') %>%
    mutate(w = w * NN5),
  y = matrix(
    pz5$p, nrow = nz5, ncol = JZ5, byrow = TRUE,
    dimnames = list(with(x.5.z, paste(SERIAL, PERNUM, sep = '_')), unique(y.5.z$TRACT))
  ) %>%
    as.data.frame() %>%
    mutate(id = row.names(.)) %>%
    pivot_longer(-id, names_to = 'tract', values_to = 'w') %>%
    mutate(w = w * NZ5),
  by = c('id', 'tract'), all.x = FALSE, all.y = FALSE, suffixes = c('.y', '.z')
)

x.5.compare %>% summarise(rmsd = sqrt(sum((w.z - w.y)^2)))
# holy moly, root mean squared difference of .28 that is so small

x.5.compare %>% arrange(desc(abs(w.y - w.z)))
# wow a MAX DIFFERENCE of 0.4
# INSANE
# okay so there really isn't much marginal benefit (if any?) to using the total
# population counts

# ---------------------------------
# ---------------------------------
# ---------------------------------
# # RESOLVED TROUBLESHOOTING CODE BELOW #
# # issue was TRACTs were not aligned properly in Y
# # each constraint needs to roll through 
# # need to arrange/rowsort by ALL constraint vars and have TRACT in dead last place (or at least that worked here)
# # also: accidentally sorted the veteran status ascending instead of descending (easy fix)
# y.5 %>%
#   mutate(pred = pp5$pred * NN5) %>%
#   pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
#   filter(tf) %>%
#   select(-tf) %>%
#   ggplot(aes(x = E, y = pred)) +
#   annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
#   geom_point(aes(colour = vartype), size = 3)
# 
# # okay our grand totals are spot on
# # everything else looking super shitty
# 
# y.5.p.vartype = y.5 %>%
#   mutate(pred = pp5$pred * NN5) %>%
#   pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
#   filter(tf) %>%
#   select(-tf)
# 
# y.5.p.vartype %>%
#   mutate(i = 1:nrow(.)) %>%
#   mutate(in.mar = pred < E + M & pred > E - M) %>%
#   ggplot(aes(x = i)) +
#   geom_segment(aes(xend = i, y = E - M, yend = E + M), linewidth = 0.2) +
#   geom_point(aes(y = pred, colour = vartype, shape = in.mar)) +
#   scale_shape_manual(values = c(19, 21))
# 
# y.5.p.vartype %>%
#   mutate(in.mar = pred < E + M & pred > E - M) %>%
#   ggplot(aes(x = E)) +
#   annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
#   geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
#   geom_point(aes(y = pred, colour = vartype, shape = in.mar), size = 3) +
#   scale_shape_manual(values = c(19, 21))
# 
# # looks to me like:
# # - grand totals are great
# # - worst to me are maybe the sex.age and sex.age.vet (might just be because there are more of them though)
# 
# y.5.p.vartype %>%
#   group_by(vartype) %>%
#   summarise(pin.mar = mean(pred < E + M & pred > E - M)) %>%
#   arrange(pin.mar)
# 
# # hmm... no clear pattern...
# 
# y.5.p.vartype %>%
#   mutate(i = 1:nrow(.)) %>%
#   ggplot(aes(x = i, y = TRACT)) +
#   geom_point(aes(colour = vartype))
# 
# ###
# ### okay... hot idea... what if we only ran on the universe of individuals 18+
# ###
# 
# # Filter out individuals 18+ and remove grand total col
# x.5.z = x.5 %>%
#   select(-total) %>%
#   filter(!age %in% '0,18')
# 
# # Give me constraints that do not include the grand total
# y.5.z = y.5 %>% filter(!is.grand.total) %>% select(-is.grand.total)
# 
# # total PUMA population size
# NZ5 = y.5.z %>% filter(total.18p) %>% pull(E) %>% sum()
# # PUMS sample size
# nz5 = nrow(x.5.z)
# # number of tracts
# JZ5 = y.5.z %>% distinct(TRACT) %>% nrow()
# # number of constraints
# kz5 = y.5.z %>% filter(TRACT %in% 2002) %>% nrow()
# 
# # response (constraints)
# yz5 = y.5.z$E / NZ5
# # margins of error
# vz5 = (y.5.z$M * nz5 / (NZ5^2)) %>%
#   .sparseDiagonal(n = length(.)) %>% 
#   as('generalMatrix')
# 
# # prior design weights
# qz5 = x.5.all %>%
#   filter(AGE > 17) %>%
#   arrange(SERIAL, PERNUM) %>%
#   pull(PERWT) %>%
#   rep(each = JZ5) %>%
#   (\(x) x / sum(x)) %>%
#   matrix(ncol = 1)
# 
# # PUMS data
# xz5 = kronecker(t(as.matrix(x.5.z[, -(1:5)])), .sparseDiagonal(n = JZ5)) %>%
#   t() %>%
#   as('dgCMatrix')
# 
# # Model
# pz5 = PMEDM_solve(xz5, yz5, vz5, qz5)
# 
# y.5.z %>%
#   mutate(pred = pz5$pred * NZ5) %>%
#   pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
#   filter(tf) %>%
#   select(-tf) %>%
#   ggplot(aes(x = E, y = pred)) +
#   annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
#   geom_point(aes(colour = vartype), size = 3)
# 
# # lmao still looks like dook
# 
# y.5.z %>%
#   mutate(pred = pz5$pred * NZ5) %>%
#   # filter(TRACT < 2002) %>%
#   pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
#   filter(tf) %>%
#   select(-tf) %>%
#   ggplot(aes(x = E, y = pred)) +
#   annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
#   geom_point(aes(colour = vartype), size = 3) +
#   facet_wrap(~ TRACT) +
#   theme(legend.position = 'top')
# 
# # Heh... not seeing a clear overall pattern here...
# 
# y.5 %>%
#   mutate(pred.y = pp5$pred * NN5) %>%
#   filter(!is.grand.total) %>%
#   pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
#   filter(tf) %>%
#   select(-tf) %>%
#   mutate(pred.z = pz5$pred * NZ5) %>%
#   ggplot(aes(x = pred.y, y = pred.z)) +
#   annotate('segment', x = 0, xend = 5000, y = 0, yend = 5000, linetype = 2) +
#   geom_point(aes(colour = vartype))
# # huh... so rank ordering is the same, but the new estimates are all a little bit higher

