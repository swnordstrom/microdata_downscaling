##########################################################
# Script for getting estimates of people living in institutions
##########################################################

library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

# Clear da namespace
rm(list = ls())

#############################
# -------- Readins ---------#
#############################

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
tra.m = read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  select(-STATEFP, COUNTYFP)

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00036.xml')

# Read in ACS sample and subset
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Select columns of interest
  select(
    YEAR, PUMA, SERIAL, PERNUM, PERWT, HHWT, 
    SEX, AGE, GQ, GQTYPE,
    STATEFIP, COUNTYFIP
  ) %>%
  filter(STATEFIP %in% 41, COUNTYFIP %in% 51) %>%
  select(-c(STATEFIP, COUNTYFIP)) %>%
  mutate(PUMA = gsub('^13', '51', PUMA))

# acs.extra = read_ipums_ddi('01_raw_data/usa_00038.xml') %>%
#   # Read in data
#   read_ipums_micro() %>% 
#   # Subset to just Multnomah County, Oregon
#   filter(PUMA %in% unique(tra.m$PUMA5CE)) %>%
#   # Select columns of interest
#   select(
#     YEAR, PUMA, SERIAL, PERNUM, PERWT, HHWT, 
#     SEX, AGE, GQ,
#     STATEFIP, COUNTYFIP
#   ) %>%
#   filter(STATEFIP %in% 41, COUNTYFIP %in% 51) %>%
#   select(-c(STATEFIP, COUNTYFIP)) %>%
#   mutate(PUMA = gsub('^13', '^51', PUMA))

# Read in NHGIS tabular data
# need to do this in two steps bec ause the file_select argument doesn't work to
# read in multiple files...
tab1.raw = read_nhgis('01_raw_data/nhgis0015_csv.zip', file_select = 1) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
tab2.raw = read_nhgis('01_raw_data/nhgis0015_csv.zip', file_select = 2) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))

# Merge together NHGIS tabular data (with tracts) with PUMA data
tab.m = merge(tab1.raw, tab2.raw, by = 'TRACTA') %>%
  # merge(
  # tab.raw %>% select(TRACTA, starts_with('AQ')),
  # pop.raw %>% select(TRACTA, starts_with('AQ'))
  # ) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

head(tab.m)

##############################################
# -------- Format data for routine ----------#
##############################################

# Format y
y.m = tab.m %>%
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  merge(
    rbind(
      ipums_var_info(tab1.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(tab2.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc)
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
  # Classify constraint types
  mutate(
    # grand total: variable name is 'total' and universe is total population
    grand.total = var_label %in% 'total' & grepl('universe\\:\\stotal\\spopulation', var_desc),
    # total population in group quarters
    quart.total = grepl('quarters', var_desc),
    # Total within sex has sex with no info after i.e. no colon
    sex.total = grepl('(fe)?male$', var_label),
    sex.age = grepl('(fe)?male\\:\\s', var_label)
  ) %>%
  # swap 'under' to zero for age pulling
  mutate(var_label = gsub('[Uu]nder', '0 to', var_label)) %>%
  # Extract variable categories
  mutate(
    sex = ifelse(sex.total | sex.age, gsub('\\:.*', '', var_label), NA),
    age = ifelse(sex.age, gsub('(fe)?male\\:\\s(\\d{1,2}).*', '\\2', var_label), NA),
    age = ifelse(!is.na(sex.age), as.numeric(age), NA)
  ) %>% 
  # Now, time to sort rows
  arrange(desc(grand.total), desc(quart.total), desc(sex.total), desc(sex.age), sex, age, TRACT) %>%
  # Finally, turn the logicals into variable types
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>%
  select(-tf)

# Get age cuts
age.cuts = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% arrange() %>% pull(age)

# Format X
x.m.all = acs.m %>%
  mutate(
    # group.quar = ifelse(GQ %in% 3:4, 'gq', 'nongq'),
    sex = ifelse(SEX %in% 1, 'male', 'female'),
    age.s = cut(AGE, breaks = c(age.cuts, Inf), right = FALSE),
    age.s = as.numeric(gsub('\\[(\\d{1,2})\\,\\w{1,3}\\)', '\\1', age.s))
  ) %>%
  arrange(YEAR, SERIAL, PERNUM) %>%
  select(-c(HHWT, GQTYPE))

# Generate X matrix for model fitting routine
x.m = x.m.all %>%
  arrange(sex, age.s) %>%
  mutate(
    total = 1,
    group = as.numeric(GQ %in% 3:4),
    sex.ones = 1,
    sex.cols = sex,
    sex.age.ones = 1,
    sex.age.sex.cols = sex,
    sex.age.age.cols = age.s
  ) %>%
  # pivot out 
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.age.sex.cols, sex.age.age.cols), names_sep = '_',
    values_from = sex.age.ones, values_fill = 0
  ) %>%
  arrange(YEAR, SERIAL, PERNUM) %>%
  select(-c(SEX, AGE, sex, age.s))


# Check for alignment:
ncol(x.m) - 6
y.m %>% filter(TRACT %in% min(TRACT)) %>% nrow()
# same number of rows so far...

data.frame(
  x = names(x.m)[-(1:6)],
  y = y.m %>% filter(TRACT %in% min(TRACT)) %>% select(sex, age, vartype)
)
# and away we go...

#################################
# -------- Fit models ----------#
#################################

# Wrapper function
pmedm.prep.fit = function(x, y) {
  
  # # # define variables
  # total universe size
  N = y %>% filter(vartype %in% 'grand.total') %>% pull(E) %>% sum()
  # PUMS sample size
  n = nrow(x)
  # Number of tracts
  J = y %>% distinct(TRACT) %>% nrow()
  
  # # # Prepare response and margins of error
  # Response
  Y = y$E / N
  # Error matrix
  v = (y$M * n / (N^2)) %>%
    .sparseDiagonal(n = length(.)) %>% 
    as('generalMatrix')
  
  # # # Prepare PUM data
  # Prepare X matrix
  # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED (correct here)
  X = kronecker(t(as.matrix(x[, -(1:6)])), .sparseDiagonal(n = J)) %>%
    t() %>%
    as('dgCMatrix')
  # prior weights
  q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
  
  return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  
}

# Fit the model
all.fit = map2(
    .x = split(x.m, ~ PUMA),
    .y = split(y.m, ~ PUMA),
    pmedm.prep.fit
)

# Extract model constraint fits
all.constr = map2_df(
  .x = all.fit,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred)
) 

all.constr = all.constr %>%
  group_by(PUMA) %>%
  mutate(pred = pred * sum(E[vartype %in% 'grand.total'])) %>%
  ungroup()

all.constr %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 7000, y = 0, yend = 7000, linetype = 2) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 3) +
  facet_wrap(~ PUMA)

# Cool, good, etc.

all.constr %>%
  group_by(PUMA) %>%
  summarise(p.in.mar = mean(pred < E + M & pred > E - M))
# all constraints within MOE, very nice

# Now... to extract model estimates from this...

extract.fun = function(x.data, mod.fit) {
  # x.data: ACS matrix used for extraction
  # mod.fit: model object (i.e. what is returned pmedm)
  
  matr = matrix(
    data = mod.fit$p, nrow = nrow(x.data), byrow = TRUE,
    # NOTE the year column here... maybe it would be a good idea to make a
    # flexible 'id' column
    dimnames = list(with(x.data, paste(YEAR, SERIAL, PERNUM, sep = '_')), NULL)
  )
  
  return(matr)
  
}

normalize.label.fun = function(p.matrix, y.data) {
  # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
  # as-yet-unlabelled tracts)
  # y.data: constraint table, with a column for tract (TRACT)
  
  # Assign tract names
  tracts = y.data %>% distinct(TRACT) %>% pull()
  dimnames(p.matrix)[[2]] = paste(y.data$PUMA[1], tracts, sep = '_')
  
  # Normalize by population size
  pop.size = y.data %>% filter(vartype %in% 'grand.total') %>% pull(E) %>% sum()
  p.matrix = p.matrix * pop.size
  
  # Return matrix
  return(p.matrix)
}

all.alloc = map2(.x = split(x.m, ~ PUMA), .y = all.fit, .f = extract.fun)
all.alloc = map2(.x = all.alloc, .y = split(y.m, ~ PUMA), .f = normalize.label.fun)

all.alloc = all.alloc %>%
  lapply(
    FUN = function(m) {
      data.frame(m) %>%
        mutate(id = row.names(.)) %>%
        pivot_longer(-id, names_to = 'puma_tract', values_to = 'alloc') %>%
        separate_wider_delim(puma_tract, delim = '_', names = c('puma', 'tract'))
    }
  ) %>%
  do.call(what = rbind) %>%
  mutate(puma = gsub('X', '', puma)) %>%
  separate_wider_delim(id, delim = '_', names = c('year', 'serial', 'pernum')) %>%
  mutate(across(where(is.character), as.numeric))

head(all.alloc)

alloc.df = merge(
  x.m.all, all.alloc,
  by.x = c('YEAR', 'SERIAL', 'PERNUM', 'PUMA'),
  by.y = c('year', 'serial', 'pernum', 'puma'),
) %>%
  select(-age.s)

alloc.df %>%
  filter(GQ %in% 3:4) %>%
  group_by(PUMA, tract, GQ = ifelse(GQ %in% 3, 'inst', 'noninst')) %>%
  summarise(estimated.n = sum(alloc)) %>%
  pivot_wider(names_from = GQ, values_from = estimated.n) %>%
  ggplot(aes(x = inst, y = noninst)) +
  geom_text(aes(label = tract))

# 73: looks like it has a correctional program, so this tracks
# 5602 is Portland State, 4002 is University of Portland - also tracks

inst.table1 = alloc.df %>%
  # subset to people in institutional settings
  filter(GQ %in% 3) %>%
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

inst.table1 %>%
  pivot_wider(names_from = age.out, values_from = allo) %>%
  filter(if_any(matches('[0-9]'), ~ is.na(.)))
# nice - gang is all here

inst.table1 = inst.table1 %>%
  mutate(table.code = 'INS')

### Export!
write.csv(
  inst.table1, row.names = FALSE,
  '03_downscale_out/institutionalized_raw.csv'
)

#################################
#################################
#################################
# OLD ###########################
#################################
#################################

#################################
# -------- Fit models ----------#
#################################

# Try it again, but with *just* the group quarters data

x.g = x.m %>% filter(group > 0) %>% select(-c(total, contains('male')))
y.g = y.m %>% filter(vartype %in% 'quart.total')


# Fit:
group.fit = map2(
  .x = split(x.g, ~ PUMA),
  .y = split(y.g, ~ PUMA),
  function(x, y) {
    # # # define variables
    # total universe size
    N = y %>% pull(E) %>% sum()
    # PUMS sample size
    n = nrow(x)
    # Number of tracts
    J = y %>% distinct(TRACT) %>% nrow()
    
    # # # Prepare response and margins of error
    # Response
    Y = y$E / N
    # Error matrix
    v = (y$M * n / (N^2)) %>%
      .sparseDiagonal(n = length(.)) %>% 
      as('generalMatrix')
    
    # # # Prepare PUM data
    # Prepare X matrix
    # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED (correct here)
    X = kronecker(t(as.matrix(x[, -(1:6)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  }
)

# Model component fits (probably trivially easy...)
group.constr = map2_df(
  .x = group.fit,
  .y = split(y.g, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(y$E))
) 

with(group.constr, mean(pred < E + M & pred > E - M))
# all in margins of error, very cool

group.alloc = map2(.x = split(x.g, ~ PUMA), .y = group.fit, .f = extract.fun)

group.alloc = map2(
  .x = group.alloc,
  .y = split(y.g, ~ PUMA),
  .f = function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(TRACT) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$PUMA[1], tracts, sep = '_')
    
    # Normalize by population size
    p.matrix = p.matrix * sum(y.data$E)
    
    # Return matrix
    return(p.matrix)
  }
)

group.alloc = group.alloc %>%
  lapply(
    FUN = function(m) {
      data.frame(m) %>%
        mutate(id = row.names(.)) %>%
        pivot_longer(-id, names_to = 'puma_tract', values_to = 'alloc') %>%
        separate_wider_delim(puma_tract, delim = '_', names = c('puma', 'tract'))
    }
  ) %>%
  do.call(what = rbind) %>%
  mutate(puma = gsub('X', '', puma)) %>%
  separate_wider_delim(id, delim = '_', names = c('year', 'serial', 'pernum')) %>%
  mutate(across(where(is.character), as.numeric))


alloc.compare = merge(
  all.alloc, group.alloc,
  all.x = FALSE, all.y = TRUE,
  by = c('year', 'serial', 'pernum', 'puma', 'tract'),
  suffixes = c('.all', '.group')
)

alloc.compare = merge(
  x.m.all, alloc.compare,
  all.x = FALSE, all.y = TRUE,
  by.x = c('YEAR', 'SERIAL', 'PERNUM', 'PUMA'), 
  by.y = c('year', 'serial', 'pernum', 'puma')
)

alloc.compare %>%
  mutate(AGE = as.numeric(AGE)) %>%
  ggplot(aes(x = alloc.all, y = alloc.group)) +
  annotate('segment', x = 0, xend = 70, y = 0, yend = 70, linetype = 2) +
  geom_point(aes(colour = AGE), size = 3) +
  scale_colour_viridis_c()

# woof, looks quite bad
# pattern difficult to discern

# Hmm.

alloc.compare %>%
  group_by(PUMA, tract) %>%
  summarise(across(c(alloc.all, alloc.group), sum))
# No surprise, there are more or less the same

alloc.compare %>%
  mutate(GQ = factor(GQ)) %>%
  ggplot(aes(x = alloc.all, y = alloc.group, colour = GQ)) +
  geom_point() +
  facet_wrap(~ PUMA)

alloc.compare %>% 
  filter(tract %in% 4002) %>% 
  mutate(GQ = factor(GQ), AGE = as.numeric(AGE)) %>% 
  ggplot(aes(x = alloc.all, y = alloc.group, shape = GQ, fill = AGE)) + 
  scale_fill_viridis_c() +
  scale_shape_manual(values = 23:24) +
  geom_point(size = 4)

# Yeah... I have to think that the all-variables one is better
# It's using the age structure to assign the ~20 y/o individuals to the
# precincts full of college students...

# Using extra variables is good/useful! Makes it a worthy exercise to figure out
# best variables
