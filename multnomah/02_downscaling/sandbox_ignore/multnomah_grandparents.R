##########################################################
# Script for getting estimates of grandparents raising grandchildren
##########################################################

library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

#############################
# -------- Readins ---------#
#############################

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
tra.m = read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  select(-STATEFP, COUNTYFP)

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00035.xml')

# Read in ACS sample and subset
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Multnomah County, Oregon
  filter(COUNTYFIP %in% 51) %>%
  # Re-label PUMAs
  mutate(PUMA = gsub('^13', '51', PUMA)) %>%
  # Select columns of interest
  select(
    YEAR, PUMA, SERIAL, PERNUM, PERWT, 
    SEX, AGE, RELATE, MOMLOC, POPLOC, SPLOC
  )

# Read in NHGIS tabular data
tab.raw = read_nhgis('01_raw_data/2018-2022_ACS_grandparents_b10051.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')

# Merge together NHGIS tabular data (with tracts) with PUMA data
tab.m = tab.raw %>% select(TRACTA, matches('A\\w{3}[EM]\\d{3}')) %>%
  # merge(
  # tab.raw %>% select(TRACTA, starts_with('AQ')),
  # pop.raw %>% select(TRACTA, starts_with('AQ'))
  # ) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

################################
# -------- Formatting ---------#
################################

# Format Y
y.m = tab.m %>%
  # Pivot variable columns to rows and merge in variable information
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  # Merging in variable info
  merge(ipums_var_info(tab.raw, matches('A\\w{3}[EM]\\d{3}')) %>% select(var_name, var_label)) %>%
  # Convert variable info to lowercase for easier regexing
  mutate(var_label = tolower(var_label)) %>%
  # Split the variable name up so that we can pivot out estimates and margins
  # for same variable
  separate_wider_position(var_name, c(form = 4, me = 1, code = 3)) %>%
  mutate(
    var_label = gsub('estimates:\\s', '', var_label),
    var_label = gsub('margins\\sof\\serror\\:\\s', '', var_label)
  ) %>%
  pivot_wider(names_from = me, values_from = value) %>%
  mutate(
    is.grand.total = var_label %in% 'total',
    resp.total = grepl('resp', var_label) & !grepl('\\:', var_label),
    resp.pare = grepl('\\:\\s[a-z]', var_label) & !grepl('\\:\\s[0-9]', var_label),
    resp.pare.age = grepl('\\:\\s[0-9]', var_label),
  ) %>%
  mutate(
    # Code for responsibiility
    resp = case_when(
      grepl('grandparent\\sresp', var_label) ~ 'responsible',
      grepl('not\\sresp', var_label) ~ 'notresponsible',
      .default = NA
    ),
    # Code for parent present
    pare = case_when(
      grepl('no\\sparent', var_label) ~ 'noparent',
      grepl('other\\sgrand', var_label) ~ 'parent',
      .default = NA
    ),
    # Code for 
    age = ifelse(
      resp.pare.age,
      gsub('.*\\:\\s(\\d{1,2}).*', '\\1', var_label),
      NA
    )  
  ) %>%
  arrange(
    desc(is.grand.total), desc(resp.total), desc(resp.pare), desc(resp.pare.age), 
    desc(resp), desc(pare), age,
    PUMA, TRACT) %>%
  # for vargroup:
  pivot_longer(cols = where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>%
  select(-tf)

# get age cuts (could hard code in but this seems safer)
age.breaks = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% pull() %>% as.numeric()

### Format X

# Reading in missing age-PUMA combos
missing.age.combos = read.csv('02_downscaling/multnomah_missing_grandparents_age.csv') %>%
  select(PUMA, age.out)

# Just one age group (no <18 year old grandparents), missing from a few PUMAs
present.households = acs.m %>%
  # Give me individuals in the selected PUMAs
  filter(PUMA %in% setdiff(unique(acs.m$PUMA), missing.age.combos$PUMA)) %>%
  # Now, filter out households with grandparents age 85+ living with grandchildren
  group_by(YEAR, PUMA, SERIAL) %>%
  filter(
    # Give me individuals of the following characteristics:
    # Age 85+
    any(AGE > 84), 
    any(RELATE %in% 9 & AGE < 18) | (any(RELATE %in% 5:6) & any(RELATE %in% 3:4 & AGE < 18))
  ) %>%
  ungroup()

# Okay, going to do this in(?)-elegantly
pums.synthetic = merge(present.households, data.frame(PUMA = unique(acs.m$PUMA)), all = TRUE) %>%
  # The output of the merge has rows that are NA for all values except for PUMA
  # for the PUMAs for which we have no individuals
  # Using complete(), we'll get a data frame where synthetic individuals have all NA
  # except for PERWT (because it is left out of the nesting() call)
  complete(PUMA, nesting(YEAR, SERIAL, PERNUM, SEX, AGE, RELATE, MOMLOC, POPLOC, SPLOC)) %>%
  # Give us ONLY the rows that are NA for PERWT but non-SA for everything else (e.g., SERIAL)
  filter(is.na(PERWT), !is.na(SERIAL)) %>%
  # Assign a person-weight of one
  # negating serial... maybe this will still work
  mutate(
    SERIAL = -SERIAL,
    PERWT = 1
  ) %>%
  # Arrange columns to be same as original ACS
  select(names(acs.m))

# looks like ~35 synthetic individuals

x.m.all = rbind(acs.m, pums.synthetic) %>%
  group_by(YEAR, PUMA, SERIAL) %>%
  filter(
    # Condition 1: grandparent is head of house and at least one respondent is listed as grandchild
    # Condition 2: parent is householder and one of respondents is their parent
    any(RELATE %in% 9 & AGE < 18) | 
    (any(RELATE %in% 5:6) & any(RELATE %in% 3:4 & AGE < 18))
  ) %>%
  mutate(
    # Grandparent head of household as logical (head of household is responsible)
    grandp.resp = any(RELATE %in% 9 & AGE < 18),
    # Cases to designate: parent present/absent, grandparent is raising child
    parent.pres = any(RELATE %in% 5:6) | any(MOMLOC %in% 1 | POPLOC %in% 1)
  ) %>%
  # Subset to only grandparents
  mutate(
    grandpar = ifelse(
      # if the household has grandparent as the head
      grandp.resp, 
      # flag the household head *and* the spouse as grandparents
      (PERNUM %in% 1) | (SPLOC %in% 1), 
      # otherwise, give the individuals listed as parent/PIL of household head
      (RELATE %in% 5:6)
    )
  ) %>%
  ungroup() %>%
  filter(grandpar) %>%
  mutate(age.g = cut(AGE, breaks = c(0, sort(age.breaks), Inf), right = FALSE)) %>%
  mutate(age.g = gsub('[\\[|\\)]', '', age.g))


### There are issues in constructing X here
# more combinatorial fun
# y.m is just for sorting, x.m is where the sorting action occurs

x.m = x.m.all %>%
  select(YEAR, PUMA, SERIAL, PERNUM, PERWT, grandp.resp, parent.pres, age.g) %>%
  # re-code columns to text
  mutate(
    grandp.resp = ifelse(grandp.resp, 'responsible', 'notresponsible'),
    parent.pres = ifelse(parent.pres, 'parent', 'noparent')
  ) %>%
  arrange(desc(grandp.resp), desc(parent.pres), age.g) %>%
  # Start pivoting
  mutate(
    # Total
    total = 1,
    # Total for responsibility
    resp.ones = 1,
    resp.cols = grandp.resp, 
    # responsibility-parental presence combined
    resp.pare.ones = 1,
    resp.pare.resp = grandp.resp,
    resp.pare.pare = parent.pres,
    # Total for age-resp-pare
    resp.pare.ageg.ones = 1,
    resp.pare.ageg.resp = grandp.resp,
    resp.pare.ageg.pare = parent.pres,
    resp.pare.ageg.ageg = age.g
  ) %>%
  pivot_wider(names_from = resp.cols, values_from = resp.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(resp.pare.resp, resp.pare.pare), 
    names_sep = '_', values_from = resp.pare.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(resp.pare.ageg.resp, resp.pare.ageg.pare, resp.pare.ageg.ageg),
    names_sep = '_', values_from = resp.pare.ageg.ones, values_fill = 0
  ) %>%
  # Okay - not sure if it can be done with pivoting, so I will remove the not
  # responsible-parent combo
  select(-notresponsible_parent)
  
# ncol(x.m) - 7
# y.m %>% nrow() %>% (\(x) x / 197)

# ah... there's no parent notresponsible-parent combo
# okay... but do I remove those after in post-processing or before during pivoting?

data.frame(
  x = names(x.m)[-(1:8)],
  y = y.m %>% distinct(var_label, .keep_all = TRUE) %>% select(resp, pare, age)
)

# Aligned.
# And away we go...

###################################
# -------- Split and fit ---------#
###################################


### Define a function for model fitting

pmedm.prep.fit = function(x, y) {
  
  # # # define variables
  # total universe size
  N = y %>% filter(vartype %in% 'is.grand.total') %>% pull(E) %>% sum()
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
  X = kronecker(t(as.matrix(x[, -(1:8)])), .sparseDiagonal(n = J)) %>%
    t() %>%
    as('dgCMatrix')
  # prior weights
  q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
  
  return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  
}

# Let's see how this goes:
# use purrr's `map2` function
all.p = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  pmedm.prep.fit
)

# holy shit that was fast lmao

# Examine fits
all.constr = map2_df(
  .x = all.p,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred)
)

all.constr = all.constr %>%
  group_by(PUMA) %>%
  mutate(pred = pred * sum(E[vartype %in% 'is.grand.total'])) %>%
  ungroup()

all.constr %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 350, y = 0, yend = 350, linetype = 2) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 3) +
  facet_wrap(~ PUMA)
# pretty good
# a few ducks though...

all.constr %>%
  group_by(PUMA, vartype) %>%
  summarise(inmar = mean(pred < E + M & pred > E - M)) %>%
  pivot_wider(names_from = vartype, values_from = inmar)
# Mostly very good, only a couple off:
# - a few misses for the parents in 5103

all.constr %>% filter(!(pred < E+M & pred > E - M))
# possible some columns got switched?

x.m %>% filter(PUMA %in% 5103, parent.pres %in% 'noparent')
# hmm... missing from the PUMS I guess so nothing to extrapolate

x.m %>% filter(PUMA %in% 5101, grandp.resp %in% 'notresponsible')
# again - sampling variance, as all three cases here are over 60 (even though
# the table says there are ~4x as many cases of under 60 than over)

# interestingly these cases get allocated zero because they're missing from the PUMS
# are all subgroupings that are missing from the PUMS getting assigned zeros...?
# oh well check it later

#################################
# -------- Allocations ---------#
#################################

gpar.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = all.p,
  .f =  function(x.data, mod.fit) {
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
)

gpar.allos = map2(
  .x = gpar.allos,
  .y = split(y.m, ~ PUMA),
  function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(TRACT) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$PUMA[1], tracts, sep = '_')
    
    # Normalize by population size
    pop.size = y.data %>% filter(vartype %in% 'is.grand.total') %>% pull(E) %>% sum()
    p.matrix = p.matrix * pop.size
    
    # Return matrix
    return(p.matrix)
  }
)

gpar.allos = gpar.allos %>%
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

gpar.allos

x.gpar.allos = merge(
  x.m.all, gpar.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

### Split and do allocations

gpar.table1 = x.gpar.allos %>%
  filter(grandp.resp) %>%
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup() %>%
  mutate(table.code = 'GRP')

# Check for missing individuals

gpar.table1 %>% complete(age.out, tract) %>% filter(is.na(allo)) %>% filter(!grepl('^\\[0', age.out))

head(gpar.table1)

write.csv(
  gpar.table1, row.names = FALSE,
  '03_downscale_out/grandparents_raw.csv'
)
