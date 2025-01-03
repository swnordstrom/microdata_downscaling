################################################################
# Script for getting estimates of people living alone (by age) #
################################################################

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
acs.ddi = read_ipums_ddi('01_raw_data/usa_00030.xml')

# Read in ACS sample and subset
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Multnomah County, Oregon
  # filter(PUMA %in% unique(tra.m$PUMA5CE)) %>%
  filter(STATEFIP %in% 41, COUNTYFIP %in% 51) %>%
  select(-c(STATEFIP, COUNTYFIP)) %>%
  mutate(PUMA = gsub('^13', '51', PUMA)) %>%
  # Select columns of interest
  select(
    YEAR, PUMA, SERIAL, PERNUM, PERWT, HHWT,
    FAMUNIT, SEX, AGE
  )

# Read in and subset NHGIS data
tab.raw = read_nhgis('01_raw_data/nhgis0021_csv.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))

# Merge these together
tab.m = tab.raw %>%
  # Merge in PUMA data
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())


###############################
# -------- Data prep ---------#
###############################

y.m = tab.m %>%
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  merge(
    ipums_var_info(tab.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc)
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
  # Get universe to distinguish data types (more succinct than var_desc)
  mutate(univ = gsub('.+\\(universe\\:\\s(.+)\\)$', '\\1', var_desc)) %>%
  select(-var_desc) %>%
  # Label scrubbing to make greps easier
  mutate(
    # change 'under' to '0 to' for ages
    var_label = gsub('under', '0 to', var_label),
    # get rid of 'households' in var_labels (redundant)
    var_label = gsub('\\shousehold(s)?', '', var_label)
  ) %>%
  # Classify constraints
  mutate(
    person.total = var_label %in% 'total' & grepl('pop', univ),
    househ.total = var_label %in% 'total' & grepl('hou', univ),
    sex.total = grepl('(fe)?male$', var_label),
    sex.age   = grepl('(fe)?male\\:', var_label),
    family.type = grepl('family$', var_label),
    family.size = grepl('family\\:', var_label)
  ) %>%
  # Extract variables
  mutate(
    sex = case_when(
      sex.total ~ var_label,
      sex.age ~ gsub('\\:.+$', '', var_label),
      .default = NA
    ),
    age = ifelse(
      sex.age,
      gsub('.+\\:\\s(\\d{1,2}).+', '\\1', var_label),
      NA
    ),
    age = as.numeric(age),
    family = case_when(
      family.type ~ var_label,
      family.size ~ gsub('\\:.+$', '', var_label),
      .default = NA
    ),
    hhsize = ifelse(
      family.size,
      gsub('\\D+\\:\\s', '', var_label),
      NA
    )
  ) %>%
  # Order columns
  arrange(
    desc(person.total), desc(househ.total), desc(sex.total), desc(sex.age),
    desc(family.type), desc(family.size),
    sex, age, family, hhsize, TRACT
  ) %>%
  # Convert logicals to a variable type column
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

age.breaks = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% arrange(age) %>% pull()

### Format X

x.m.all = acs.m %>%
  mutate(
    sex = ifelse(SEX > 1, 'female', 'male'),
    age = cut(AGE, c(age.breaks, Inf), right = FALSE),
    age = gsub('\\[(\\d{1,2}).+', '\\1', age)
  ) %>%
  # Any household (Year-Puma-Serial combo) with more than one member of a family
  # is a 'family household' by definition
  group_by(YEAR, PUMA, SERIAL) %>%
  mutate(
    hhsize = paste0(ifelse(n() < 7, n(), '7+'), '-person'),
    is.fam = ifelse(any(duplicated(FAMUNIT)), 'family', 'nonfamily')
  ) %>%
  ungroup()

# desc(person.total), desc(househ.total), desc(sex.total), desc(sex.age),
# desc(family.type), desc(family.size),
# sex, age, family, hhsize

x.m = x.m.all %>%
  # Arranging
  arrange(sex, age, is.fam, hhsize) %>%
  complete(sex, age, nesting(is.fam, hhsize)) %>%
  # Create columns to pivot
  # NOTE: individuals will only count towards the household constraints if they
  # are person number 1
  mutate(
    person.total = 1,
    househ.total = as.numeric(PERNUM %in% 1),
    # sex
    sex.ones = 1,
    sex.cols = sex,
    # sex-age
    sex.age.ones = 1,
    sex.age.sex  = sex,
    sex.age.age  = age,
    # family
    fam.ones = as.numeric(PERNUM %in% 1),
    fam.cols  = is.fam,
    # family-household size
    fam.size.ones = as.numeric(PERNUM %in% 1),
    fam.size.fam  = is.fam,
    fam.size.size = hhsize
  ) %>%
  # Pivot
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.age.sex, sex.age.age), names_sep = '_',
    values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(names_from = fam.cols, values_from = fam.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(fam.size.fam, fam.size.size), names_sep = '_',
    values_from = fam.size.ones, values_fill = 0
  ) %>%
  filter(!is.na(SERIAL)) %>%
  arrange(YEAR, SERIAL, PUMA, PERNUM)

names(x.m)[-(1:13)] %>% length()
y.m %>% filter(TRACT %in% 101) %>% nrow()

data.frame(
  x = names(x.m)[-(1:13)],
  y = y.m %>% filter(TRACT %in% 101) %>% select(sex, age, family, hhsize, vartype)
)
# nailed it

###############################
# -------- Model fit ---------#
###############################

alone.fit = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  .f = function(x, y) {
    
    # # # define variables
    # total universe size
    N = y %>% filter(vartype %in% 'person.total') %>% pull(E) %>% sum()
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
    X = kronecker(t(as.matrix(x[, -(1:13)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
    
  }
)

### Compare constraints

alone.cons = map2_df(
  .x = alone.fit,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'person.total']))
) 

alone.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(xend = E, y = E-M, yend = E+M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 2) +
  facet_wrap(~ PUMA)
# heyo

##############################
# -------- Allocate ---------#
##############################

alone.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = alone.fit,
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

alone.allos = map2(
  .x = alone.allos,
  .y = split(y.m, ~ PUMA),
  function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(TRACT) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$PUMA[1], tracts, sep = '_')
    
    # Normalize by population size
    pop.size = y.data %>% filter(vartype %in% 'person.total') %>% pull(E) %>% sum()
    p.matrix = p.matrix * pop.size
    
    # Return matrix
    return(p.matrix)
  }
)

alone.allos = alone.allos %>%
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

alone.allos

x.alone.allos = merge(
  x.m.all, alone.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

# Get quantities of interest

alone.table1 = x.alone.allos %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  filter(grepl('^1', hhsize)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

# Look for missing groups
alone.table1 %>% complete(tract, age.out) %>% filter(is.na(allo))
# nice - the whole gang is here

alone.table1 = alone.table1 %>% mutate(table.code = 'ALO')

write.csv(
  alone.table1, row.names = FALSE,
  '03_downscale_out/living-alone_raw.csv'
)
