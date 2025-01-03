###############################################################
# Script for getting estimates of people living in households #
###############################################################

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
acs.ddi = read_ipums_ddi('01_raw_data/usa_00029.xml')

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
    OWNERSHP, OWNCOST, RENTGRS, GQ,
    SEX, AGE
  )

# Read in and subset NHGIS data
tab.raw = read_nhgis('01_raw_data/nhgis0023_csv.zip', file_select = 1) %>% 
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

### Format y
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
  mutate(univ = ifelse(form %in% 'AQM4', 'total', 'in.housing')) %>%
  # Some label scrubbing
  mutate(
    var_label = gsub('[Uu]nder', '0 to', var_label),
    var_label = gsub('\\soccupied$', '', var_label)
  ) %>%
  # Classify constraints
  mutate(
    grand.total = var_label %in% 'total' & univ %in% 'total',
    house.total = var_label %in% 'total' & univ %in% 'in.housing',
    sex.total   = grepl('(fe)?male$', var_label),
    sex.age     = grepl('(fe)?male\\:', var_label),
    owner.rent  = !(var_label %in% 'total') & univ %in% 'in.housing'
  ) %>%
  # Get constraint info
  mutate(
    sex = case_when(
      sex.total ~ var_label,
      sex.age ~ gsub('\\:.+$', '', var_label),
      .default = NA
    ),
    age = ifelse(sex.age, gsub('^.+\\:\\s(\\d{1,2}).+', '\\1', var_label), NA),
    age = as.numeric(age),
    own = ifelse(owner.rent, var_label, NA)
  ) %>%
  arrange(
     desc(grand.total), desc(house.total), desc(sex.total), desc(sex.age), desc(owner.rent),
    sex, age, own, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

age.bins = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% pull() %>% sort()

### Format X

x.m.all = acs.m %>%
  mutate(
    age = cut(AGE, breaks = c(age.bins, Inf), right = FALSE),
    age = as.numeric(gsub('\\[(\\d{1,2})\\,.+', '\\1', age)),
    own = case_match(
      OWNERSHP,
      0 ~ 'nonhousing',
      1 ~ 'owner',
      2 ~ 'renter'
    ),
    sex = ifelse(SEX > 1, 'female', 'male')
  )

# desc(grand.total), desc(house.total), desc(sex.total), desc(sex.age), desc(owner.rent),
# sex, age, own, TRACT

x.m = x.m.all %>%
  select(PUMA, YEAR, SERIAL, PERNUM, PERWT, sex, age, own) %>%
  arrange(sex, age, own) %>%
  mutate(
    grand.total = 1,
    house.total = as.numeric(!(own %in% 'nonhousing')),
    sex.ones = 1,
    sex.cols = sex,
    sex.age.ones = 1,
    sex.age.sex  = sex,
    sex.age.age  = age,
    own.ones = as.numeric(!(own %in% 'nonhousing')),
    own.cols = own
  ) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.age.sex, sex.age.age), names_sep = "_",
    values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(names_from = own.cols, values_from = own.ones, values_fill = 0) %>%
  select(-contains('nonhousing')) %>%
  arrange(PUMA, YEAR, SERIAL, PERNUM)

names(x.m)[-(1:8)]
y.m %>% filter(TRACT %in% 101) %>% nrow()

data.frame(
  x = names(x.m)[-(1:8)],
  y = y.m %>% filter(TRACT %in% 101) %>% select(sex, age, own, vartype)
)
# parfait
#

###############################
# -------- Model fit ---------#
###############################

home.fit = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  .f = function(x, y) {
    
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
    X = kronecker(t(as.matrix(x[, -(1:8)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
    
  }
)

### Compare constraints

home.cons = map2_df(
  .x = home.fit,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'grand.total']))
) 

home.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(xend = E, y = E-M, yend = E+M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 2) +
  facet_wrap(~ PUMA)
# heyo

with(home.cons, mean(pred < E + M & pred > E - M))

##############################
# -------- Allocate ---------#
##############################

home.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = home.fit,
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

home.allos = map2(
  .x = home.allos,
  .y = split(y.m, ~ PUMA),
  function(p.matrix, y.data) {
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
)

home.allos = home.allos %>%
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

home.allos

x.rent.allos = merge(
  x.m.all, home.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

### Compile into final table

home.table1 = x.rent.allos %>%
  filter(OWNERSHP > 0) %>%
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup() %>%
  mutate(table.code = 'HOM')

home.table1 %>% head()

home.table1 %>% complete(tract, age.out) %>% filter(is.na(allo))
# have all groups represented


write.csv(
  home.table1, row.names = FALSE,
  '03_downscale_out/in-household_raw.csv'
)
