##########################################################
# Script for getting estimates of people by housing cost #
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
    OWNERSHP, OWNCOST, RENTGRS, HHINCOME,
    SEX, AGE
  )

# Read in and subset NHGIS data
tab1.raw = read_nhgis('01_raw_data/nhgis0020_csv.zip', file_select = 1) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
tab2.raw = read_nhgis('01_raw_data/nhgis0020_csv.zip', file_select = 2) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))

# Merge these together
tab.m = merge(tab1.raw, tab2.raw, by = 'TRACTA') %>%
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
  # Get universe to distinguish data types (more succinct than var_desc)
  mutate(univ = gsub('.+\\(universe\\:\\s(.+)\\)$', '\\1', var_desc)) %>%
  select(-var_desc) %>%
  # Label scrubbing to make greps easier
  mutate(
    # get rid of 'occupied housing units' substr for neat greping
    var_label = gsub('\\-occupied\\shousing\\sunits', '', var_label),
    # get rid of commas to make cost easier to assess
    var_label = gsub('\\,', '', var_label),
    # change 'less than's to '0 to' for pct, 1 for income
    # (1 for income because there's a separate category for '0 to')
    # doing in two steps because there are dollars and percentages
    var_label = gsub('less\\sthan\\s\\$', '$1 to ', var_label),
    var_label = gsub('less\\sthan\\s([^\\$])', '0 to \\1', var_label),
    # change 'under' to '0 to' for ages
    var_label = gsub('under', '0 to', var_label)
  ) %>%
  # Classify variables
  mutate(
    person.total = var_label %in% 'total' & grepl('popul', univ),
    househ.total = var_label %in% 'total' & grepl('hous', univ),
    sex.total = grepl('(fe)?male$', var_label),
    sex.age   = grepl('\\:', var_label) & grepl('popul', univ),
    own.total = !grepl('\\:', var_label) & grepl('hous', univ) & !househ.total,
    own.inc   = grepl('^[a-z]+\\:[^\\:]+$', var_label) & grepl('hous', univ),
    own.inc.pct = grepl('percent', var_label)
  ) %>%
  # Extract variables
  mutate(
    sex = case_when(
      sex.total ~ var_label,
      sex.age ~ gsub('\\:.+', '', var_label),
      .default = NA
    ), 
    age = ifelse(sex.age, gsub('(fe)?male\\:\\s(\\d{1,2}).+', '\\2', var_label), NA),
    own = case_when(
      own.total ~ var_label,
      own.inc | own.inc.pct ~ gsub('([a-z]+)\\:.+', '\\1', var_label),
      .default = NA
    ),
    inc = case_when(
      own.inc & grepl('\\$', var_label) ~ gsub('^[a-z]+\\:\\s(\\$\\d+).+', '\\1', var_label),
      own.inc & !grepl('\\%', var_label) ~ gsub('^[a-z]+\\:\\s', '', var_label),
      own.inc.pct ~ gsub('^[a-z]+\\:\\s(\\$\\d+).+', '\\1', var_label),
      .default = NA
    ),
    pct = ifelse(own.inc.pct, gsub('.+\\:\\s(\\d{1,2})[^\\:]+$', '\\1', var_label), NA),
    # Convert age and pct to numeric
    across(c(age, pct), as.numeric)
  ) %>%
  # Sort rows
  arrange(
    desc(person.total), desc(househ.total), desc(sex.total), desc(sex.age),
    desc(own.total), desc(own.inc), desc(own.inc.pct),
    sex, age, own, inc, pct, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)
  
# Get age and pct breaks
age.breaks = y.m %>% distinct(age) %>% filter(!is.na(age)) %>% arrange(age) %>% pull()
pct.breaks = y.m %>% distinct(pct) %>% filter(!is.na(pct)) %>% arrange(pct) %>% pull()
inc.breaks = y.m %>% 
  distinct(inc) %>% 
  filter(grepl('\\d', inc)) %>%
  pull() %>% 
  gsub('\\$', '', .) %>% 
  as.numeric() %>% 
  sort()

### Now, format x

x.m.all = acs.m %>%
  # Estimate housing/income percentages
  mutate(
    housing.cost = case_match(
      OWNERSHP,
      1 ~ OWNCOST,
      2 ~ RENTGRS,
      .default = NA
    ),
    pct = ifelse(
      HHINCOME <= 0 | OWNERSHP < 1,
      NA,
      100 * housing.cost / (HHINCOME/12)
    ),
    # Going to change the 999... into NAs because they are annoying
    HHINCOME = ifelse(OWNERSHP < 1, NA, HHINCOME)
  ) %>%
  # Do cutting and other formatting
  mutate(
    sex = ifelse(SEX > 1, 'female', 'male'),
    age = cut(AGE, c(age.breaks, Inf), right = FALSE),
    own = case_match(
      OWNERSHP,
      1 ~ 'owner',
      2 ~ 'renter',
      .default = NA
    ),
    # inc = cut(HHINCOME, c(-Inf, inc.breaks, Inf), right = FALSE),
    # inc = ifelse(
    #   grepl('\\-Inf', inc),
    #   'zero.or.negative',
    #   gsub('\\[([^\\,]+)\\,.+$', '\\1', inc)
    # ),
    inc = case_when(
      HHINCOME <= 0 ~ 'zero.or.neg',
      housing.cost < 1 ~ 'no.cost',
      .default = cut(HHINCOME, c(inc.breaks, Inf), right = FALSE)
    ),
    inc = gsub('\\[([^\\,]+)\\,.+$', '\\1', inc),
    pct = cut(pct, c(pct.breaks, Inf), right = FALSE),
    across(c(age, pct), ~ as.numeric(gsub('\\[(\\d+)\\,.+$', '\\1', .)))
  )

# desc(person.total), desc(househ.total), desc(sex.total), desc(sex.age),
# desc(own.total), desc(own.inc), desc(own.inc.pct),
# sex, age, own, inc, pct, TRACT
  
x.m = x.m.all %>%
  complete(sex, age, nesting(own, inc, pct)) %>% 
  arrange(sex, age, own, inc, pct) %>%
  mutate(
    person.total = 1,
    househ.total = as.numeric(PERNUM %in% 1),
    sex.ones = 1,
    sex.cols = sex,
    sex.age.ones = 1,
    sex.age.sex  = sex,
    sex.age.age  = age,
    own.ones = as.numeric(PERNUM %in% 1),
    own.cols = own,
    own.inc.ones = as.numeric(PERNUM %in% 1),
    own.inc.own  = own,
    own.inc.inc  = inc,
    own.inc.pct.ones = as.numeric(PERNUM %in% 1),
    own.inc.pct.own  = own,
    own.inc.pct.inc  = inc,
    own.inc.pct.pct  = pct
  ) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.age.sex, sex.age.age), names_sep = '_',
    values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(names_from = own.cols, values_from = own.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(own.inc.own, own.inc.inc), names_sep = '_',
    values_from = own.inc.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(own.inc.pct.own, own.inc.pct.inc, own.inc.pct.pct),
    names_sep = '_', values_from = own.inc.pct.ones, values_fill = 0
  ) %>%
  filter(!is.na(YEAR)) %>%
  arrange(YEAR, SERIAL, PERNUM) %>%
  # Remove NA columns
  select(-contains('NA', ignore.case = FALSE)) %>%
  # remove no cost-0% column (redundant)
  select(-contains('no.cost_', ignore.case = FALSE))

ncol(x.m) - 18
y.m %>% filter(TRACT %in% 101) %>% nrow()

data.frame(
  x = names(x.m)[-(1:18)],
  y = y.m %>% filter(TRACT %in% 101) %>% select(sex, age, own, inc, pct, vartype)
)
# aligned!


###############################
# -------- Model fit ---------#
###############################

rent.fit = map2(
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
    X = kronecker(t(as.matrix(x[, -(1:18)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
   
  }
)

### Compare constraints

rent.cons = map2_df(
  .x = rent.fit,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'person.total']))
) 

rent.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(xend = E, y = E-M, yend = E+M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 2) +
  facet_wrap(~ PUMA)
# heyo

##############################
# -------- Allocate ---------#
##############################

rent.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = rent.fit,
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

rent.allos = map2(
  .x = rent.allos,
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

rent.allos = rent.allos %>%
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

rent.allos

x.rent.allos = merge(
  x.m.all, rent.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

# Get quantities of interest

rent.table1 = x.rent.allos %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  filter(pct %in% '30') %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc))

rent.table1 %>% 
  pivot_wider(names_from = age.out, values_from = allo) %>%
  filter(if_any(matches('[0-9]'), ~ is.na(.)))
# no NAs

rent.table1 = rent.table1 %>% mutate(table.code = 'HCO30')

write.csv(
  rent.table1, row.names = FALSE,
  '03_downscale_out/housing-cost_raw.csv'
)
