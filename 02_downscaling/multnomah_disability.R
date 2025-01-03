################################################################
# Script for getting number of people with disability (by age) #
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
acs.ddi = read_ipums_ddi('01_raw_data/usa_00039.xml')

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
    YEAR, PUMA, SERIAL, PERNUM, PERWT,
    SEX, AGE, RACE, HISPAN, OCC, GQ, contains("_DIS")
  )

# Read in and subset NHGIS data
tab1.raw = read_nhgis('01_raw_data/nhgis0022_csv.zip', file_select = 1) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
tab2.raw = read_nhgis('01_raw_data/nhgis0022_csv.zip', file_select = 2) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
# Race/ethnicity data (downloaded for other table)
rhp.raw = read_nhgis('01_raw_data/nhgis_race_hisp.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  # Remove Pov/Income ratio vars
  select(-starts_with('AQ68'))

# Merge these together
tab.m = merge(tab1.raw, tab2.raw, by = 'TRACTA') %>%
  merge(rhp.raw %>% select(TRACTA, starts_with('AQ')), by = 'TRACTA') %>%
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
    rbind(
      ipums_var_info(tab1.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(tab2.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(rhp.raw,  matches('[EM]\\d{3}$')) %>% select(var_name, var_label, var_desc)
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
  # Start scrubbing variables for greping
  mutate(
    var_label = gsub ('[Uu]nder', '0 to', var_label),
    univ = gsub('(people\\swho\\sare\\s)?', '', univ),
    univ = gsub('\\salone', '', univ),
    univ = gsub('\\spopulation', '', univ)
  ) %>%
  # Designate variable categories
  mutate(
    vargroup = case_when(
      grepl('total', univ) ~ 'total',
      grepl('civilian', univ) ~ 'dis',
      grepl('hispan', univ) ~ 'hisp',
      .default = 'race'
    )
  ) %>%
  # Categorize the constraints
  mutate(
    # 'duniv' is 'disabled universe' (noninstitutionalized civilians)
    grand.total = grepl('total', var_label) & vargroup %in% 'total',
    duniv.total = grepl('total', var_label) & vargroup %in% 'dis',
    rac.total = grepl('total', var_label) & vargroup %in% 'race', 
    hsp.total = grepl('total', var_label) & vargroup %in% 'hisp',
    sex.total   = grepl('(fe)?male$', var_label) & vargroup %in% 'total',
    sex.duniv   = grepl('(fe)?male$', var_label) & vargroup %in% 'dis',
    sex.rac = grepl('(fe)?male$', var_label) & vargroup %in% 'race',
    sex.hsp = grepl('(fe)?male$', var_label) & vargroup %in% 'hisp',
    sex.ages    = grepl('(fe)?male\\:', var_label) & vargroup %in% 'total',
    sex.aged    = grepl('(fe)?male\\:[^\\:]+$', var_label) & vargroup %in% 'dis',
    sex.rac.age = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label) & vargroup %in% 'race', 
    sex.hsp.age = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label) & vargroup %in% 'hisp',
    sex.aged.dis = grepl('(fe)?male\\:.+\\:', var_label) & vargroup %in% 'dis'
  ) %>%
  # Scrape out variables
  mutate(
    sex = case_when(
      sex.total | sex.duniv | sex.rac | sex.hsp ~ var_label,
      sex.ages | sex.aged | sex.rac | sex.hsp | sex.aged.dis | sex.rac.age | sex.hsp.age ~ gsub('^([^\\:]+)\\:.+', '\\1', var_label),
      .default = NA
    ),
    # Age columns
    ages = ifelse(
      sex.ages,
      gsub('(fe)?male\\:\\s(\\d{1,2}).+', '\\2', var_label),
      NA
    ),
    aged = ifelse(
      sex.aged | sex.aged.dis,
      gsub('(fe)?male\\:\\s(\\d{1,2}).+', '\\2', var_label),
      NA
    ),
    ager = ifelse(sex.rac.age, gsub('.+\\:\\s(\\d{1,2}).+', '\\1', var_label), NA),
    ageh = ifelse(sex.hsp.age, gsub('.+\\:\\s(\\d{1,2}).+', '\\1', var_label), NA),
    # Race column
    rac = ifelse(vargroup %in% 'race', univ, NA),
    # Hispanic column
    hsp = case_when(
      vargroup %in% 'hisp' & !grepl('not', univ) ~ 'hispanic',
      vargroup %in% 'hisp' & grepl('not', univ) ~ 'nonhispanic white',
      .default = NA
    ),
    # disability status
    dis = ifelse(
      sex.aged.dis,
      gsub('^[^\\:]+\\:[^\\:]+\\:\\s(.+)$', '\\1', var_label),
      NA
    ),
    across(c(ages, aged, ager, ageh), as.numeric),
  ) %>%
  # Collapse asian and pacific islander into one category
  mutate(rac = ifelse(rac %in% 'asian' | grepl('islander', rac), 'aanhpi', rac)) %>%

# %>%
  group_by(
    TRACT, PUMA, 
    grand.total, duniv.total, rac.total, hsp.total, sex.total, sex.duniv, 
    sex.rac, sex.hsp, sex.ages, sex.aged, sex.rac.age, sex.hsp.age, sex.aged.dis, 
    sex, ages, aged, ager, ageh, rac, hsp, dis
  ) %>%
  summarise(E = sum(E), M = sqrt(sum(M^2))) %>%
  ungroup() %>%
  # Arrange rows
  arrange(
    desc(grand.total), desc(duniv.total), desc(rac.total), desc(hsp.total),
    desc(sex.total), desc(sex.duniv), desc(sex.rac), desc(sex.hsp), desc(sex.ages), 
    desc(sex.aged), desc(sex.rac.age), desc(sex.hsp.age), desc(sex.aged.dis),
    sex, ages, aged, ager, ageh, rac, hsp, dis, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

aged.breaks = y.m %>% distinct(aged) %>% arrange(aged) %>% filter(!is.na(aged)) %>% pull()
ages.breaks = y.m %>% distinct(ages) %>% arrange(ages) %>% filter(!is.na(ages)) %>% pull()
ager.breaks = y.m %>% distinct(ager) %>% arrange(ager) %>% filter(!is.na(ager)) %>% pull()
ageh.breaks = y.m %>% distinct(ageh) %>% arrange(ageh) %>% filter(!is.na(ageh)) %>% pull()

### Now, format X:

x.m.all = acs.m %>%
  # Convert disability info to long
  pivot_longer(matches('US20\\d{2}A\\_DIS'), names_to = 'disyear', values_to = 'DIS') %>%
  filter(as.numeric(gsub('[^0-9]', '', disyear)) == YEAR) %>%
  select(-disyear) %>%
  # make variables compatible with y
  mutate(
    sex = ifelse(SEX > 1, 'female', 'male'),
    aged = cut(AGE, breaks = c(aged.breaks, Inf), right = FALSE),
    ages = cut(AGE, breaks = c(ages.breaks, Inf), right = FALSE),
    ager = cut(AGE, breaks = c(ager.breaks, Inf), right = FALSE),
    ageh = cut(AGE, breaks = c(ageh.breaks, Inf), right = FALSE),
    across(c(ages, aged, ager, ageh), ~ as.numeric(gsub('\\[(\\d{1,2}).+', '\\1', .))),
    # occupation codes: https://usa.ipums.org/usa/volii/occ2018.shtml
    in.duniv = !((GQ %in% 3) | (OCC >= 9800 & OCC < 9900)),
    rac = case_match(
      RACE,
      1 ~ 'white',
      2 ~ 'black.or.african american',
      3 ~ 'american.indian.alaska native',
      4:6 ~ 'aanhpi',
      7 ~ 'some.other.race',
      8:9 ~ 'two.or.more.races'
    ),
    hsp = case_when(
      HISPAN > 0 ~ 'hisp',
      rac %in% 'white' ~ 'nhwh',
      .default = 'nhnw'
    ),
    dis = ifelse(DIS %in% '2', 'not.disabled', 'with.disability')
  ) %>%
  # Get rid of a couple unnecessary variables
  select(-c(OCC, GQ))

# desc(grand.total), desc(duniv.total), desc(rac.total), desc(hsp.total),
# desc(sex.total), desc(sex.duniv), desc(sex.rac), desc(sex.hsp), desc(sex.ages), 
# desc(sex.aged), desc(sex.age.rac), desc(sex.age.hsp), desc(sex.aged.dis),
# sex, ages, aged, ager, ageh, rac, hsp, dis, TRACT

x.m = x.m.all %>%
  arrange(sex, ages, aged, ager, ageh, rac, hsp, dis) %>%
  complete(sex, nesting(ages, aged, ager, ageh), nesting(rac, hsp), dis) %>%
  # Make columns to pivot
  mutate(
    grand.total = 1,
    duniv.total = as.numeric(in.duniv),
    rac.ones  = 1,
    rac.cols  = rac,
    hsp.ones  = 1,
    hsp.cols  = hsp,
    sexa.ones = 1,
    sexa.cols = sex,
    sexd.ones = as.numeric(in.duniv),
    sexd.cols = paste0(sex, '.dis'),
    sex.rac.ones  = 1,
    sex.rac.sex   = sex,
    sex.rac.rac   = rac,
    sex.hsp.ones  = 1,
    sex.hsp.sex   = sex,
    sex.hsp.hsp   = hsp,
    sex.ages.ones = 1,
    sex.ages.sex  = sex,
    sex.ages.age  = ages,
    sex.aged.ones = as.numeric(in.duniv),
    sex.aged.sex  = paste0(sex, '.dis'),
    sex.aged.aged = aged,
    sex.age.rac.ones = 1,
    sex.age.rac.sex  = sex,
    sex.age.rac.age  = ager,
    sex.age.rac.rac  = rac,
    sex.age.hsp.ones = 1,
    sex.age.hsp.sex  = sex,
    sex.age.hsp.age  = ageh,
    sex.age.hsp.hsp  = hsp,
    sex.aged.dis.ones = as.numeric(in.duniv),
    sex.aged.dis.sex  = sex,
    sex.aged.dis.aged = aged,
    sex.aged.dis.dis  = dis
  ) %>%
  # Pivot out columns
  pivot_wider(names_from = rac.cols,  values_from = rac.ones,  values_fill = 0) %>%
  pivot_wider(names_from = hsp.cols,  values_from = hsp.ones,  values_fill = 0) %>%
  pivot_wider(names_from = sexa.cols, values_from = sexa.ones, values_fill = 0) %>%
  pivot_wider(names_from = sexd.cols, values_from = sexd.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.rac.sex, sex.rac.rac), names_sep = '_',
    values_from = sex.rac.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.hsp.sex, sex.hsp.hsp), names_sep = '_',
    values_from = sex.hsp.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.ages.sex, sex.ages.age), names_sep = '_',
    values_from = sex.ages.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.aged.sex, sex.aged.aged), names_sep = '_',
    values_from = sex.aged.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.rac.sex, sex.age.rac.age, sex.age.rac.rac), names_sep = '_',
    values_from = sex.age.rac.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.hsp.sex, sex.age.hsp.age, sex.age.hsp.hsp), names_sep = '_',
    values_from = sex.age.hsp.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.aged.dis.sex, sex.aged.dis.aged, sex.aged.dis.dis), names_sep = '_',
    values_from = sex.aged.dis.ones, values_fill = 0
  ) %>%
  # get rid of extra rows (combos from complete() to alphebetize)
  filter(!is.na(SERIAL)) %>%
  # get rid of all columns for non-hispanic non-whites (nhnw)
  select(-contains('nhnw')) %>%
  # arrange names
  arrange(YEAR, SERIAL, PERNUM)

names(x.m)[-(1:19)]

data.frame(
  x = names(x.m)[-(1:19)],# [121:180],
  y = y.m %>% filter(TRACT %in% 101) %>% select(sex, ages, aged, ager, ageh, rac, hsp, dis, vartype)# %>% slice(121:180)
) %>%
  sample_n(30)


##############################
# -------- Fit mods ---------#
##############################

dis.fit = map2(
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
    X = kronecker(t(as.matrix(x[, -(1:19)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
    
  }
)

# Visualize constraints
dis.cons = map2_df(
  .x = dis.fit,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'grand.total']))
) 

dis.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(xend = E, y = E-M, yend = E+M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), shape = 21, size = 2) +
  facet_wrap(~ PUMA)

dis.cons %>%
  group_by(PUMA, vartype) %>%
  summarise(pinmar = mean(pred < E + M & pred > E - M)) %>%
  pivot_wider(names_from = vartype, values_from = pinmar)
# lol - one lonely combination

##############################
# -------- Allocate ---------#
##############################


dis.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = dis.fit,
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

dis.allos = map2(
  .x = dis.allos,
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

dis.allos = dis.allos %>%
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

dis.allos

x.dis.allos = merge(
  x.m.all, dis.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

# Get quantities of interest:

dis.table1 = x.dis.allos %>%
  filter(DIS %in% '1') %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

dis.table1 %>% complete(age.out, tract) %>% filter(is.na(allo))
# great - all age-tract combinations here

dis.table1 %>% pivot_wider(names_from = age.out, values_from = allo)

# Nice - everything is here.

# Now, BIPOC-only table

dis.table2 = x.dis.allos %>%
  filter(DIS %in% '1', !(hsp %in% 'nhwh')) %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

dis.table2 %>% complete(age.out, tract) %>% filter(is.na(allo))
# looks like we have full coverage even for BIPOC

dis.table2 %>% pivot_wider(names_from = age.out, values_from = allo)

### Export

dis.table1 = dis.table1 %>% mutate(table.code = 'DIS')
dis.table2 = dis.table2 %>% mutate(table.code = 'DIS_POC')

write.csv(
  dis.table1, row.names = FALSE,
  '03_downscale_out/disability_raw.csv'
)

write.csv(
  dis.table2, row.names = FALSE,
  '03_downscale_out/disability_bipoc_raw.csv'
)
