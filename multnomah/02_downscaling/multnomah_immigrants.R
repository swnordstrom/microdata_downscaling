##########################################################
# Script for getting estimates of people by nativity
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
acs.ddi = read_ipums_ddi('01_raw_data/usa_00027.xml')

# Read in ACS sample and subset
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Multnomah County, Oregon
  filter(STATEFIP %in% 41, COUNTYFIP %in% 51) %>%
  select(-c(STATEFIP, COUNTYFIP)) %>%
  mutate(PUMA = gsub('^13', '51', PUMA)) %>%
  # Select columns of interest
  select(
    YEAR, PUMA, SERIAL, PERNUM, PERWT,
    SEX, AGE, BPL, CITIZEN
  )

# whoops... downloaded 2017 data too

# Read in and subset NHGIS data
tab1.raw = read_nhgis('01_raw_data/nhgis0019_csv.zip', file_select = 1) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
tab2.raw = read_nhgis('01_raw_data/nhgis0019_csv.zip', file_select = 2) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))

# Combine into a single NHGIS table
tab.m = merge(tab1.raw, tab2.raw, by = 'TRACTA') %>%
  # Merge in PUMA data
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())


#####################################
# -------- Processing data ---------#
#####################################

# Both tables for y have the same universe (total population)

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
  # Get rid of redundant constraints
  # Get rid of redundant 'total' column
  filter(!(grepl('nativity', var_desc) & !grepl('\\:', var_label))) %>%
  # Get rid of age-nativity (only) constraint
  # sex+age(n)
  filter(!(grepl('(fe)?male\\:[^\\:]+$', var_label) & form %in% 'AQY6'))  %>%
  # Change 'under' to to '0 to' for gsubs
  mutate(var_label = gsub('[Uu]nder', '0 to', var_label)) %>%
  # Start classifying
  mutate(
    # overall total
    grand.total = grepl('total', var_label),
    # sex total
    sex.total = grepl('(fe)?male$', var_label),
    # sex+age(s)
    sex.ages = grepl('(fe)?male\\:', var_label) & form %in% 'AQM4',
    # sex+age(n)+nativity
    sex.agen.natv = grepl('(fe)?male\\:[^\\:]+\\:[^\\:]+$', var_label),
    # sex+age(n)+nativity+citizen
    sex.agen.natv.ctzn = grepl('(fe)?male\\:[^\\:]+\\:[^\\:]+\\:.+$', var_label)
  ) %>%
  # Start extracting variables
  mutate(
    sex = case_when(
      sex.total ~ var_label, 
      !grand.total ~ gsub('^([^\\:]+)\\:.+', '\\1', var_label),
      .default = NA
    ),
    ages = ifelse(sex.ages, gsub('^(fe)?male\\:\\s(\\d{1,2}).+', '\\2', var_label), NA),
    agen = ifelse(
      sex.agen.natv | sex.agen.natv.ctzn, 
      gsub('^(fe)?male\\:\\s(\\d{1,2}).+', '\\2', var_label), 
      NA
    ),
    across(c(ages, agen), as.numeric),
    natv = ifelse(
      sex.agen.natv | sex.agen.natv.ctzn,
      gsub('(fe)?male\\:[^\\:]+\\:\\s(\\w+).*', '\\2', var_label),
      NA
    ),
    ctzn = ifelse(
      sex.agen.natv.ctzn,
      gsub('.+\\:\\s(\\w+)[^\\:]+$', '\\1', var_label),
      NA
    )
  ) %>%
  # Sort rows
  arrange(
    desc(grand.total), desc(sex.total), desc(sex.ages),
    desc(sex.agen.natv), desc(sex.agen.natv.ctzn),
    sex, ages, agen, natv, ctzn, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

# Get age bins
ages.breaks = y.m %>% distinct(ages) %>% filter(!is.na(ages)) %>% arrange(ages) %>% pull()

# Now do X

x.m.all = acs.m %>%
  mutate(
    sex = ifelse(SEX > 1, 'female', 'male'),
    ages = cut(AGE, c(ages.breaks, Inf), right = FALSE),
    ages = gsub('\\[(\\d{1,2}).+', '\\1', ages) %>% as.numeric(),
    agen = ifelse(AGE < 18, 0, 18),
    natv = ifelse(BPL <= 120 | CITIZEN %in% 1, 'native', 'foreign'),
    ctzn = case_match(
      CITIZEN,
      2 ~ 'naturalized',
      c(0, 3:5) ~ 'not',
      .default = NA
    )
  )

# desc(grand.total), desc(sex.total), desc(sex.ages), desc(sex.agen), 
# desc(sex.agen.natv), desc(sex.agen.natv.ctzn),
# sex, ages, agen, natv, ctzn, TRACT

x.m = x.m.all %>%
  # Sort rows
  arrange(sex, ages, agen, natv, ctzn) %>%
  # Form columns for pivoting
  mutate(
    total = 1,
    sex.ones = 1,
    sex.cols = sex,
    sex.ages.ones = 1,
    sex.ages.sex  = sex,
    sex.ages.ages = ages,
    sex.agen.natv.ones = 1,
    sex.agen.natv.sex  = sex,
    sex.agen.natv.agen = agen,
    sex.agen.natv.natv = natv,
    sex.agen.natv.ctzn.ones = 1,
    sex.agen.natv.ctzn.sex  = sex,
    sex.agen.natv.ctzn.agen = agen,
    sex.agen.natv.ctzn.natv = natv,
    sex.agen.natv.ctzn.ctzn = ctzn
  ) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = c(sex.ages.sex, sex.ages.ages), names_sep = '_',
    values_from = sex.ages.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.agen.natv.sex, sex.agen.natv.agen, sex.agen.natv.natv),
    names_sep = '_', values_from = sex.agen.natv.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(
      sex.agen.natv.ctzn.sex, sex.agen.natv.ctzn.agen, 
      sex.agen.natv.ctzn.natv, sex.agen.natv.ctzn.ctzn
    ),
    names_sep = '_', values_from = sex.agen.natv.ctzn.ones, values_fill = 0
  ) %>%
  select(-c(sex, ages, agen, natv, ctzn)) %>%
  arrange(YEAR, SERIAL, PERNUM) %>%
  # remove columns with info beyond citizenship
  select(-contains('native_', ignore.case = FALSE))

names(x.m)[-(1:9)] %>% length()
y.m %>% filter(TRACT %in% 102) %>% nrow()

data.frame(
  inx = names(x.m)[-(1:9)],
  iny = y.m %>% filter(TRACT %in% 102) %>% select(sex, ages, agen, natv, ctzn, vartype)
)
# awesome


#########################################
# -------- Running downscaling ---------#
#########################################

imm.fits = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  function(x, y) {
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
    X = kronecker(t(as.matrix(x[, -(1:9)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  }
)


### Look at constraints

imm.cons = map2_df(
  .x = imm.fits,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(y$E[y$vartype %in% 'grand.total']))
) 

imm.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(xend = E, y = E-M, yend = E+M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), size = 2, shape = 21) +
  facet_wrap(~ PUMA) +
  theme(legend.position = 'bottom')

# Spot on

################################
# --------Allocations ---------#
################################

# Build sample-tract matrices
imm.allos = map2(
  .x = split(x.m, ~ PUMA), 
  .y = imm.fits, 
  .f = function(x.data, mod.fit) {
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

# Merge in tracts for sample-tract matrix colnames (and normalize)
imm.allos = map2(
  .x = imm.allos,
  .y = split(y.m, ~ PUMA),
  .f = function(p.matrix, y.data) {
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

imm.allos = imm.allos %>%
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
  mutate(across(where(is.character), as.numeric)) %>%
  # Merge in with x data
  merge(
    x.m.all %>% select(YEAR, SERIAL, PERNUM, AGE, natv),
    by.x = c('year', 'serial', 'pernum'),
    by.y = c('YEAR', 'SERIAL', 'PERNUM') 
  )

# Report requested allocations:

imm.table1 = imm.allos %>%
  # Get only immigrants
  filter(natv %in% 'foreign') %>%
  # Requested age bins
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc))

imm.table1 %>% 
  pivot_wider(names_from = age.out, values_from = allo) %>%
  filter(if_any(matches('[0-9]'), ~ is.na(.)))
# Nice - no NAs

imm.table1 = imm.table1 %>% mutate(table.code = 'IMM')

write.csv(
  imm.table1, row.names = FALSE,
  '03_downscale_out/immigration_raw.csv'
)
