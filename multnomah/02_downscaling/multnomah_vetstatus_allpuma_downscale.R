library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

##-@@@@@@@
##-@@@@@@@ Data readin
##-@@@@@@@

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
# Looking at the single-puma script, this actually maybe wasn't doing very much...
# pop.raw = read_nhgis('01_raw_data/2018-2022_ACS_tract_population_sizes.zip') %>% 
#   filter(STUSAB %in% 'OR', COUNTYA %in% '051')

# Merge together NHGIS tabular data (with tracts) with PUMA data
tab.m = tab.raw %>% select(TRACTA, starts_with('AQ')) %>%
# merge(
  # tab.raw %>% select(TRACTA, starts_with('AQ')),
  # pop.raw %>% select(TRACTA, starts_with('AQ'))
# ) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

head(tab.m)
names(tab.m)

table(acs.m$PUMA)
table(tab.m$PUMA)

##-@@@@@@@
##-@@@@@@@ Process Y data
##-@@@@@@@

y.m = tab.m %>%
  # Pivot variable columns to rows and merge in variable information
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  # Merging in variable info
  merge(ipums_var_info(tab.raw, starts_with('AQ')) %>% select(-val_labels)) %>%
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
  ### Establish total columns
  mutate(
    total.18p = grepl('total', var_label),
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
  ) %>%
  # select(-c(form, code, TRACT, PUMA)) %>%
  # distinct(var_label, .keep_all = TRUE) %>%
  arrange(
    desc(total.18p), desc(total.sex), desc(total.vet), 
    desc(sex.vet), desc(sex.age), desc(sex.age.vet), sex, age, desc(vet),
    PUMA, TRACT
  ) %>% 
  # get rid of unnecessary columns
  select(-c(form, code, var_desc)) %>%
  # Pivot out variable type
  pivot_longer(where(is.logical), names_to = 'vargroup', values_to = 'tf') %>%
  filter(tf) %>% 
  select(-tf)

head(y.m)

# Get age breaks
age.breaks = y.m %>%
  filter(!is.na(age)) %>%
  distinct(age) %>%
  pull(age) %>%
  gsub('(\\d{1,2})\\,\\w+', '\\1', .) %>%
  as.numeric()

age.breaks

##-@@@@@@@
##-@@@@@@@ Process X data
##-@@@@@@@

x.m.all = acs.m %>%
  # Remove 0-17 year olds (not in unive)
  filter(AGE > 17) %>%
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

x.m = x.m.all %>%
  select(PUMA, SERIAL, PERNUM, PERWT, age, vet, sex) %>%
  complete(age, vet, sex) %>%
  arrange(sex, age, desc(vet)) %>%
  # Start filling in ones
  # (prior version of this script included 0-18 from the PUMS and thus had the
  # ones set to as.logical(age not 0,18))
  mutate(
    # for 18+ total
    total.18p = 1,
    # for sex
    sex.ones = 1,
    sex.cols = sex,
    # for vet status
    vet.ones = 1,
    vet.cols = vet
  ) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(names_from = vet.cols, values_from = vet.ones, values_fill = 0) %>%
  # Get combinations
  mutate(
    # sex-by-vet
    sex.vet.ones = 1,
    sex.vet.sex.cols = sex,
    sex.vet.vet.cols = vet,
    # sex-by-age
    sex.age.ones = 1,
    sex.age.sex.cols = sex,
    sex.age.age.cols = age,
    # sex-age-vet status
    sex.age.vet.ones = 1,
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
  arrange(PUMA, SERIAL, PERNUM)


##-@@@@@@@
##-@@@@@@@ Wrapper function for model fit
##-@@@@@@@

pmedm.prep.fit = function(x, y) {
  
  # # # define variables
  # total PUMA population size
  N = y %>% filter(vargroup %in% 'total.18p') %>% pull(E) %>% sum()
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
  # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED
  X = kronecker(t(as.matrix(x[, -(1:7)])), .sparseDiagonal(n = J)) %>%
    t() %>%
    as('dgCMatrix')
  # prior weights
  q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
  
  return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  
}

##-@@@@@@@
##-@@@@@@@ Run the model routine
##-@@@@@@@

# use purrr's `map2` function
all.p = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  pmedm.prep.fit
)

##-@@@@@@@
##-@@@@@@@ Evaluate fits
##-@@@@@@@

# # Look at how well we're matching the tabular constraints

all.constr = map2_df(
  .x = all.p,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) {
    y %>% mutate(pred = out.fit$pred)
    # N = y %>% filter(vargroup %in% 'total.18p') %>% pull(E) %>% sum()
    # return(
    #   data.frame(
    #     obsv = y$E,
    #     pred = out.fit$pred * N,
    #     name = y$PUMA
    #   )
    # )
  }
)


all.constr = all.constr %>%
  group_by(PUMA) %>%
  mutate(pred = pred * sum(E[vargroup %in% 'total.18p'])) %>%
  ungroup()

all.constr %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
  geom_point(aes(y = pred, colour = vargroup), size = 3) +
  facet_wrap(~ PUMA)

all.constr %>% 
  group_by(PUMA) %>%
  summarise(in.mar = mean(pred < E + M & pred > E - M))

# wowza!!!! over 99.5% coverage in all PUMAs

# # Look at the allocations...

# Slight challenge here: can we get the tracts in for each PUMA?
# map2 will only take two arguments
# best solution off the top of my head is inelegant but let's try it

all.allocations = map2_df(
  .x = all.p,
  .y = split(x.m, ~ PUMA),
  function(out.fit, x) {
    matrix(
      out.fit$p, nrow = nrow(x), byrow = TRUE,
      dimnames = list(with(x, paste(SERIAL, PERNUM, sep = '_')))
    ) %>%
      as.data.frame() %>%
      mutate(id = row.names(.), PUMA = x$PUMA) %>%
      pivot_longer(-c(id, PUMA), names_to = 'tractid', values_to = 'allo')
  }
)

all.allocations = merge(
  all.allocations,
  y.m %>%
    distinct(TRACT, PUMA) %>%
    group_by(PUMA) %>%
    mutate(tractid = paste0('V', 1:n())) %>%
    ungroup()
) %>%
  select(-tractid)
