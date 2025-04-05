### Table for doing age-poverty-race/ethnicity downscaling
# Generating the following tables:
# - FPL (poverty line) tables
# - FPL tables for BIPOC
# - Race and ethnicity tables
# - Sex
# The data processing workflow is very ugly here because this is adapted from
# old scripts (see `multnomah_table1_downscale.R`)

####### == Setup
####### ==
####### ==

# Load packages
library(PMEDMrcpp)
library(ipumsr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Clear namespace
rm(list = ls())

### Read in data

# Get PUMA-tract list of interest (only in Oregon's Multnomah county)
pum.m = read.csv('01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  select(-c(STATEFP, COUNTYFP)) %>% 
  rename(PUMA = PUMA5CE)

# Import ACS (PUMS) metadata
acs.ddi = read_ipums_ddi('01_raw_data/usa_00022.xml')
# Read in sample and select relevant columns
acs.m   = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to Multnomah County
  filter(COUNTYFIP %in% 51) %>%
  # Change PUMAs to 2020 designation
  mutate(PUMA = gsub('^13', '51', PUMA)) %>%
  # Filter only relevant columns
  select(
    PUMA, YEAR, SERIAL, PERNUM, PERWT, CBSERIAL,
    GQ, SEX, AGE, RACE, HISPAN, contains('POV')
  )

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
  merge(pum.m %>% distinct(TRACTCE, PUMA), by.x = c('TRACTA'), by.y = c('TRACTCE')) %>%
  select(TRACT = TRACTA, PUMA, everything())

missing.combos = read.csv('02_downscaling/multnomah_missing_raceth_age.csv')


####### == Format and reconcile data
####### ==
####### ==

### Format y (tabular) data

y.m = tab.m %>%
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
  ######## HEY - ISSUE HERE
  # Age-poverty total column is superfluous (wait... no it isn't!!!!!!)
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
  group_by(TRACT, PUMA, is.grand.total, age.pov, pov, sex, age.sex, rac, hsp) %>%
  summarise(E = sum(E), M = sqrt(sum(M^2))) %>%
  ungroup() %>%
  # Need to arrange rows now
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

pov.breaks = y.m %>%
  distinct(pov) %>%
  filter(grepl('\\d', pov)) %>% 
  mutate(pov = gsub('^(\\d{0,1}\\.{0,1}\\d{0,2}).*', '\\1', pov)) %>%
  mutate(pov = as.numeric(pov)) %>%
  arrange(pov) %>%
  pull(pov)

age.s.breaks = y.m %>%
  distinct(age.sex) %>%
  filter(grepl('\\d', age.sex)) %>%
  mutate(age.sex = gsub('^(\\d{1,2}).*', '\\1', age.sex)) %>%
  mutate(age.sex = as.numeric(age.sex)) %>%
  arrange(age.sex) %>%
  pull(age.sex)

age.p.breaks = y.m %>%
  distinct(age.pov) %>%
  filter(grepl('\\d', age.pov)) %>%
  mutate(age.pov = gsub('^(\\d{1,2}).*', '\\1', age.pov)) %>%
  mutate(age.pov = as.numeric(age.pov)) %>%
  arrange(age.pov) %>%
  pull(age.pov)

### Format x data

# First, need to fix get missing age-race/eth combos
# line below will invert the missing combos data frame to get PUMS where the
# missing age/race combos *are* present
present.combos = missing.combos %>%
  mutate(missing = TRUE) %>%
  complete(PUMA, nesting(age.out, table.code), fill = list(missing = FALSE)) %>%
  filter(!missing) %>%
  select(-missing)

# Now, to merge in the PUMS data to get records matching above
# LOL this is going to be extremely ugly because of the way I arranged the script...
# lots of this is doing some text processing stuff
pums.synthetic = merge(
  acs.m %>%
    mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
    merge(
      read.csv('01_raw_data/5acs22_orwa_reldpri.csv') %>%
        mutate(
          CBSERIAL = gsub('GQ', '01', serialno), 
          CBSERIAL = gsub('HU', '00', CBSERIAL)
        ) %>%
        select(CBSERIAL, PERNUM = sporder, ogdi),
      all.x = TRUE, all.y = FALSE
    ) %>%
    mutate(
      ogdi = gsub('\\_\\D+$', '', ogdi),
      ogdi = case_match(
        ogdi,
        'AIAN' ~ 'N',
        'NHPI' ~ 'P',
        'MENA' ~ 'E',
        'HL'   ~ 'H',
        'UNK' ~  'O',
        .default = ogdi
      )
    ),
  present.combos %>% 
    mutate(ogdi = gsub('RET\\_', '', table.code)) %>%
    select(-c(allo, table.code)),
  all = FALSE
) %>%
  # Get rid of columns not in the raw PUMS
  select(-c(age.out, ogdi)) %>%
  # Complete by PUMA by all other identifying info EXCEPT PERWT
  # this will add PERWT = NA to any synthetic individual
  complete(
    PUMA, 
    nesting(
      YEAR, SERIAL, PERNUM, CBSERIAL, GQ, SEX, AGE, RACE, HISPAN,
      US2018A_POVPIP, US2019A_POVPIP, US2020A_POVPIP, 
      US2021A_POVPIP, US2022A_POVPIP
      )
  ) %>%
  # give me just the new individuals (with PERNWT = NA)
  filter(is.na(PERWT)) %>%
  mutate(
    # Assigning (bogus) negative serial numbers to differentiate fake records
    SERIAL = -(1:nrow(.)),
    # now set the PERWT to 1 for synthetic individuals
    PERWT = 1
  ) %>%
  # order the columns identically to the full ACS PUMS
  select(names(acs.m))

# Now, format X

x.m.all = rbind(acs.m, pums.synthetic) %>%
  # Pivot to get poverty out
  mutate(across(contains('POVPIP'), function(x) ifelse(x %in% '', NA, x))) %>%
  pivot_longer(
    cols = contains('POVPIP'), names_pattern = 'US(20\\d{2})A_POVPIP',
    names_to = 'POVYEAR', values_to = 'POVPIP'
  ) %>%
  filter(!is.na(POVPIP)) %>%
  select(-POVYEAR) %>%  
  # Re-code race to match table
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
    # Code poverty status
    POVPIP = ifelse(grepl('[A-Za-z]', POVPIP), NA, POVPIP),
    pov = as.numeric(POVPIP) / 100,
    # (as character because factor coding has male first)
    sex = as.character(as_factor(SEX)) %>% tolower(),
    # Bin ages
    age.pov = cut(AGE, breaks = c(age.p.breaks, Inf), right = FALSE),
    age.sex = cut(AGE, breaks = c(age.s.breaks, Inf), right = FALSE),
    pov     = cut(pov, breaks = c(pov.breaks, Inf), right = FALSE),
    # Re-code hispanic
    hsp = case_when(
      # hispanic
      HISPAN > 0 ~ 'hisp',
      # if not hispanic and white, 'nhwh' is non-hispanic white
      RACE %in% 1 ~ 'nhwh',
      # otherwise, not hispanic not white
      .default = 'nhnw'
    )
  )

x.m = x.m.all %>%
  # NOTE: code breaking here because defunct xidx is used (rather than serial/pernum)
  # ALSO: need to change the grand total to a 1 if isn't NA
  select(YEAR, SERIAL, PERNUM, PERWT, PUMA, pov, age.pov, age.sex, sex, rac, hsp) %>%
  # note: in future, tidyr's expand() or complete() or something works more elegantly
  rbind(
    expand.grid(
      YEAR = 0,
      SERIAL = 0,
      PERNUM = 0,
      PUMA = 0,
      PERWT = 0,
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
    total = as.numeric(!is.na(pov)),
    age.pov.age.ones = 1,
    age.pov.age.tota = age.pov
  ) %>%
  pivot_wider(names_from = age.pov.age.tota, values_from = age.pov.age.ones, values_fill = 0) %>%
  # Next pivot out age-by-poverty data
  mutate(age.pov.ones = 1) %>%
  pivot_wider(
    names_from = c(age.pov, pov), values_from = age.pov.ones, 
    names_sep = '_', values_fill = 0
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
    names_sep = '_', values_fill = 0
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
    names_sep = '_', values_fill = 0
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
    names_sep = '_', values_fill = 0
  ) %>%
  # Finally, pivot out hisp-age-sex info
  mutate(hisp.age.sex.ones = 1) %>%
  pivot_wider(
    names_from = c(hsp, sex, age.sex), values_from = hisp.age.sex.ones,
    names_sep = '_', values_fill = 0
  ) %>%
  filter(SERIAL != 0) %>%
  # filter out bad/unnecessary columns
  # no constraints for non-hispanic non-whites
  select(-contains('nhnw')) %>%
  # no constraints for people with un-measurable income
  select(-matches('\\_NA$')) %>%
  arrange(YEAR, SERIAL, PERNUM)

data.frame(
  in.x = names(x.m)[-(1:5)],
  in.y = y.m %>% 
    distinct(is.grand.total, age.pov, pov, rac, hsp, sex, age.sex) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), '', .)))
) %>% select(-in.y.is.grand.total) %>% sample_n(15)
# currently NAs...

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

####### == Make wrapper and run PMEDM
####### ==
####### ==

all.pmedm = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m, ~ PUMA),
  .f = function(x.data, y.cons) {
    
    (NN = sum(y.cons$E[y.cons$is.grand.total]))
    (nn = nrow(x.data))
    (JJ = length(unique(y.cons$TRACT)))
    (kk = nrow(y.cons) / JJ)
    
    yy = y.cons$E / NN
    vv = (y.cons$M * nn / (NN^2)) %>%
      .sparseDiagonal(n = length(.)) %>% 
      as('generalMatrix')
    
    xx = kronecker(t(as.matrix(x.data[, -(1:5)])), .sparseDiagonal(n = JJ)) %>%
      t() %>%
      as('dgCMatrix')
    
    dd = matrix(rep(x.data$PERWT, each = JJ), ncol = 1)
    dd = dd / sum(dd)
    
    pp = PMEDM_solve(xx, yy, vv, dd)
    
    pp$pred = pp$pred * NN
    pp$p = pp$p * NN
    
    return(pp)
    
  }
)


all.const = map2_df(
  .x = all.pmedm,
  .y = split(y.m, ~ PUMA),
  .f = function(p, y) mutate(y, pred = p$pred)
)

# Assess:
all.const %>%
  mutate(in.mar = pred < E + M & pred > E - M) %>%
  ggplot(aes(x = E, colour = in.mar)) +
  geom_point(aes(y = pred), size = 3) +
  geom_segment(aes(xend = E, y = E - M, yend = E + M), linewidth = 0.1) +
  scale_colour_manual(values = c('red', 'black')) +
  facet_wrap(~ PUMA)

# Okay some are not in the MOE

all.const %>% filter(pred > E + M | pred < E - M) %>% print(n = nrow(.))

# huh some of these look like they're concentrated in a single tract?
# lol one is the grand total...?
# e.g., 4002 is the University of Portland tract and there's an apparent overall
# over-allocation of 18,24 year olds?

### Allocations

all.alloc = map2(
  .x = split(x.m, ~ PUMA), 
  .y = all.pmedm, 
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

all.alloc = map2(
  .x = all.alloc,
  .y = split(y.m, ~ PUMA),
  .f = function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(TRACT) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$PUMA[1], tracts, sep = '_')
    
    # Return matrix
    return(p.matrix)
  }
)

all.x.allocation = merge(
  x.m.all %>%
    merge(
      read.csv('01_raw_data/5acs22_orwa_reldpri.csv') %>%
        mutate(CBSERIAL = gsub('GQ', '01', serialno), CBSERIAL = gsub('HU', '00', CBSERIAL)) %>%
        select(CBSERIAL, PERNUM = sporder, omb, ogdi),
      all.x = TRUE, all.y = FALSE
      ),
  all.alloc %>%
    lapply(
      function(m) m %>% 
        as.data.frame() %>%
        mutate(id = row.names(.)) %>%
        pivot_longer(-id, names_to = c('PUMA', 'TRACT'), names_pattern = '(\\d{4})\\_(\\d+)')
    ) %>%
    do.call(., what = rbind) %>%
    separate_wider_delim(id, names = c('YEAR', 'SERIAL', 'PERNUM'), delim = '_'),
  by = c('YEAR', 'SERIAL', 'PERNUM', 'PUMA')
)

# Merge with external race data

pov.x.allocation = all.x.allocation %>%
  # I think for the purpose of the table missing POVPIP values are not helpful
  filter(!is.na(POVPIP)) %>%
  mutate(
    age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE),
    pov.out = cut(as.numeric(POVPIP) / 100, breaks = c(0, 1, 2.5, 4, Inf), right = FALSE),
  )

pov.table1 = pov.x.allocation %>%
  group_by(PUMA, TRACT, age.out, pov.out) %>%
  summarise(n.est = sum(value)) %>%
  ungroup()

# a test:
pov.table1 %>% 
  # filter(!grepl('\\[4', pov.out)) %>% 
  pivot_wider(names_from = age.out, values_from = n.est, names_prefix = 'a') %>% 
  print(n = 20)

pov.table1 %>%
  # filter(!grepl('\\[4', pov.out)) %>% 
  pivot_wider(names_from = age.out, values_from = n.est, names_prefix = 'a') %>%
  filter(if_any(starts_with('a'), ~ is.na(.)))
# all age-pov groups are represented

# Need to modify poverty...
# Cumulative sums and get rid of 400%+

pov.table1 = pov.table1 %>%
  # Rename a column
  rename(allo = n.est, tract = TRACT) %>%
  # Remove PUMA column
  select(-PUMA) %>%
  # Get rid of highest poverty group
  filter(!grepl('\\[4', pov.out)) %>%
  mutate(pov.out = as.numeric(gsub('\\[.+\\,(.+)\\)$', '\\1', pov.out))) %>%
  group_by(tract, age.out) %>%
  mutate(
    allo = cumsum(allo),
    table.code = paste0('POV', 100 * pov.out)
  ) %>%
  ungroup() %>%
  select(-pov.out)

#-- EXPORT

write.csv(
  pov.table1, row.names = FALSE,
  '03_downscale_out/povall_raw.csv'
)


#### BIPOC poverty table

pov.table2 = pov.x.allocation %>%
  filter(!hsp %in% 'nhwh') %>%
  group_by(PUMA, TRACT, age.out, pov.out) %>%
  summarise(n.est = sum(value)) %>%
  ungroup()

pov.table2 %>% 
  # filter(!grepl('\\[4', pov.out)) %>% 
  pivot_wider(names_from = age.out, values_from = n.est, names_prefix = 'a') %>% 
  print(n = 20)

pov.table2 %>% 
  # filter(!grepl('\\[4', pov.out)) %>% 
  pivot_wider(names_from = age.out, values_from = n.est, names_prefix = 'a') %>%
  filter(if_any(starts_with('a'), ~ is.na(.))) %>%
  distinct(PUMA, .keep_all = TRUE)

# Nice - we have all of age-poverty combos here!
# Ready to export

# Rename a column
pov.table2 = pov.table2 %>%
  rename(allo = n.est, tract = TRACT) %>%
  # Remove PUMA column
  select(-PUMA) %>%
  # Get rid of highest poverty group
  filter(!grepl('\\[4', pov.out)) %>%
  mutate(pov.out = as.numeric(gsub('\\[.+\\,(.+)\\)$', '\\1', pov.out))) %>%
  group_by(tract, age.out) %>%
  mutate(
    allo = cumsum(allo),
    table.code = paste0('POV', 100 * pov.out, '_POC')
  ) %>%
  ungroup() %>%
  select(-pov.out)

#-- EXPORT
write.csv(
  pov.table2, row.names = FALSE,
  '03_downscale_out/povbipoc_raw.csv'
)


### BIPOC all table

bipoc.x.allocation = all.x.allocation %>%
  # I think for the purpose of the table missing POVPIP values are not helpful
  filter(!hsp %in% 'nhwh') %>%
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE))

bipoc.table1 = bipoc.x.allocation %>%
  group_by(PUMA, TRACT, age.out) %>%
  summarise(allo = sum(value)) %>%
  ungroup()

bipoc.table1 %>%
  pivot_wider(names_from = age.out, values_from = allo, names_prefix = 'a') %>%
  filter(if_any(starts_with('a'), ~ is.na(.))) %>%
  distinct(PUMA, .keep_all = TRUE)
# great - every age group represented

bipoc.table1 = bipoc.table1 %>%
  select(-PUMA) %>%
  rename(tract = TRACT) %>%
  mutate(table.code = 'POC')

#-- EXPORT
write.csv(
  bipoc.table1, row.names = FALSE,
  '03_downscale_out/bipocall_raw.csv'
)


### Race and ethnicity table

raceth.x.allocation = all.x.allocation %>%
  mutate(
    ogdi = gsub('\\_\\D+$', '', ogdi),
    age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)
  ) %>%
  group_by(ogdi, age.out, TRACT = as.numeric(TRACT), PUMA) %>%
  summarise(n = sum(value)) %>%
  ungroup() %>%
  mutate(
    ogdi = case_match(
      ogdi,
      'AIAN' ~ 'N',
      'NHPI' ~ 'P',
      'MENA' ~ 'E',
      'HL'   ~ 'H',
      'UNK' ~  'O',
      .default = ogdi
    ),
    table.code = paste0('RET_', ogdi)
  ) %>%
  select(tract = TRACT, age.out, allo = n, table.code, PUMA)

head(raceth.x.allocation)

raceth.x.allocation

raceth.x.allocation %>% 
  select(-tract) %>%
  distinct(PUMA, age.out, table.code, .keep_all = TRUE) %>%
  complete(PUMA, age.out, table.code) %>%
  filter(is.na(allo))

raceth.x.allocation %>% 
  select(-tract) %>%
  distinct(PUMA, age.out, table.code, .keep_all = TRUE) %>%
  complete(PUMA, age.out, table.code) %>%
  filter(is.na(allo)) %>%
  group_by(age.out, table.code) %>%
  summarise(n = n())
# coverage everywhere after adding the synthetic indivduals!

# raceth.x.allocation %>% 
#   select(-tract) %>%
#   distinct(PUMA, age.out, table.code, .keep_all = TRUE) %>%
#   complete(PUMA, age.out, table.code) %>%
#   filter(is.na(allo)) %>%
#   arrange(PUMA, table.code, age.out) %>%
#   write.csv('02_downscaling/multnomah_missing_raceth_age.csv', row.names = FALSE)

head(raceth.x.allocation)

reth.table1 = raceth.x.allocation %>% select(-PUMA)

write.csv(
  reth.table1, row.names = FALSE,
  '03_downscale_out/raceth_raw.csv'
)


### Sex table

sex.table1 = all.x.allocation %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract = TRACT, age.out, sex) %>%
  summarise(allo = sum(value))

sex.table1 = sex.table1 %>%
  mutate(table.code = paste0('SEX_', ifelse(sex %in% 'female', 'F', 'M'))) %>%
  select(-sex)

head(sex.table1)

write.csv(
  sex.table1, row.names = FALSE,
  '03_downscale_out/sex_raw.csv'
)
