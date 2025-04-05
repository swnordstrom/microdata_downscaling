# Script for doing *single PUMA* downscaling of PUMS data (Multnomah County) to
# obtain veteran-race-age counts

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
acs.ddi = read_ipums_ddi('01_raw_data/usa_ACS_vet_race.xml')

# Read in sample and 
acs.m = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>% 
  # Subset to just Oregon
  filter(PUMA %in% unique(tra.m$PUMA5CE)) %>%
  # Remove unnecessary columns
  select(PUMA, SERIAL, PERNUM, PERWT, AGE, SEX, RACE, HISPAN, VETSTAT)

nrow(acs.m)

# Read in NHGIS tabular data
vet.raw = read_nhgis('01_raw_data/nhgis_vetstatus.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')
rhp.raw = read_nhgis('01_raw_data/nhgis_race_hisp.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  # Remove Pov/Income ratio vars
  select(-starts_with('AQ68'))
pop.raw = read_nhgis('01_raw_data/2018-2022_ACS_tract_population_sizes.zip') %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051')

# Merge together NHGIS tabular data (merge population sizes with vet status)
tab.m = merge(
  vet.raw %>% select(TRACTA, starts_with('AQ')),
  rhp.raw %>% select(TRACTA, starts_with('AQ'))
) %>%
  merge(pop.raw %>% select(TRACTA, starts_with('AQ'))) %>%
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

head(tab.m)
names(tab.m)

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Now: begin data processing

y.m = tab.m %>%
  # Pivot variable columns to rows and merge in variable information
  pivot_longer(-c(TRACT, PUMA), names_to = 'var_name', values_to = 'value') %>%
  # Merging in variable info
  merge(
    rbind(
      ipums_var_info(vet.raw, starts_with('AQ')) %>% select(-val_labels),
      ipums_var_info(rhp.raw, starts_with('AQ')) %>% select(-val_labels),
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
    universe = gsub('(\\salone)*\\)', '', universe),
    universe = gsub('\\spopulation', '', universe)
  ) %>%
  # Unnecessary columns
  select(-c(form, code)) %>%
  # Designate variable categories
  mutate(
    var.cat = case_when(
      grepl('total', universe) ~ 'total',
      grepl('civilian', universe) ~ 'vet',
      grepl('hispan', universe) ~ 'hisp',
      .default = 'race'
    )
  ) %>%
  ### Establish logical/sorting columns
  # first, for ease, swap 'under' for '0 to' to make gsubing easier
  mutate(var_label = gsub('under', '0 to', var_label)) %>%
  mutate(
    is.grand.total = grepl('total', var_label) & var.cat %in% 'total',
    total.18p = grepl('total', var_label) & var.cat %in% 'vet',
    total.vet = grepl('^(non)*vet', var_label),
    total.rac = grepl('total', var_label) & var.cat %in% 'race', 
    total.hsp = grepl('total', var_label) & var.cat %in% 'hisp',
    total.sex.vet = grepl('(fe)?male$', var_label) & var.cat %in% 'vet',
    sex.rac = grepl('(fe)?male$', var_label) & var.cat %in% 'race',
    sex.hsp = grepl('(fe)?male$', var_label) & var.cat %in% 'hisp',
    sex.vet = grepl('^(fe)?male\\:\\s(non)?vet', var_label) & var.cat %in% 'vet',
    sex.agev = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label) & var.cat %in% 'vet',
    sex.ager = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label) & var.cat %in% 'race', 
    sex.ageh = grepl('^(fe)?male\\:\\s\\d[^\\:]+$', var_label) & var.cat %in% 'hisp',
    sex.age.vet = grepl('\\:.*\\:', var_label)
  ) %>%
  # checking to see if these logicals got everything
  # filter(!(is.grand.total | total.18p | total.vet | total.rac | total.sex.vet | total.sex.rac | sex.vet | sex.agev | sex.ager | sex.age.vet))
  ## # Get labels/groupings for sex, age, vet status
  mutate(
    # Sex
    sex = case_when(
      total.sex.vet | sex.rac | sex.hsp ~ var_label,
      sex.vet | sex.age.vet | sex.agev | sex.ager | sex.ageh ~ gsub('\\:.*', '', var_label),
      .default = NA
    ),
    # Age (for veteran status category)
    agev = case_when(
      sex.agev ~ gsub('.+\\:\\s', '', var_label),
      sex.age.vet ~ gsub('.+\\:\\s(.+)\\:.+', '\\1', var_label),
      .default = NA
    ),
    # Age (for race categories)
    ager = ifelse(sex.ager, gsub('.+\\:\\s', '', var_label), NA),
    ageh = ifelse(sex.ageh, gsub('.+\\:\\s', '', var_label), NA),
    # Veteran status
    vet = case_when(
      total.vet ~ var_label,
      sex.vet ~ gsub('(fe)*male\\:\\s', '', var_label),
      sex.age.vet ~ gsub('.+\\:.+\\:\\s', '', var_label),
      .default = NA
    ),
    # Race column
    rac = ifelse(var.cat %in% 'race', universe, NA),
    # Hispanic column
    hsp = case_when(
      var.cat %in% 'hisp' & !grepl('not', universe) ~ 'hispanic',
      var.cat %in% 'hisp' & grepl('not', universe) ~ 'nonhispanic white',
      .default = NA
    )
  ) %>% # distinct(var_label, sex, agev, vet, ager, rac, hsp) %>% print(n = nrow(.))
  # Format age column
  mutate(
    # below is maybe easier with \\w
    # NEXT: figure out how to get \\w to cover 'to' and 'and' (lmao)
    agev = gsub('years\\sand\\sover', 'to Inf', agev),
    agev = gsub('\\syears$', '', agev),
    agev = gsub('\\s\\w+\\s', ',', agev),
    ager = gsub('years\\sand\\sover', 'to Inf', ager),
    ager = gsub('\\syears$', '', ager),
    ager = gsub('\\s\\w+\\s', ',', ager),
    ageh = gsub('years\\sand\\sover', 'to Inf', ageh),
    ageh = gsub('\\syears$', '', ageh),
    ageh = gsub('\\s\\w+\\s', ',', ageh),
  ) %>% # distinct(age)
  # Re-code race columns
  mutate(rac = ifelse(rac %in% 'asian' | grepl('islander', rac), 'aanhpi', rac)) %>%
  group_by(
    TRACT, PUMA, var.cat,
    is.grand.total, total.18p, total.vet, total.rac, total.hsp,
    total.sex.vet, sex.vet, sex.agev, sex.age.vet, 
    sex.rac, sex.hsp, sex.ager, sex.ageh,
    vet, sex, agev, ager, ageh, rac, hsp
  ) %>%
  summarise(E = sum(E), M = sqrt(sum(M^2))) %>%
  ungroup()

# Okay. Now need to start pivoting...
y.m = y.m %>%
  # Need to re-format age column so that it sorts correctly
  mutate(across(c(ager, agev, ageh), \(x) as.numeric(gsub('(\\d{1,2})\\,.+', '\\1', x)), .names = "{.col}.sort")) %>%
  # select(-c(form, code, TRACT, PUMA)) %>%
  # distinct(var_label, .keep_all = TRUE) %>%
  arrange(
    desc(is.grand.total), desc(total.18p), desc(total.vet), desc(total.sex.vet), 
    desc(total.rac), desc(total.hsp), desc(sex.vet), desc(sex.agev), desc(sex.age.vet), 
    desc(sex.rac), desc(sex.hsp), desc(sex.ager), desc(sex.ageh),
    desc(vet), sex, agev.sort, ager.sort, ageh.sort, rac, hsp,
    TRACT
  ) %>% # %>% distinct(var_label) %>% print(n = nrow(.))
  select(-contains('sort'))

# Get age breaks
agev.breaks = y.m %>%
  filter(!is.na(agev)) %>%
  distinct(agev) %>%
  pull(agev) %>%
  gsub('(\\d{1,2})\\,\\w+', '\\1', .) %>%
  as.numeric()

ager.breaks = y.m %>%
  filter(!is.na(ager)) %>%
  distinct(ager) %>%
  pull(ager) %>%
  gsub('(\\d{1,2})\\,\\w+', '\\1', .) %>%
  as.numeric() %>%
  sort()

y.m.vargroup = y.m %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>%
  select(-tf)

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Do the X (ACS/PUMS data)

# Some notes:
# - No missing data
# acs.m %>% filter(if_any(c(AGE, SEX, VETSTAT, RACE, HISPAN), is.na))

x.m.all = acs.m %>%
  # Need to re-code VETSTAT to zero for age 17 individuals
  mutate(VETSTAT = ifelse(AGE %in% 17, 0, VETSTAT)) %>%
  arrange(SERIAL, PERNUM) %>%
  # Bin ages
  mutate(
    # Ages for vet status
    agev = cut(AGE, breaks = c(0, agev.breaks, Inf), right = FALSE),
    # re-format for readability
    agev = gsub('[\\[|\\)]', '', agev),
    # Ages associated with race/hisp and re-format
    ager = cut(AGE, breaks = c(ager.breaks, Inf), right = FALSE),
    ager = gsub('[\\[|\\)]', '', ager),
    vet = case_match(
      VETSTAT,
      0 ~ 'tooyoung',
      1 ~ 'nonveteran',
      2 ~ 'veteran'
    ),
    sex = ifelse(SEX %in% 1, 'male', 'female'),
    # Need to re-code race
    rac = case_match(
      RACE,
      1 ~ 'white',
      2 ~ 'black or african american',
      3 ~ 'american indian and alaska native',
      4:6 ~ 'aanhpi',
      7 ~ 'some other race',
      8:9 ~ 'two or more races'
    ),
    hsp = case_when(
      HISPAN > 0 ~ 'hisp',
      rac %in% 'white' ~ 'nhwh',
      .default = 'nhnw'
    )
  ) 

# desc(is.grand.total), desc(total.18p), desc(total.vet), desc(total.sex.vet), 
# desc(total.rac), desc(sex.vet), desc(sex.agev), desc(sex.age.vet), 
# desc(sex.rac), desc(sex.ager), desc(vet), sex, agev, ager, rac, hsp,

x.m = x.m.all %>%
  select(PUMA, SERIAL, PERNUM, PERWT, agev, vet, sex, ager, rac, hsp) %>%
  # arrange(sex, age, desc(vet)) %>%
  # complete(nesting(vet, agev), nesting(rac, hsp, ager, sex)) %>%
  complete(vet, sex, nesting(rac, hsp), nesting(agev, ager)) %>%
  # Need to arrange differently... ages are not right
  mutate(across(c(ager, agev), \(x) as.numeric(gsub('(\\d{1,2}).+', '\\1', x)), .names = "{.col}.sort")) %>%
  arrange(desc(vet), sex, agev.sort, ager.sort, rac, hsp) %>%
  # Start filling in ones
  mutate(
    # grand total
    total = 1,
    # NOTE: the veteran data is all for universe 18+
    # so they should ONLY COUNT TOWARDS THE GRAND TOTAL
    # and for everything else they count as zero
    # for 18+ total
    total.18p = as.numeric(!(agev %in% '0,18')),
    # for vet status
    total.vet.ones = as.numeric(!(agev %in% '0,18')),
    total.vet.cols = vet,
    # for sex
    total.sex.vet.ones = as.numeric(!(agev %in% '0,18')),
    total.sex.vet.cols = sex,
    # For race totals (NOTE: here all individuals are counted)
    total.rac.ones = 1,
    total.rac.cols = rac,
    total.hsp.ones = 1,
    total.hsp.cols = hsp# ,
    # total.sex.rac.ones = 1,
    # total.sex.rac.cols = sex,
  ) %>%
  # Pivot vet and sex(v) columns
  pivot_wider(names_from = total.vet.cols, values_from = total.vet.ones, values_fill = 0) %>%
  pivot_wider(names_from = total.sex.vet.cols, values_from = total.sex.vet.ones, values_fill = 0) %>%
  # Pivot race and hisp cols
  pivot_wider(names_from = total.rac.cols, values_from = total.rac.ones, values_fill = 0) %>%
  pivot_wider(names_from = total.hsp.cols, values_from = total.hsp.ones, values_fill = 0) %>%
  # Get combinations
  mutate(
    # sex-by-vet
    sex.vet.ones = as.numeric(!(agev %in% '0,18')),
    sex.vet.sex.cols = sex,
    sex.vet.vet.cols = vet,
    # sex-by-age
    sex.age.ones = as.numeric(!(agev %in% '0,18')),
    sex.age.sex.cols = sex,
    sex.age.age.cols = agev,
    # sex-age-vet status
    sex.age.vet.ones = as.numeric(!(agev %in% '0,18')),
    sex.age.vet.sex.cols = sex,
    sex.age.vet.age.cols = agev,
    sex.age.vet.vet.cols = vet
  ) %>%
  pivot_wider(
    names_from = c(sex.vet.vet.cols, sex.vet.sex.cols), names_sep = '_', 
    values_from = sex.vet.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.sex.cols, sex.age.age.cols), 
    names_sep = '_', values_from = sex.age.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.age.vet.vet.cols, sex.age.vet.sex.cols, sex.age.vet.age.cols),
    names_sep = '_', values_from = sex.age.vet.ones, values_fill = 0
  ) # %>%
  # NEXT UP
  # RACE/SEX/AGE COLUMNS

x.m = x.m %>%
  mutate(
    # Sex-race and sex-hispanic columns
    sex.rac.ones = 1,
    sex.rac.sex.cols = sex,
    sex.rac.rac.cols = rac,
    sex.hsp.ones = 1,
    sex.hsp.sex.cols = sex,
    sex.hsp.hsp.cols = hsp,
    # Sex-race-age and sex-hispanic-age columns
    sex.rac.age.ones = 1,
    sex.rac.age.sex.cols = sex,
    sex.rac.age.age.cols = ager,
    sex.rac.age.rac.cols = rac,
    sex.hsp.age.ones = 1,
    sex.hsp.age.sex.cols = sex,
    sex.hsp.age.age.cols = ager,
    sex.hsp.age.hsp.cols = hsp
  ) %>%
  pivot_wider(
    names_from = c(sex.rac.sex.cols, sex.rac.rac.cols),
    names_sep = '_', values_from = sex.rac.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.hsp.sex.cols, sex.hsp.hsp.cols),
    names_sep = '_', values_from = sex.hsp.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.rac.age.sex.cols, sex.rac.age.age.cols, sex.rac.age.rac.cols),
    names_sep = '_', values_from = sex.rac.age.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.hsp.age.sex.cols, sex.hsp.age.age.cols, sex.hsp.age.hsp.cols),
    names_sep = '_', values_from = sex.hsp.age.ones, values_fill = 0
  )

x.m = x.m %>%
  filter(!is.na(SERIAL) | !is.na(PERNUM)) %>%
  arrange(PUMA, SERIAL, PERNUM) %>%
  select(-contains('tooyoung')) %>%
  select(-contains('0,18')) %>%
  select(-contains('nhnw')) %>%
  select(-contains('sort'))

data.frame(
  in.x = names(x.m)[-(1:10)],
  in.y = y.m.vargroup %>% filter(TRACT %in% 2001) %>% select(vartype, vet, sex, agev, ager, ageh, rac, hsp)
) %>%
  sample_n(15)
# looks like everything is good

as.matrix(x.m[,-(1:10)]) %>% apply(1, sum)

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Do the PMEDM

# Wrapper function
pmedm.prep.fit = function(x, y) {
  
  # # # define variables
  # total PUMA population size
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
  # NOTE: CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED
  X = kronecker(t(as.matrix(x[, -(1:10)])), .sparseDiagonal(n = J)) %>%
    t() %>%
    as('dgCMatrix')
  # prior weights
  q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
  
  return(PMEDM_solve(X, Y, v, q, lambda = NULL))
  
}

# use purrr's `map2` function
all.p = map2(
  .x = split(x.m, ~ PUMA),
  .y = split(y.m.vargroup, ~ PUMA),
  pmedm.prep.fit
)

# ---------------------------------
# ---------------------------------
# ---------------------------------
# Evaluate

all.constr = map2_df(
  .x = all.p,
  .y = split(y.m.vargroup, ~ PUMA),
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
  mutate(pred = pred * sum(E[vartype %in% 'is.grand.total'])) %>%
  ungroup()

all.constr %>%
  filter(PUMA %in% '5105') %>%
  mutate(
    varb.char = case_match(
      vartype,
      'is.grand.total' ~ 'grand total',
      'total.18p' ~ 'total population 18+',
      'total.rac' ~ 'total race',
      'total.hsp' ~ 'total ethnicity',
      'total.vet' ~ 'total veteran status',
      'total.sex.vet' ~ 'total sex',
      'sex.ager' ~ 'sex-age bin (race binning) combination',
      'sex.ageh' ~ 'sex-age bin (ethnicity binning) combination',
      'sex.agev' ~ 'sex-age bin (veteran binning) combination',
      'sex.rac' ~ 'sex-race combination',
      'sex.hsp' ~ 'sex-ethnicity combination',
      'sex.vet' ~ 'sex-veteran combination',
      'sex.age.vet' ~ 'sex-veteran-age bin (veteran binning) combination'
    ),
    varb.char = factor(
      varb.char,
      levels = c(
        'grand total',
        'total population 18+',
        'total race',
        'total ethnicity',
        'total veteran status',
        'total sex',
        'sex-age bin (race binning) combination',
        'sex-age bin (ethnicity binning) combination',
        'sex-age bin (veteran binning) combination',
        'sex-race combination',
        'sex-ethnicity combination',
        'sex-veteran combination',
        'sex-veteran-age bin (veteran binning) combination'
      )
    )
  ) %>%
  mutate(in.mar = pred < E + M & pred > E - M) %>%
  ggplot(aes(x = E)) +
  annotate('segment', x = 0, xend = 6000, y = 0, yend = 6000, linetype = 2) +
  geom_segment(
    aes(xend = E, y = E - M, yend = E + M, colour = in.mar),
    linewidth = 0.1
  ) +
  geom_point(
    aes(y = pred, fill = varb.char, colour = in.mar), 
    size = 3, pch = 21
  ) +
  scale_fill_discrete('variable type') +
  scale_colour_manual(values = c('red', 'black')) +
  guides(colour = 'none') +
  labs(x = 'constraint in table', y = 'constraint estimate in model') +
  theme(
    panel.background = element_blank(),
    legend.position = 'right',
    legend.title.position = 'top',
    legend.justification = 'center',
  )

ggsave(
  '02_downscaling/draft_figs/2024-12-mod2-constraints.png',
  width = 8, height = 5
)

# nice
