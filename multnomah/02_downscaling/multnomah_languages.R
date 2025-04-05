##########################################################
# Script for getting estimates of people by language spoken, english
# proficiency, sex+age
##########################################################

library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

# setting working directory (repo was reorganized after script first run)
setwd('multnomah/')

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
acs.ddi = read_ipums_ddi('01_raw_data/usa_00023.xml')

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
    SEX, AGE, LANGUAGE, SPEAKENG
  )

# Extra individuals for 
acs.supp = read_ipums_ddi('01_raw_data/usa_00025.xml') %>%
  read_ipums_micro() %>%
  filter(COUNTYFIP %in% 51) %>%
  select(-c(STATEFIP, COUNTYFIP)) %>%
  mutate(PUMA = gsub('^13', '51', PUMA)) %>%
  # Select columns of interest
  select(
    YEAR, PUMA, SERIAL, PERNUM, PERWT,
    SEX, AGE, LANGUAGE, SPEAKENG
  )

# Read in and subset NHGIS data
tab1.raw = read_nhgis('01_raw_data/nhgis0017_csv.zip', file_select = 1) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))
tab2.raw = read_nhgis('01_raw_data/nhgis0017_csv.zip', file_select = 2) %>% 
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(TRACTA, matches('A\\w{3}[EM]\\d{3}'))

# Merge these together
tab.m = merge(tab1.raw, tab2.raw, by = 'TRACTA') %>%
  # Merge in PUMA data
  mutate(TRACTA = as.numeric(TRACTA)) %>%
  merge(tra.m %>% select(TRACTCE, PUMA5CE), by.x = 'TRACTA', by.y = 'TRACTCE') %>%
  select(TRACT = TRACTA, PUMA = PUMA5CE, everything())

# PUMA-age-language combinations missing from ACS above
missing.combos = read.csv('02_downscaling/multnomah_missing_lang_prof_age.csv')

# Crosswalk for language classification
# langx = read.csv('01_raw_data/lang_xwalk_lanp16.csv')

##############################################
# -------- Format data for routine ----------#
##############################################

# Format Y (constraint table)
# (looks like there are 310 columns in data currently, so 154 constraints)
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
  # Remove the duplicate `total` column for the language (universe 5 and older)
  # (estimate and margin are identical)
  filter(!(form %in% 'AQPY' & var_label %in% 'total')) %>%
  # Formatting:
  # First, label variable types
  mutate(
    # case_when might be better coding but I'll just do it like this for now
    grand.total = var_label %in% 'total' & form %in% 'AQM4',
    total.5p = var_label %in% 'total' & form %in% 'AQ5H',
    total.langd = !(var_label %in% 'total') & !grepl('\\:\\sspeak', var_label) & form %in% 'AQ5H',
    total.sex  = grepl('(fe)?male$', var_label),
    total.agel = grepl('years', var_label) & !grepl('\\:', var_label) & form %in% 'AQPY',
    total.agel.lang = grepl('years(\\s\\w+)*\\:[^:]+$', var_label) & form %in% 'AQPY',
    langd.prof = grepl('\\:', var_label) & form %in% 'AQ5H',
    sex.ages  = grepl('\\:', var_label) & form %in% 'AQM4',
    agel.lang.profd = grepl('\\:.*\\:', var_label)
  ) %>%
  # swap out 'under' for '0 to'
  mutate(var_label = gsub('[Uu]nder', '0 to', var_label)) %>%
  # Now get the levels out
  mutate(
    lang = case_when(
      total.agel.lang ~ gsub('.*\\:\\sspeak\\s', '', var_label),
      agel.lang.profd ~ gsub('.*\\:\\sspeak\\s([^\\:]+)\\:.*', '\\1', var_label),
      .default = NA
    ),    
    langd = case_when(
      total.langd ~ gsub('speak\\s', '', var_label),
      langd.prof  ~ gsub('\\:.*$', '', var_label),
      .default = NA
    ),
    # presence of 'other' is annoying here!
    across(c(lang, langd), ~ ifelse(grepl('^other\\s(and\\s)?', .), paste0(., ' other'), .)),
    across(c(lang, langd), ~ gsub('^other\\s(and\\sunspecified\\s)?', '', .)),
    agel = ifelse(total.agel | total.agel.lang | agel.lang.profd, gsub('^(\\d{1,2}).*', '\\1', var_label), NA),
    ages = ifelse(sex.ages, gsub('(fe)?male\\:\\s(\\d{1,2}).*', '\\2', var_label), NA),
    across(c(agel, ages), as.numeric),
    sex  = case_when(
      total.sex ~ var_label,
      sex.ages ~ gsub('\\:.*$', '', var_label),
      .default = NA
    ),
    prof  = ifelse(langd.prof, gsub('.*\\:\\sspeak\\senglish\\s', '', var_label), NA),
    prof  = case_match(
      prof,
      "'very well'" ~ 1,
      "less than 'very well'" ~ 2,
      .default = NA
    ),
    profd = ifelse(agel.lang.profd, gsub('^.*\\:\\sspeak\\senglish\\s', '', var_label), NA),
    profd = case_match(
      profd,
      "'very well'" ~ 1,
      "'well'" ~ 2,
      "'not well'" ~ 3,
      "'not at all'" ~ 4,
      .default = NA
    )
  ) %>%
  # # Lines for checking
  # distinct(var_label, var_desc, .keep_all = TRUE) %>%
  # select(-var_desc) %>%
  # select(var_label, lang, langd, agel, ages, sex, prof, profd)
  arrange(
    desc(grand.total), desc(total.5p), desc(total.langd), desc(total.sex), desc(total.agel),
    desc(total.agel.lang), desc(langd.prof), desc(sex.ages), desc(agel.lang.profd),
    lang, langd, agel, ages, sex, prof, profd, TRACT
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

# Get age bins

agel.bins = y.m %>%
  distinct(agel) %>%
  filter(!is.na(agel)) %>%
  arrange(agel) %>%
  pull()

ages.bins = y.m %>%
  distinct(ages) %>%
  filter(!is.na(ages)) %>%
  arrange(ages) %>%
  pull()

# not 'detailed' language
y.m %>% distinct(lang) %>% filter(!is.na(lang))
# 'detailed' language
# y.m %>% distinct(langd) %>% filter(!is.na(langd))

### Okay, next on to X

# language list
# ipums_var_info(acs.m, vars = LANGUAGE)$val_labels[[1]] %>% print(n = nrow(.))
# see: https://www.census.gov/topics/population/language-use/about.html

# Getting missing combinations

# Merge this list with numeric LANGUAGE and SPEAKENG codes used in PUMS
missing.combos = merge(
  missing.combos,
  data.frame(
    langd = c(
      'arabic', rep('asian.other', 13), 'chinese', 
      rep('indo-european.other', 17), 
      rep('lang.other', 45),
      rep('german.west.germanic', 3), 'korean', 'spanish', 
      rep('russian.polish.slavic', 9), 'french.haitian.cajun', 'tagalog', 'vietnamese'
    ),
    LANGUAGE = c(
      57, setdiff(40:56, c(43, 49, 50, 54)), 43,
      setdiff(c(2:11, 13:32), c(2:4, 11, 18:26)),
      c(33:39, 58:94, 96),
      2:4, 49, 12,
      18:26, 11, 54, 50
    )
  )
) %>%
  merge(
    data.frame(
      prof = c('prof1', rep('prof2', 3)),
      SPEAKENG = c(4, 1, 5:6)
    )
  ) %>%
  rename(age = age.out)

# Invert above data frame to get all of the PUMA-(language-age) combos that are
# present in PUMS
present.combos = missing.combos %>%
  mutate(missing = TRUE) %>%
  complete(PUMA, nesting(age, langd, prof, LANGUAGE, SPEAKENG), fill = list(missing = FALSE)) %>%
  filter(!missing) %>%
  select(-missing)

pums.synthetic = merge(
  rbind(acs.m, acs.supp) %>% mutate(age = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)),
  present.combos %>% select(-c(langd, prof)),
  all = FALSE
) %>%
  select(-age) %>%
  # Complete by PUMA by all other identifying info EXCEPT PERWT
  # this will add PERWT = NA to any synthetic individual
  complete(PUMA, nesting(YEAR, SERIAL, PERNUM, SEX, AGE, LANGUAGE, SPEAKENG)) %>%
  # give me just the new individuals (with PERNWT = NA)
  filter(is.na(PERWT)) %>%
  mutate(
    # Add 0.5 to the SERIAL of any synthetic individual 
    SERIAL = -(1:nrow(.)),
    # now set the PERWT to 1 for synthetic individuals
    PERWT = 1
  ) %>%
  # order the columns identically to the full ACS PUMS
  select(names(acs.m))


x.m.all = rbind(acs.m, acs.supp, pums.synthetic) %>%
  mutate(
    sex = ifelse(SEX > 1, 'female', 'male'),
    # agel (age for language interaction)
    agel = cut(AGE, c(agel.bins, Inf), right = FALSE),
    # ages (age for sex interaction)
    ages = cut(AGE, c(ages.bins, Inf), right = FALSE),
    # language (not detailed)
    lang = case_match(
      LANGUAGE,
      1 ~ 'only.english',
      12 ~ 'spanish',
      c(2:11, 13:32) ~ 'indo-european.other',
      40:56 ~ 'asian.other',
      c(33:39, 57:94, 96) ~ 'lang.other',
      .default = NA
    ),
    langd = case_match(
      # see lang12 in langx
      LANGUAGE,
      1 ~ 'only.english',
      12 ~ 'spanish',
      57 ~ 'arabic',
      43 ~ 'chinese',
      11 ~ 'french.haitian.cajun',
      2:4 ~ 'german.west.germanic',
      49 ~ 'korean',
      18:26 ~ 'russian.polish.slavic',
      54 ~ 'tagalog',
      50 ~ 'vietnamese',
      .default = lang
    ),
    prof = case_when(
      LANGUAGE %in% 1 ~ NA,
      SPEAKENG %in% 4 ~ 1,
      SPEAKENG %in% c(1, 5:6) ~ 2,
      .default = NA
    ),
    prof = paste0('prof', prof),
    profd = case_when(
      LANGUAGE %in% 1 ~ NA,
      SPEAKENG %in% 4 ~ 1,
      SPEAKENG %in% 5 ~ 2,
      SPEAKENG %in% 6 ~ 3,
      SPEAKENG %in% 1 ~ 4,
      .default = NA
    ),
    profd = paste0('profd', profd)
  ) %>%
  mutate(across(c(agel, ages), ~ as.numeric(gsub('\\[(\\d{1,2}).+', '\\1', .))))

# desc(grand.total), desc(total.5p), desc(total.langd), desc(total.sex), desc(total.agel),
# desc(total.agel.lang), desc(langd.prof), desc(sex.ages), desc(agel.lang.profd),
# lang, langd, agel, ages, sex, prof, profd, TRACT

# X
x.m = x.m.all %>%
  complete(sex, nesting(ages, agel), nesting(langd, prof), nesting(lang, profd)) %>%
  arrange(as.character(langd), as.character(lang), ages, agel, sex, prof, profd) %>%
  mutate(
    total = 1,
    total.5p = as.numeric(AGE > 4),
    langd.ones = as.numeric(AGE > 4),
    langd.cols = langd,
    sex.ones = 1,
    sex.cols = sex,
    agel.ones = as.numeric(AGE > 4),
    agel.cols = agel,
    agel.lang.ones = as.numeric(AGE > 4),
    agel.lang.agel = agel,
    agel.lang.lang = lang,
    langd.prof.ones = as.numeric(AGE > 4),
    langd.prof.langd = langd,
    langd.prof.prof  = prof,
    sex.ages.ones = 1,
    sex.ages.sex  = sex,
    sex.ages.ages = ages,
    agel.lang.profd.ones  = as.numeric(AGE > 4),
    agel.lang.profd.agel  = agel,
    agel.lang.profd.lang  = lang,
    agel.lang.profd.profd = profd
  ) %>%
  pivot_wider(names_from = langd.cols, values_from = langd.ones, values_fill = 0) %>%
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  pivot_wider(
    names_from = agel.cols, names_prefix = 'a',
    values_from = agel.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(agel.lang.lang, agel.lang.agel), names_sep = '_',
    values_from = agel.lang.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(langd.prof.langd, langd.prof.prof), names_sep = '_',
    values_from = langd.prof.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(sex.ages.sex, sex.ages.ages), names_sep = '_',
    values_from = sex.ages.ones, values_fill = 0
  ) %>%
  pivot_wider(
    names_from = c(agel.lang.profd.agel, agel.lang.profd.lang, agel.lang.profd.profd),
    names_prefix = 'a', names_sep = '_',
    values_from = agel.lang.profd.ones, values_fill = 0
  ) %>%
  select(-c(SEX, AGE, LANGUAGE, SPEAKENG)) %>%
  # Filter out empty records made by the complete() above
  filter(!is.na(SERIAL)) %>%
  # Get rid of extra cases
  # (there were created for agel and language vars where individuals were below
  # age 5 and out of universe) 
  select(-contains('NA', ignore.case = FALSE)) %>%
  arrange(YEAR, SERIAL, PERNUM) %>%
  # Down-weight the out-of-sample individuals
  mutate(PERWT = ifelse(YEAR < 2018, PERWT / (2019-YEAR), PERWT))

dim(x.m)
names(x.m)[-(1:12)] %>% length()
y.m %>% filter(TRACT %in% 102) %>% nrow()

data.frame(
  inx = names(x.m)[-(1:12)],
  iny = y.m %>% filter(TRACT %in% 102) %>% select(agel, lang, profd, ages, sex, langd, prof, vartype)
) %>%
  filter(grepl('langd', iny.vartype))
  # slice(1:50)
  # sample_n(15)

# We are properly aligned!

#############################
# -------- Fitting ---------#
#############################

### Fit downscaling routine
lang.fits = map2(
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
    X = kronecker(t(as.matrix(x[, -(1:12)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, lambda = NULL))
    
  }
)


### Eval vits:
lang.cons = map2_df(
  .x = lang.fits,
  .y = split(y.m, ~ PUMA),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'grand.total']))
) 

lang.cons %>% print(n = 30)

lang.cons %>% group_by(PUMA, vartype) %>% summarise(p = mean(pred<E+M & pred>E-M)) %>% filter(p < 1)
# nice - entirely inside the MOE

lang.cons %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(y = E-M, yend = E+M, xend = E), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), size = 3, shape = 21) +
  # scale_colour_manual(values = c('red', 'black')) +
  facet_wrap(~ PUMA)  

# Looking pretty good

### Extract the allocations

lang.allos = map2(
  .x = split(x.m, ~ PUMA),
  .y = lang.fits,
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

lang.allos = map2(
  .x = lang.allos,
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

lang.allos = lang.allos %>%
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

lang.allos

# Merge with PUMS data
x.lang.allos = merge(
  x.m.all, lang.allos,
  by.x = c('YEAR', 'SERIAL', 'PERNUM'),
  by.y = c('year', 'serial', 'pernum')
)

head(x.lang.allos)

### Language by age allocation

lang.age.allos = x.lang.allos %>%
  mutate(
    age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE),
    langd = ifelse(grepl('other', langd), 'other', langd)
  ) %>%
  # group_by(PUMA, tract, age.out, langd) %>%
  group_by(PUMA, tract, age.out, langd) %>%
  summarise(n = sum(alloc)) %>%
  filter(!is.na(langd)) %>%
  ungroup()

# final.allos %>%
#   pivot_wider(names_from = age.out, values_from = n)
# 
# final.allos %>%
#   ungroup() %>%
#   complete(nesting(PUMA, tract), langd, age.out) %>%
#   filter(is.na(n)) %>%
#   distinct(PUMA, langd, age.out) %>%
#   print(n = nrow(.))

# Got everything.

# final.allos %>% 
#   pivot_wider(names_from = age.out, values_from = n) %>% 
#   filter(if_any(matches('\\['), ~ is.na(.))) %>% 
#   distinct(PUMA, langd, .keep_all = TRUE)

lang.table1 = lang.age.allos %>%
  merge(
    data.frame(
      langd = c(
        'arabic', 'chinese', 'only.english', 'french.haitian.cajun', 'german.west.germanic',
        'korean', 'other', 'russian.polish.slavic', 'spanish', 'tagalog', 'vietnamese'
      ),
      letter = c('ARB', 'CHN', 'ENG', 'FRN', 'GRM', 'KRN', 'OTH', 'RUS', 'SPN', 'TGL', 'VTN')
    )
  ) %>%
  mutate(table.code = paste0('LNG_', letter))

lang.table1 = lang.table1 %>%
  select(tract, age.out, allo = n, table.code)

# --- EXPORT

write.csv(
  lang.table1, row.names = FALSE,
  '03_downscale_out/languages_raw.csv'
)


### Non-English table

neng.table1 = x.lang.allos %>%
  filter(!langd %in% 'only.english', !is.na(langd)) %>%
  mutate(age.out = cut(AGE, breaks = c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(PUMA, tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

head(neng.table1)

neng.table1 = neng.table1 %>%
  select(-PUMA) %>%
  mutate(table.code = 'NEN')

# --- EXPORT

write.csv(
  neng.table1, row.names = FALSE,
  '03_downscale_out/non-english_raw.csv'
)


### Limited-English proficiency export

lim.table1 = x.lang.allos %>%
  filter(SPEAKENG %in% c(1, 5, 6)) %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

lim.table1 %>% complete(tract, age.out) %>% filter(is.na(allo))
# Gang's all here

lim.table1 = lim.table1 %>% mutate(table.code = 'LEP')

head(lim.table1)

# --- EXPORT

write.csv(
  lim.table1, row.names = FALSE,
  '03_downscale_out/lim-english_raw.csv'
)


### NEW IN MARCH 2025
### Language-limited english combinations
# ALSO these estimates were generated setting "speak no english" to prof <- 'prof2'

lang.leng.table = x.lang.allos %>%
  filter(!(langd %in% 'only.english'), SPEAKENG %in% c(1, 4:6)) %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, langd, prof, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

lang.leng.table %>%
  complete(tract, langd, prof, age.out) %>%
  group_by(tract, langd, age.out) %>%
  filter(any(is.na(allo))) %>%
  pivot_wider(names_from = prof, values_from = allo)
# uh, well, still a ton here
# looking at the germans though... they all speak english well!
# oh well...
# going to call it zero
  
lang.leng.table = lang.leng.table %>%
  complete(tract, langd, prof, age.out) %>%
  mutate(allo = ifelse(is.na(allo), 0, allo))

# Ah - we also want totals
lang.table = x.lang.allos %>%
  filter(!is.na(langd)) %>%
  mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
  group_by(tract, langd, age.out) %>%
  summarise(allo = sum(alloc)) %>%
  ungroup()

head(lang.table)

lang.table %>%
  filter(tract %in% 101) %>%
  complete(tract, langd, age.out) %>%
  filter(is.na(allo))

# all language groups included

# Okay - next step is export clean up and re-export

rbind(
  lang.leng.table %>% rename(group = prof),
  lang.table %>% mutate(group = 'total') %>% select(tract, langd, group, age.out, allo)
) %>%
  write.csv(
    row.names = FALSE,
    '03_downscale_out/lang_and_prof_raw_spring2025.csv'
  )


# Making a supp table for generating synthetic pums

# x.lang.allos %>%
#   filter(!is.na(langd), SERIAL > 0, AGE > 4, !(langd %in% 'only.english')) %>%
#   mutate(age.out = cut(AGE, c(0, 18, 55, 60, 85, Inf), right = FALSE)) %>%
#   group_by(PUMA, langd, prof, age.out) %>%
#   summarise(allo = sum(alloc)) %>%
#   ungroup() %>%
#   complete(PUMA, langd, prof, age.out) %>%
#   filter(is.na(allo)) %>%
#   select(-allo) %>%
#   write.csv('02_downscaling/multnomah_missing_lang_prof_age.csv', row.names = FALSE)
