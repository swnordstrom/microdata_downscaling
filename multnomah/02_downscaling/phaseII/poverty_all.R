library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

# ==============================================================
# Read in raw and curated data

# Get PUMS ddi
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00053.xml')

# Read in PUMS and subset to MultCo
pums.raw = pums.raw.ddi %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset geography
  filter(PUMA %in% c(5100 + c(1:3, 5, 14, 16), 1300 + c(1:3, 5, 14, 16)))

# Read in tabular data
acs.tab.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0034_csv.zip', file_select = 1) %>%
  filter(grepl('Multno', COUNTY))
acs.tab.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0034_csv.zip', file_select = 2) %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')

# Synthetic records
# Get synthetic records (PUMS and dummy)
synthetic.records = read.csv('multnomah/02_downscaling/phaseII/reld_ancestry_poverty_synthetic_records.csv')


# ==============================================================
# Combine and massage data (before formatting for data)

### Handle tabular constraints

# Format ACS tables and merge in to get PUMAs for each tract
acs.tab = merge(
  acs.tab.raw1 %>% select(TRACTA, matches('[EM]\\d{3}$')),
  acs.tab.raw2 %>% select(TRACTA, matches('[EM]\\d{3}$'))
) %>%
  # Rename tract column for merge
  mutate(tract = as.numeric(TRACTA)) %>%
  select(-TRACTA) %>%
  # Merge to get PUMAs
  merge(puma.tract %>% filter(puma > 5000) %>% select(tract, puma)) %>%
  # Do some label name/constraint massaging 
  pivot_longer(-c(puma, tract), names_to = 'var_name', values_to = 'var_value') %>%
  merge(
    rbind(
      ipums_var_info(acs.tab.raw1, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc),
      ipums_var_info(acs.tab.raw2, matches('[EM]\\d{3}')) %>% select(var_name, var_label, var_desc)
    )
  ) %>%
  # Get rid of 'Margins of error' or 'Estimates' in val label
  mutate(var_label = sub('^[^\\:]+\\:\\s', '', tolower(var_label))) %>%
  # Get var code for pivoting
  separate_wider_position(var_name, widths = c(form = 4, em = 1, 3)) %>%
  pivot_wider(names_from = em, values_from = var_value) %>%
  # Remove table name to separate out var description and universe
  mutate(
    var_desc = gsub('^table\\s[A-Za-z0-9]{4}\\:\\s', '', tolower(var_desc)),
    var_desc = gsub('\\)$', '', var_desc)
  ) %>%
  separate_wider_delim(var_desc, delim = ' (universe: ', names = c('var_desc', 'univ')) %>%
  # Change age labels ('Under' to '00 to', and add preceding zero where needed)
  mutate(
    # Handle poverty bins (which have decimals)
    var_label = gsub('under\\s\\.', '0.00 to 0.', var_label),
    var_label = gsub('^\\.(\\d{2})', '0.\\1', var_label),
    var_label = gsub('\\s\\.(\\d{2})', ' 0.\\1', var_label),
    # Handle age bins (no decimal)
    var_label = gsub('under\\s(\\d)', '00 to \\1', var_label),
    var_label = gsub('^(\\d{1})\\s', '0\\1 ', var_label),
    var_label = gsub('\\:\\s(\\d{1})\\s', ': 0\\1 ', var_label)
  )

# Format the ancestry table
ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) %>%
  # CRUCIAL: want to arrange the rows to conform with hte other table
  arrange(var_label, tract)


### Working with PUMS

# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, 
      STRATA, HISPAND, ANCESTR1D, ANCESTR2D, OCC, GQ
    )
  ) %>%
  mutate(PUMA = PUMA + ifelse(PUMA < 5000, (5100-1300), 0))

nrow(pums)
table(pums$PUMA)


# ==============================================================
# Now start formatting data

### Format the constraint table (y)

y.tab = acs.tab %>%
  # Filter out duplicate cases
  filter(
    # Total in Hispanic/Latino (ASOB) table is a duplicate
    !(var_label %in% 'total' & form %in% 'ASOB')
  ) %>%
  # Classify 
  mutate(
    # Get totals (tract-level total is in ASNQ)
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    # Whole poverty universe
    is.total.pov.univ = form %in% 'AS75' & var_label %in% 'total',
    # Total sex
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    # Sex-age (whole population)
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    # Race totals
    is.race = form %in% paste0('ASN', 4:9),
    # Hispanic ethnicity
    is.hisp = form %in% 'ASOB',
    # Age group total by poverty level (age total only)
    is.pov.age     = form %in% 'AS75' & grepl('\\d', var_label) & !grepl('\\:', var_label),
    # Poverty-age combination groups
    is.pov.age.pov = form %in% 'AS75' & grepl('\\:', var_label)
  ) %>%
  # Change the var_label to universe for the race (var_label is currently 'total')
  mutate(var_label = ifelse(is.race, univ, var_label)) %>%
  # Now, arrange rows
  arrange(
    # Grand total + poverty universe total
    desc(is.total), desc(is.total.pov.univ),
    # Race and Hispanic ethnicity
    desc(is.race), desc(is.hisp),
    # Sex totals 
    desc(is.sex), 
    # Sex-age (grand and disability universe)
    desc(is.sex.age),
    # Sex-age-disability combos
    desc(is.pov.age), desc(is.pov.age.pov),
    # Now sort within constraints
    var_label,
    # sort geographies
    tract
  ) %>%
  # Pivot the constraint label columns
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf) %>%
  rbind(
    ancestry.tab %>%
      mutate(form = NA, var_desc = NA, univ = NA, vartype = 'is.anc') %>%
      select(form, tract, puma, var_label, var_desc, univ, E, M, vartype)
  )

y.tab
nrow(y.tab) / 197
# That's a lot!!


### Get bounds for PUMS binning

# Fine-grained age groupings
age.fine = y.tab %>% 
  filter(vartype %in% 'is.sex.age') %>% distinct(var_label) %>%
  mutate(age = sub('^[^\\:]+\\:\\s(\\d{2}).+', '\\1', var_label)) %>% 
  distinct(age) %>% pull()

# Age by poverty groupings
age.pov  = y.tab %>% 
  filter(vartype %in% 'is.pov.age') %>% distinct(var_label) %>%
  mutate(age = sub('^(\\d{2}).+', '\\1', var_label)) %>% 
  distinct(age) %>% pull()

# Income/poverty gropus
pov.bins = y.tab %>%
  filter(vartype %in% 'is.pov.age.pov') %>% distinct(var_label) %>%
  mutate(pov = sub('^[^\\:]+\\:\\s(\\d{1}\\.\\d{2}).+', '\\1', var_label)) %>%
  distinct(pov) %>% pull()

age.fine
age.pov
pov.bins

### Format the PUMS (x)

x.tab = pums %>%
  # Add and modify columns
  mutate(
    # change sex to character (for sorting)
    sex = as.character(as_factor(SEX)),
    # get age bins
    age.fine = cut(AGE, c(as.numeric(age.fine), Inf), right = FALSE, labels = age.fine),
    age.pov  = cut(AGE, c(as.numeric(age.pov ), Inf), right = FALSE, labels = age.pov ),
    pov      = cut(POVERTY / 100, c(as.numeric(pov.bins), Inf), right = FALSE, labels = pov.bins),
    # set pov to NA for individuals with poverty level zero?
    pov = ifelse(POVERTY < 1, NA, as.character(pov)),
    # change race to 0/1 (for model matrix)
    across(starts_with('RAC'), ~ . - 1),
    # recode hispanic ethnicity here
    HISPAN = ifelse(HISPAN %in% 1:4, 'hisp', 'not.hisp'),
    in.pov.univ = as.numeric(POVERTY > 0)
  ) %>%
  # Get ancestry counts here
  mutate(
    # Ancestry (this one will be a doozy!)
    across(
      c(ANCESTR1, ANCESTR2), 
      ~ case_match(
        .,
        # Western European (present in b04006)
        # (including 122 German Russians in here...)
        c(1, 3, 5, 8:9, 11, 20:22, 24, 26, 32, 46, 
          49:51, 77:78, 82, 84, 88:89, 91, 97:99, 122, 183) ~ 'ReWestEur',
        # White North Americans (not REALD but still present in b04006)
        # Scots-Irish, PA Dutch, Canadian, French Canadian, Acadian/Cajun, American
        c(87, 929, 931, 935, 936, 940) ~ 'Other.northam',
        # Eastern European (present in b04006)
        c(100, 115, 120, 125, 128:129, 144, 431) ~ 'ReEastEur',
        # Slavic European (present in b04006)
        c(103, 109, 111, 130, 142, 148, 152:154, 171, 176, 178) ~ 'ReSlavic',
        # Other European (in b04006, not corresponding to reald)
        # Eastern European NEC, European
        c(190, 195) ~ 'Other.euro',
        # Caribbean (present in b04006)
        c(300:302, 308, 310, 314, 322, 335:337) ~ 'ReCaribbean',
        # Hispanic South American
        c(360, 370) ~ 'ReHispSou',
        # Other "Arab" (in b04006 but NOT consistent for reald)
        # Algerian, Libyan, North African, Saudi, Yemeni, Kurdish
        c(400, 404, 411, 427, 435, 442, 496) ~ 'Other.arab',
        # North African
        c(402, 406, 429) ~ 'ReNoAfr',
        # Middle eastern
        # (including 600 Afghans here)
        c(416:417, 419, 421, 425, 434, 465, 482, 495, 600) ~ 'ReMidEast',
        # African (in reald and in b04006)
        c(510, 529, 534, 541, 553, 564, 566, 570, 576, 587:588, 593) ~ 'ReAfrican',
        # African other (in b04006 but NOT consistent for reald)
        # e.g., because this is a catch-all it includes Eritrean (ReEthiopian)...
        # Cameroonian, Congolese, Eritrean, Gambian, Guinean, Togo, West African,
        # African, Other Subsaharan African
        c(508, 515, 523, 527, 530, 586, 595, 598:599)  ~ 'Other.african',
        # Ethiopian
        522 ~ 'ReEthiopian',
        # Somalian
        568 ~ 'ReSomalian',
        # ANZAs (in b04006 but NOT consistent for reald)
        c(800, 803) ~ 'Other.anza',
        # Other groups not present in b04006 (assigned to 'other groups')
        # Flemish, British Isles, Prussian, Sicilian, Belorussian, Cossack,
        # Bohemian, Rom, Moldov(i)an, Uzbek, Central Eur., Southern Eur., Western
        # Eur., Spanish, all Hispanic, Grenadian, St. Lucian, "Middle Eastern",
        # all non-Afghan Asians, all Pacific islanders, var. American ethnicities,
        # Mixture, Other
        c(9, 12, 40, 68, 102, 108, 112, 124, 146, 169, 181, 
          185, 187, 200:295, 329, 331, 490, 
          603:799, 808:870, 900:924, 983:995, 998) ~ 'Other',
        .default = NA
      )
    )
  ) %>%
  ### Get ancestry group counts
  # this step is heinously slow, probably not programmed super well...
  # Pivot out to get ancestry and counts in two columns
  pivot_longer(cols = contains('ANC'), names_to = 'a12', values_to = 'ancestry') %>%
  # add a count for how many times each ancestry group is recorded for each respondent
  group_by(CBSERIAL, PERNUM, PUMA, ancestry) %>%
  add_count(name = 'n.ancestry') %>%
  # drop a12 because col it is unnecessary
  select(-a12) %>%
  # distinct will get rid of duplicates
  distinct(.keep_all = TRUE) %>%
  # Now, make some edits to the ancestry 
  group_by(CBSERIAL, PERNUM, PUMA) %>%
  mutate(
    n.ancestry = case_when(
      # Where there are NAs and non-NA ancestry counts, switch NA = 1 to NA = 0
      is.na(ancestry) & any(!is.na(ancestry)) ~ 0,
      # Where there are 2 NAs (i.e., no other ancestry counts) switch from NA = 2 to NA = 1
      is.na(ancestry) & !any(!is.na(ancestry)) ~ 1,
      .default = n.ancestry
    )
  ) %>%
  ungroup() %>%
  # Filtering out individuals with extraneous ancestry (NA = 0 where other
  # ancestry is assigned, see above)
  filter(n.ancestry > 0) %>%
  # Add in 'unidentified' or whatever for the missing ancestries
  mutate(ancestry = ifelse(is.na(ancestry), 'unclassified', ancestry))

# That was pretty fast for some unknown reason... 

x.mat = x.tab %>%
  # Arrange rows (so that columns are ordered properly)
  complete(
    nesting(RACAMIND, RACASIAN, RACBLK, RACPACIS, RACOTHER, RACWHT), HISPAN,
    sex, nesting(age.fine, age.pov), pov, ancestry,
  ) %>%
  arrange(
    HISPAN, sex, age.fine, age.pov, pov, ancestry,
    CBSERIAL, PERNUM
  ) %>%
  mutate(
    # in total
    total = 1,
    # in disability universe (age groups)
    total.pov.univ = in.pov.univ,
    # race dummy variables
    rac.amind = RACAMIND,
    rac.asian = RACASIAN,
    rac.black = RACBLK,
    rac.nhpac = RACPACIS,
    rac.other = RACOTHER,
    rac.white = RACWHT,
    # Hispanic ethnicity
    hisp.cols = HISPAN,
    hisp.ones = 1,
    # sex groupings
    sex.cols = sex,
    sex.ones = 1,
    # sex-age groupings
    sex.age.sex = sex,
    sex.age.age = age.fine,
    sex.age.ones = 1,
    # age (by poverty binning) in univ. totals
    pov.age  = age.pov,
    pov.ones = in.pov.univ,
    # age-poverty groups
    pov.age.age  = age.pov,
    pov.age.pov  = pov,
    pov.age.ones = in.pov.univ
    # ancestry is handled above
  ) %>%
  ### Pivot!
  # no need to pivot totals
  # race columns are handled above
  # pivot out hispanic columns
  pivot_wider(names_from = hisp.cols, values_from = hisp.ones, values_fill = 0) %>%
  # pivot out sex totals
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  # pivot out sex-age combinations
  pivot_wider(names_from = c(sex.age.sex, sex.age.age), values_from = sex.age.ones, values_fill = 0) %>%
  # pivot out age (poverty bins)
  pivot_wider(names_from = pov.age, values_from = pov.ones, names_prefix = 'p.age', values_fill = 0) %>%
  # pivot out poverty-age combos
  pivot_wider(names_from = c(pov.age.age, pov.age.pov), values_from = pov.age.ones, values_fill = 0) %>%
  # pivot out ancestry
  pivot_wider(names_from = ancestry, values_from = n.ancestry, values_fill = 0) %>%
  # deselect NA columns
  select(-c(matches('NA\\_', ignore.case = FALSE), matches('\\_NA', ignore.case = FALSE))) %>%
  # filter out dummy records made by the complete() above
  filter(!is.na(CBSERIAL)) %>%
  arrange(CBSERIAL, PERNUM) %>%
  select(CBSERIAL, PERNUM,  PERWT, PUMA, total:last_col())

# Make duplicated synthetic records
x.mat.synthetic = merge(
  synthetic.records %>% filter(!is.na(CBSERIAL)) %>% select(CBSERIAL, PERNUM, PUMA),
  x.mat %>% select(-PUMA)
) %>%
  arrange(CBSERIAL, PERNUM, PUMA) %>%
  mutate(CBSERIAL = -1 * (1:nrow(.)), PERNUM = 1, PERWT = 1)

x.mat = rbind(x.mat, x.mat.synthetic) %>%
  arrange(CBSERIAL, PERNUM, PUMA)

nrow(x.mat)  
ncol(x.mat)

x.mat[,-(1:4)] %>% apply(2, sum)
x.mat[,-(1:4)] %>% apply(1, sum) %>% table()

slicey = 1:100

data.frame(
  x = names(x.mat)[-(1:4)],
  y = y.tab %>% filter(tract %in% 1301) %>% select(var_label, vartype)
)
# looks aligned to moi


# ==============================================================
# Press go button!!!

### Run downscale
fit.list = map2(
  .x = split(x.mat, ~ PUMA),
  .y = split(y.tab, ~ puma),
  function(x, y) {
    
    # # # define variables
    # total universe size
    N = y %>% filter(vartype %in% 'is.total') %>% pull(E) %>% sum()
    # PUMS sample size
    n = nrow(x)
    # Number of counties
    J = y %>% distinct(tract) %>% nrow()
    
    # # # Prepare response and margins of error
    # Response
    Y = y$E / N
    # Error matrix
    v = (y$M * n / (N^2)) %>%
      .sparseDiagonal(n = length(.)) %>% 
      as('generalMatrix')
    
    # # # Prepare PUMS data
    # Prepare X matrix
    # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED
    X = kronecker(t(as.matrix(x[, -(1:4)])), .sparseDiagonal(n = J)) %>%
      t() %>%
      as('dgCMatrix')
    # prior weights
    # CHECK HHWT vs. PERWT
    q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
    
    return(PMEDM_solve(X, Y, v, q, opt = list(tr_rad = 2)))
    
  }
)


# ==============================================================
# Evaluate fits

### Extract constraints
con.list = map2_df(
  .x = fit.list,
  .y = split(y.tab, ~ puma),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[vartype %in% 'is.total']))
) 

# Okay let's see what happens.

con.list %>% 
  count(puma, in.mar = abs(E - pred) < M) %>%
  pivot_wider(names_from = in.mar, values_from = n, names_prefix = 'in.mar', values_fill = 0)
# bleh... good, not great

con.list %>% 
  count(vartype, in.mar = abs(E - pred) < M) %>%
  pivot_wider(names_from = in.mar, values_from = n, names_prefix = 'in.mar', values_fill = 0)
# it's sex-age...?

con.list %>% filter(abs(E - pred) > M) %>% print(n = nrow(.))
# okay something weird is happening with the sex-age... and it looks like it's confined to a handful of tracts?
# (I guess some amount of deviation is to be expected with the MOEs but still)

con.list %>%
  filter(abs(E - pred) > M) %>%
  ggplot(aes(x = E)) +
  geom_segment(aes(x = E, xend = E, y = E - M, yend = E + M), linewidth = 0.2) +
  geom_point(aes(y = pred, fill = vartype), size = 3, shape = 21)

# Missing ancestry samples (handled before)
# plus two random misses from the same tract (weirdly with the same estimate)
# - sex-age (18-19 y/o women)
# - somali ancestry?

# pplt = con.list %>%
#   filter(tract %in% c(601, 8301:8302, 1602), vartype %in% 'is.sex.age') %>%
#   separate_wider_delim(var_label, delim = ': ', names = c('sex', 'ages'))
# 
# pplt %>%
#   ggplot(aes(x = ages)) +
#   # geom_ribbon(aes(ymin = E-M, ymax = E+M), alpha = 0.2) +
#   geom_point(aes(y = E), size = 3) +
#   geom_point(aes(y = pred), shape = 21, size = 3) +
#   facet_wrap(~ paste0(sex, tract), nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90))

# pplt = con.list %>%
#   filter(puma %in% 5103, vartype %in% 'is.sex.age', grepl('18', var_label)) %>%
#   separate_wider_delim(var_label, delim = ': ', names = c('sex', 'ages')) %>%
#   group_by(sex) %>%
#   arrange(E) %>%
#   mutate(ii = 1:n()) %>%
#   ungroup()
# 
# pplt %>%
#   ggplot(aes(x = factor(tract))) +
#   geom_segment(aes(xend = factor(tract), y = E-M, yend = E+M, colour = sex)) +
#   geom_point(aes(y = E, colour = sex), size = 3) +
#   geom_point(aes(y = pred, colour = sex), shape = 21, size = 3) +
#   # scale_x_continuous(labels = tract) +
#   # facet_wrap(~ sex, nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90))
# 
# pplt %>% filter(tract %in% 8301)
# Okay I don't really know what's going on here...
# except to say that is kind of a small number for girls...

# pplt = con.list %>% filter(puma %in% 5103, var_label %in% 'ReSomalian')
# 
# pplt %>%
#   ggplot(aes(x = factor(tract))) +
#   geom_segment(aes(xend = factor(tract), y = E-M, yend = E+M)) +
#   geom_point(aes(y = E), size = 3) +
#   geom_point(aes(y = pred), shape = 21, size = 3) +
#   # scale_x_continuous(labels = tract) +
#   # facet_wrap(~ tract, nrow = 2) +
#   theme(axis.text.x = element_text(angle = 90))
# # hmm...

# oh... the only somalian in the PUMS for this PUMA is an 18 y/o woman
# and doesn't have a poverty level listed... 
# that's probably the issue here
# oh... but the totals are the same... interesting... maybe for anonymity
# oh well...

# ==============================================================
# Assign weights and export

allo.weights = map2(
  .x = split(x.mat, ~ PUMA),
  .y = fit.list,
  .f =  function(x.data, mod.fit) {
    # x.data: ACS matrix used for extraction
    # mod.fit: model object (i.e. what is returned pmedm)
    
    matr = matrix(
      data = mod.fit$p, nrow = nrow(x.data), byrow = TRUE,
      # NOTE the year column here... maybe it would be a good idea to make a
      # flexible 'id' column
      dimnames = list(with(x.data, paste(CBSERIAL, PERNUM, sep = '_')), NULL)
    )
    
    return(matr)
  }
)

allo.weights = map2(
  .x = allo.weights,
  .y = split(y.tab, ~ puma),
  function(p.matrix, y.data) {
    # p.matrix: output from extract.fun (matrix with rows = PUMS data, cols =
    # as-yet-unlabelled tracts)
    # y.data: constraint table, with a column for tract (TRACT)
    
    # Assign tract names
    tracts = y.data %>% distinct(tract) %>% pull()
    dimnames(p.matrix)[[2]] = paste(y.data$puma[1], tracts, sep = '_')
    
    # Normalize by population size
    pop.size = y.data %>% filter(vartype %in% 'is.total') %>% pull(E) %>% sum()
    p.matrix = p.matrix * pop.size
    
    # Return matrix
    return(p.matrix)
  }
)

allo.weights = allo.weights %>%
  lapply(
    FUN = function(m) {
      data.frame(m) %>%
        mutate(id = row.names(.)) %>%
        pivot_longer(-id, names_to = 'tract', values_to = 'alloc')
    }
  ) %>%
  do.call(what = rbind) %>%
  mutate(tract = gsub('X', '', tract)) %>%
  separate_wider_delim(id, delim = '_', names = c('cbserial', 'pernum')) %>%
  mutate(across(c(cbserial, pernum), as.numeric)) 

write.csv(allo.weights, row.names = FALSE, file = 'multnomah/03_downscale_out/poverty_all_weights.csv')
