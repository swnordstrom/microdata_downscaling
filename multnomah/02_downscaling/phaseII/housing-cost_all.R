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
pums.raw.ddi = read_ipums_ddi('multnomah/01_raw_data/usa_00057.xml')

# Read in PUMS and subset to MultCo
pums.raw = read_ipums_micro(pums.raw.ddi)

# Read in tabular data
acs.tab.raw1 = read_nhgis('multnomah/01_raw_data/nhgis0038_csv.zip', file_select = 1) %>%
  filter(grepl('Multno', COUNTY))
acs.tab.raw2 = read_nhgis('multnomah/01_raw_data/nhgis0038_csv.zip', file_select = 2) %>%
  filter(grepl('Multno', COUNTY))

# PUMA-tract crosswalk
puma.tract = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv') %>%
  filter(STATEFP %in% 41, COUNTYFP %in% 51) %>%
  rename(state = STATEFP, county = COUNTYFP, puma = PUMA5CE, tract = TRACTCE)

# Read in the aggregated ancestry counts
ancestry.tab = read.csv('multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')

# Synthetic records
# Get synthetic records (PUMS and dummy)
# synthetic.records = read.csv('')

# ==============================================================
# Merge data products together and do some pre-processing

# ACS tabular controls
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
    # Handle age bins (no decimal)
    var_label = gsub('under\\s(\\d)', '00 to \\1', var_label),
    var_label = gsub('^(\\d{1})\\s', '0\\1 ', var_label),
    var_label = gsub('\\:\\s(\\d{1})\\s', ': 0\\1 ', var_label),
    # Handle percentages
    var_label = gsub('less\\sthan\\s(\\d)', '00.0 to \\1', var_label)
  ) %>%
  # Get rid of some duplicate cases
  filter(
    # Hispanic total (same as sex/age total)
    !(form %in% 'ASOB' & var_label %in% 'total')
  ) %>%
  # Change 'total' label to race for race columns
  mutate(
    var_label = ifelse(
      form %in% paste0('ASN', 4:9),
      gsub('^(.+)\\salone.+', '\\1', var_desc),
      var_label
    )
  )

# Format the ancestry table
ancestry.tab = ancestry.tab %>%
  mutate(tract = as.numeric(gsub('\\d+US41051', '', GEOID))) %>%
  merge(puma.tract %>% select(tract, puma)) %>%
  select(tract, puma, var_label = reald, E = ESTIMATE, M = MOE) %>%
  mutate(
    form = NA, var_desc = 'ancestry re-groupings', univ = 'people reporting ancestry'
  ) %>%
  select(names(acs.tab))

# Combine the acs tables into one:
acs.tab = rbind(acs.tab, ancestry.tab)

### PUMS microdata
# Get only relevant columns for PUMS and standardize the PUMA labels
pums = pums.raw %>%
  select(
    -c(
      YEAR, MULTYEAR, SAMPLE, SERIAL, CLUSTER, STATEFIP, COUNTYICP,
      STRATA, HISPAND, ANCESTR1D, ANCESTR2D, OWNERSHPD
    )
  )


# ==============================================================
# Get matrices/vectors for PMED-M


y.tab = acs.tab %>%
  mutate(
    ### various totals
    # grand
    is.total = form %in% 'ASNQ' & var_label %in% 'total',
    # housing tenure total
    is.total.ten = form %in% 'ASTM' & var_label %in% 'total',
    # renter total
    is.total.ren = form %in% 'ASVH' & var_label %in% 'total',
    # owner total
    is.total.own = form %in% 'ASWA' & var_label %in% 'total',
    ### race-ethnicity
    is.rac = form %in% paste0('ASN', 4:9),
    is.hsp = form %in% 'ASOB',
    ### sex/age-sex
    # grand-total for sexes
    is.sex = form %in% 'ASNQ' & grepl('(fe)?male$', var_label),
    # age-sex
    is.sex.age = form %in% 'ASNQ' & grepl('\\d', var_label),
    # housing tenure
    is.ten = form %in% 'ASTM' & grepl('occupied$', var_label),
    # renter percentage
    is.ren.pct = form %in% 'ASVH' & !grepl('total', var_label), # grepl('percent', var_label),
    # renter percentage (NA)
    # is.ren.pct.nc = form %in% 'ASVH' & grepl('not\\scomput', var_label),
    # renter other/NA?
    # householder by age
    is.own.age = form %in% 'ASWA' & grepl('years', var_label) & !grepl('\\:', var_label),
    # householder by age by percent
    is.own.age.pct = form %in% 'ASWA' & grepl('\\:', var_label), # grepl('\\:\\s\\d', var_label),
    # householder by age by percent (NA)
    # is.own.age.pct.nc = form %in% 'ASWA' & grepl('not\\scomput', var_label),
    # group quarters
    is.gquart = form %in% 'ATFB',
    # ancestry
    is.anc = grepl('ancestry', var_desc)
  ) %>%
  arrange(
    # Arrange totals
    desc(is.total), desc(is.total.ten), desc(is.total.ren), desc(is.total.own),
    # Arrange group quarters
    desc(is.gquart),
    # Arrange race and ethnicity
    desc(is.rac), desc(is.hsp),
    # Arrange age/age-sex
    desc(is.sex), desc(is.sex.age),
    # Arrange housing tenure
    desc(is.ten),
    # Arrange renter cost column
    desc(is.ren.pct), # desc(is.ren.pct.nc),
    # Arrange ownership cost
    desc(is.own.age), desc(is.own.age.pct), # desc(is.own.age.pct.nc),
    # Arrange ancestry
    desc(is.anc),
    # Alphabetize within groups and arrange tracts
    var_label, tract
  ) %>%
  pivot_longer(where(is.logical), names_to = 'vartype', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

### Get age bins

ages.fine = y.tab %>%
  filter(vartype %in% 'is.sex.age') %>%
  distinct(var_label) %>%
  mutate(age = gsub('(fe)?male\\:\\s(\\d{2}).*', '\\2', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

ages.own = y.tab %>%
  filter(vartype %in% 'is.own.age') %>%
  distinct(var_label) %>%
  mutate(age = gsub('householder\\s(\\d{2}).*', '\\1', var_label)) %>%
  distinct(age) %>% arrange() %>% pull()

pcts.ren = y.tab %>% 
  filter(vartype %in% 'is.ren.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('^(\\d{2}).+', '\\1', var_label)) %>%
  distinct(pct) %>% arrange() %>% pull()

pcts.own = y.tab %>% 
  filter(vartype %in% 'is.own.age.pct') %>% 
  distinct(var_label) %>%
  mutate(pct = gsub('.+\\:\\s(\\d{2}).+', '\\1', var_label)) %>%
  distinct(pct) %>% arrange() %>% pull()

ages.fine
ages.own
pcts.ren
pcts.own

### Format PUMS (X)

x.tab = pums %>%
  # Add and modify columns
  mutate(
    # change sex to character (for sorting)
    sex = as.character(as_factor(SEX)),
    # get age bins
    age.fine = cut(AGE, c(as.numeric(ages.fine), Inf), right = FALSE, labels = ages.fine),
    age.own  = cut(AGE, c(as.numeric(ages.own ), Inf), right = FALSE, labels = ages.own ),
    # housing tenure
    tenure = case_match(
      OWNERSHP,
      0 ~ 'NA',
      1 ~ 'owner',
      2 ~ 'renter'
    ),
    # estimate housing costs / income - need to do this separately for renters and owners
    own.pct.num = ifelse(
      # If respondent is homeowner with positive income and housing cost
      (OWNERSHP %in% 1 & HHINCOME > 0 & OWNCOST > 0),
      # get housing cost as a percentage of annual income
      100 * ((12 * OWNCOST) / HHINCOME),
      # otherwise give an NA
      NA
    ),
    ren.pct.num = ifelse(
      # If the respondent is a renter with positive income and rent cost,
      OWNERSHP %in% 2 & HHINCOME > 0 & RENTGRS > 0,
      # get rent as a percentage of annual income
      100 * ((12 * RENTGRS) / HHINCOME),
      # otherwise give NA
      NA
    ),
    # Get binned household or renter cost percentages
    own.pct = cut(own.pct.num, breaks = c(as.numeric(pcts.own), 100), right = FALSE, labels = pcts.own),
    # Get 'not computed' housing costs for owners
    own.pct = ifelse(
      OWNERSHP %in% 1 & !(HHINCOME > 0 & OWNCOST > 0),
      'NC',
      as.character(own.pct)
    ),
    ren.pct = cut(ren.pct.num, breaks = c(as.numeric(pcts.ren), 100), right = FALSE, labels = pcts.ren),
    # Get 'not computed' housing costs for renters
    ren.pct = ifelse(
      OWNERSHP %in% 2 & !(HHINCOME > 0 & RENTGRS > 0),
      'NC',
      as.character(ren.pct)
    ),
    # change race to 0/1 (for model matrix)
    across(starts_with('RAC'), ~ . - 1),
    # recode hispanic ethnicity here
    hispan = ifelse(HISPAN %in% 1:4, 'hisp', 'not.hisp'),
    # flag for group quarters
    in.gq = GQ %in% (3:4)
  ) %>%
  # Now do the ancestry:
  # Now, get ancestry:
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

# "is.total"          "is.total.ten"      "is.total.ren"      "is.total.own"      "is.rac"            
# "is.hsp"            "is.sex"            "is.sex.age"        "is.ten"            "is.ren.pct"        
# "is.ren.pct.na"     "is.own.age"       
# "is.own.age.pct"    "is.own.age.pct.na" "is.gquart"         "is.anc"  

x.mat = x.tab %>%
  # Case complete and alphabetize rows
  complete(
    sex, nesting(age.fine, age.own), hispan, ancestry, 
    tenure, in.gq, nesting(own.pct, ren.pct)
  ) %>%
  arrange(sex, age.fine, age.own, hispan, ancestry, tenure, own.pct, ren.pct) %>%
  # Make columns for pivoting  
  mutate(
    ### Totals
    # Grand total
    total = 1,
    # Tenure total (people in occupied housing units)
    total.ten = as.numeric(!in.gq),
    # Renter total (renter-occupied housing units)
    total.ren = as.numeric(OWNERSHP %in% 2 & PERNUM < 2),
    # Owner total (owner-occupied housing units)
    total.own = as.numeric(OWNERSHP %in% 1 & PERNUM < 2),
    ### Group quarters
    gquart = as.numeric(in.gq),
    ### Race
    rac.amind = RACAMIND,
    rac.asian = RACASIAN,
    rac.black = RACBLK,
    rac.pacis = RACPACIS,
    rac.other = RACOTHER,
    rac.white = RACWHT,
    ### Hispanic ancestry
    hsp.cols = hispan,
    hsp.ones = 1,
    ### Sex-age table
    # Sex totals
    sex.cols = sex,
    sex.ones = 1,
    # Sex-age groups
    sex.age.sex = sex,
    sex.age.age = age.fine,
    sex.age.one = 1,
    ### Tenure
    ten.cols = tenure,
    ten.ones = total.ten,
    ### Renter cost percentages
    ren.cols = paste0('ren.', ren.pct),
    ren.ones = total.ren,
    ### Owner cost table
    # Owner ages
    own.age.cols = paste0('own.', age.own),
    own.age.ones = total.own,
    # Owner age by cost
    own.age.age = paste0('own.', age.own),
    own.age.own = paste0('own.', own.pct),
    own.age.one = total.own,
    ### (ancestry does not need new columns - made above)
  ) %>%
  ### Pivot out columns:
  # Pivot out hispanic ethnicity
  pivot_wider(names_from = hsp.cols, values_from = hsp.ones, values_fill = 0) %>%
  # Pivot out sex
  pivot_wider(names_from = sex.cols, values_from = sex.ones, values_fill = 0) %>%
  # Pivot out sex-age
  pivot_wider(names_from = c(sex.age.sex, sex.age.age), values_from = sex.age.one, values_fill = 0) %>%
  # Pivot out tenure 
  pivot_wider(names_from = ten.cols, values_from = ten.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  ### Pivot out rental cost percentages
  pivot_wider(names_from = ren.cols, values_from = ren.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  # Pivot out homeowner-age columns
  pivot_wider(names_from = own.age.cols, values_from = own.age.ones, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  # Pivot out homeowner-age-cost columns
  pivot_wider(names_from = c(own.age.age, own.age.own), values_from = own.age.one, values_fill = 0) %>%
  # remove NAs
  select(-matches('NA')) %>%
  # Pivot out ancestry
  pivot_wider(names_from = ancestry, values_from = n.ancestry, values_fill = 0) %>%
  ### Get rid of extra columns from case completion
  filter(!is.na(CBSERIAL)) %>%
  ### arrange by serial and person numbers
  arrange(CBSERIAL, PERNUM) %>%
  ### Select columns
  select(CBSERIAL, PERNUM, PERWT, PUMA, total:last_col(), gquart)

### Check for alignment:

ncol(x.mat) - 4
y.tab %>% filter(tract %in% 101) %>% nrow()
# too many columns in X...

slicey = 1:50
slicey = slicey + 50
slicey = 1:118

data.frame(
  x = names(x.mat)[-(1:4)][slicey],
  y = y.tab %>% distinct(var_label, vartype)  %>% slice(slicey)
)
# believe we are good...


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
# YEYA

con.list %>%
  filter(abs(E - pred) > M)

# Cool - missing ancestry group from PUMS (other arab) and the Somali GQ record
