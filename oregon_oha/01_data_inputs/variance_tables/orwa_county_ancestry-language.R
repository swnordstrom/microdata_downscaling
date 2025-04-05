library(PMEDMrcpp)
library(ggplot2)
library(ipumsr)
library(dplyr)
library(tidyr)
library(purrr)

# Clear namespace
rm(list = ls())

# -------------------------------------------------------------------
# Setup

### Read in PUMS data

# Read in ACS PUMA (five year sample)
acs.ddi = read_ipums_ddi('oregon_oha/01_data_inputs/usa_00044.xml')

# Make PUMS data frame and subset to OR + Clark WA
acs.pums = acs.ddi %>%
  # Read in data
  read_ipums_micro() %>%
  # Subset to Oregon + Clark County WA
  filter(STATEFIP %in% 41 | PUMA %in% c(11101:11104, 21101:21104))

### Read in ACS tabular data and subset to OR + Clark WA
tab.raw = read_nhgis('oregon_oha/01_data_inputs/nhgis0026_csv.zip', file_select = 2) %>%
  filter(STATEA %in% '41' | (STATEA %in% '53' & COUNTYA %in% '011'))

### Read in megapuma data
megapumas = read.csv('oregon_oha/01_data_inputs/orwa_county_megapuma_2010-2020.csv')

### Read in sums+moes for ancestry groups
ancestrs = read.csv('oregon_oha/01_data_inputs/variance_tables/orwa_county_ancestry-reald_sum_moe.csv')

# -------------------------------------------------------------------
# Combining files as needed

### Assign megapumas to PUMS
acs.pums = merge(
  acs.pums, megapumas %>% distinct(STATE, PUMA, megapuma),
  by.x = c('STATEFIP', 'PUMA'), by.y = c('STATE', 'PUMA'),
  all.x = TRUE
)

head(acs.pums)
sum(is.na(acs.pums$megapuma))

### Assign megapumas to ACS tables
acs.tab = merge(
  tab.raw %>% 
    select(STATEA, COUNTYA, matches('[A-Za-z0-9]{4}[ME]\\d{3}')) %>%
    mutate(across(c(STATEA, COUNTYA), as.numeric)),
  megapumas %>% filter(time %in% 2020) %>% distinct(STATE, COUNTY, megapuma),
  by.x = c('STATEA', 'COUNTYA'), by.y = c('STATE', 'COUNTY')
)

### Assign megapumas to ancestry table
ances.table = merge(
  ancestrs %>%
    select(-c(NAME, variance, stderror)) %>%
    mutate(GEOID = gsub('\\d+US', '', GEOID)) %>%
    separate_wider_position(GEOID, widths = c(STATE = 2, COUNTY = 3)) %>%
    mutate(across(c(STATE, COUNTY), as.numeric)),
  megapumas %>% filter(time %in% 2020) %>% distinct(STATE, COUNTY, megapuma)
) %>%
  # Change 'other groups' to 'other.groups' (for alphabetizing)
  mutate(reald.grp = ifelse(reald.grp %in% 'Other groups', 'other.groups', reald.grp))

# -------------------------------------------------------------------
# Format objects for downscaling routine

# Okay... I actually handled the ancestry groups in a separate file
# So we will only want the language info out of the `acs.tab` 

y.lang = acs.tab %>%
  select(-contains('ASZV')) %>%
  pivot_longer(cols = -c(COUNTYA, STATEA, megapuma), names_to = 'var_name', values_to = 'value') %>%
  merge(ipums_var_info(tab.raw, matches('[EM]\\d{3}$')) %>% select(var_name, var_label)) %>%
  # Convert variable info to lowercase for easier regexing
  mutate(var_label = tolower(var_label)) %>%
  # Split the variable name up so that we can pivot out estimates and margins
  # for same variable
  separate_wider_position(var_name, c(form = 4, me = 1, code = 3)) %>%
  mutate(
    var_label = gsub('estimates:\\s', '', var_label),
    var_label = gsub('margins\\sof\\serror\\:\\s', '', var_label)
  ) %>%
  pivot_wider(names_from = me, values_from = value) %>%
  select(-c(form, code)) %>%
  rename(STATE = STATEA, COUNTY = COUNTYA)

head(y.lang)

y.ance = ances.table %>%
  rename(
    var_label = reald.grp,
    E = estimate,
    M = mrgerror
  ) %>%
  mutate(var_label = tolower(var_label)) %>%
  select(names(y.lang))

y = rbind(
  y.lang %>% mutate(table_var = 'language'),
  y.ance %>% mutate(table_var = 'ancestry')
)

head(y)

y = y %>%
  # Change any negative MOE listings (placeholders for zero) with zero
  mutate(M = ifelse(M < 0, 0, M)) %>%
  # Classify constraints, arrange
  mutate(
    total.5p = table_var %in% 'language' & grepl('total', var_label),
    grp.lang = table_var %in% 'language' & !grepl('total', var_label) & !grepl('\\:', var_label),
    grp.lang.prof = table_var %in% 'language' & !grepl('total', var_label) & grepl('\\:', var_label),
    total.county = table_var %in% 'ancestry' & grepl('total', var_label),
    grp.ancestry = table_var %in% 'ancestry' & !grepl('total', var_label)
  ) %>%
  select(-table_var) %>%
  arrange(
    desc(total.county), desc(total.5p), desc(grp.lang), desc(grp.lang.prof), desc(grp.ancestry),
    var_label, STATE, COUNTY
  ) %>%
  pivot_longer(where(is.logical), names_to = 'var_type', values_to = 'tf') %>%
  filter(tf) %>% select(-tf)

# Okay... time now to get the x variables.

x = acs.pums %>%
  select(CBSERIAL, PERNUM, PERWT, AGE, ANCESTR1, ANCESTR2, LANGUAGE, SPEAKENG, megapuma) %>%
  # NOTE: will likely want to do a complete() in here for sorting...
  # also will want to sort here
  # variable recoding time
  mutate(
    # Language
    lang = case_match(
      # see lang12 in langx
      LANGUAGE,
      1 ~ 'speak.only.english',
      12 ~ 'spanish',
      57 ~ 'arabic',
      43 ~ 'chinese',
      11 ~ 'french.haitian.cajun',
      2:4 ~ 'german.west.germanic',
      49 ~ 'korean',
      18:26 ~ 'russian.polish.slavic',
      54 ~ 'tagalog',
      50 ~ 'vietnamese',
      0 ~ NA,
      # setdiffs here are probably not necessary but they will ease my mind
      setdiff(c(2:11, 13:32), c(2:4, 11, 18:26)) ~ 'other.indo-european',
      setdiff(40:56, c(43, 49, 50, 54)) ~ 'other.asian',
      c(33:39, 58:94, 96) ~ 'other.and.unspecified', # (note: 57 ius arabic handled above)
      # ah gotta add in some more others
      .default = NA
    ),
    # English proficiency *for those who do not have eng as first language*
    prof = case_when(
      LANGUAGE %in% 1 ~ NA,
      SPEAKENG %in% 4 ~ 1,
      SPEAKENG %in% 5:6 ~ 2,
      .default = NA
    ),
    # Ancestry (this one will be a doozy!)
    across(
      c(ANCESTR1, ANCESTR2), 
      .names = '{.col}_recode',
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
          603:799, 808:870, 900:924, 983:995, 998) ~ 'Other.groups',
        .default = NA
      )
    )
  ) %>%
  # Get ancestry group counts
  # this step is heinously slow, probably not programmed super well...
  # Pivot out to get ancestry and counts in two columns
  pivot_longer(cols = contains('recode'), names_to = 'a12', values_to = 'ancestry') %>%
  # add a count for how many times each ancestry group is recorded for each respondent
  group_by(CBSERIAL, PERNUM, ancestry) %>%
  add_count(name = 'n.ancestry') %>%
  # drop a12 because col it is unnecessary
  select(-a12) %>%
  # distinct will get rid of duplicates
  distinct(.keep_all = TRUE) %>%
  # Now, make some edits to the ancestry 
  group_by(CBSERIAL, PERNUM) %>%
  mutate(
    n.ancestry = case_when(
      # Where there are NAs and non-NA ancestry counts, switch NA = 1 to NA = 0
      is.na(ancestry) & any(!is.na(ancestry)) ~ 0,
      # Where there are 2 NAs (i.e., no other ancestry counts) switch from NA = 2 to NA = 1
      is.na(ancestry) & !any(!is.na(ancestry)) ~ 1,
      .default = n.ancestry
    )
  ) %>%
  ungroup()

x = x %>%
  filter(n.ancestry > 0) %>%
  mutate(ancestry = ifelse(is.na(ancestry), 'unclassified/missing', ancestry)) %>%
  complete(nesting(lang, prof), ancestry) %>%
  arrange(lang, prof, ancestry) %>%
  # Columns
  mutate(
    # overall county total
    total.county = 1,
    # total for language (universe is individuals 5+)
    total.5plus  = as.numeric(AGE > 4),
    # language
    lang.ones = as.numeric(AGE > 4),
    lang.cols = lang,
    # language+proficiency
    lang.prof.ones = as.numeric(AGE > 4),
    lang.prof.lang = lang,
    lang.prof.prof = prof
    # will NOT want separate ancestry name+ones columns here because that will duplicate people...
  ) %>%
  # Pivot language
  pivot_wider(names_from = lang.cols, values_from = lang.ones, values_fill = 0) %>%
  # Pivot language + proficiency
  pivot_wider(
    names_from = c(lang.prof.lang, lang.prof.prof), names_sep = '_',
    values_from = c(lang.prof.ones), values_fill = 0
  ) %>%
  # Pivot ancestry
  pivot_wider(names_from = ancestry, values_from = n.ancestry, values_fill = 0) %>%
  select(-contains('NA', ignore.case = FALSE)) %>%
  filter(!is.na(CBSERIAL)) %>%
  arrange(CBSERIAL, PERNUM)

# oh yeah and then do a filter to get rid of zeros
head(x)
names(x)

# Okay... looks like the first design column in tx is 12
names(tx)[-(1:11)] %>% length()
y %>% filter(COUNTY %in% 3) %>% nrow()

data.frame(
  x = names(tx)[-(1:11)],
  y = y %>% filter(COUNTY %in% 3) %>% pull(var_label)
)

# Alright we are aligned...
# Let's pu8sh the button and see what happens!!!

# -------------------------------------------------------------------
# Try running the downscaling routine

fit.list = map2(
    .x = split(x, ~ megapuma),
    .y = split(y, ~ megapuma),
    function(x, y) {
      
      # # # define variables
      # total universe size
      N = y %>% filter(var_type %in% 'total.county') %>% pull(E) %>% sum()
      # PUMS sample size
      n = nrow(x)
      # Number of counties
      J = y %>% distinct(STATE, COUNTY) %>% nrow()
      
      # # # Prepare response and margins of error
      # Response
      Y = y$E / N
      # Error matrix
      v = (y$M * n / (N^2)) %>%
        .sparseDiagonal(n = length(.)) %>% 
        as('generalMatrix')
      
      # # # Prepare PUMS data
      # Prepare X matrix
      # NOTE: NEED TO CHECK TO MAKE SURE PROPER COLUMNS ARE EXCLUDED (correct here)
      X = kronecker(t(as.matrix(x[, -(1:11)])), .sparseDiagonal(n = J)) %>%
        t() %>%
        as('dgCMatrix')
      # prior weights
      q = matrix(rep(x$PERWT, each = J), ncol = 1) %>% (\(m) m / sum(m))
      
      return(PMEDM_solve(X, Y, v, q, lambda = NULL))
      
    }
)

# Might be some convergence issues here...

### Extract constraints
con.list = map2_df(
  .x = fit.list,
  .y = split(y, ~ megapuma),
  function(out.fit, y) y %>% mutate(pred = out.fit$pred * sum(E[var_type %in% 'total.county']))
) 

con.list %>%
  ggplot(aes(x = E, y = pred)) +
  geom_point() +
  facet_wrap(~ megapuma, scales = 'free')

# Honestly that looks pretty good...

# Looking at prediction error versus margins
con.list %>% arrange(desc(abs(E - pred)))
# hmm... some very wide MOEs here so maybe this is fine?
# curiously a lot of these look like issues with spanish speaking populations??
# and marion county...

con.list %>% filter(M > 0) %>% count(in.margin = abs(E - pred) < M)
# yo... woke

con.list %>% filter(M > 0) %>% filter(abs(E - pred) > M)
# Hmm... failing with predicted number of Chinese speakers in Douglas County
# (any in PUMS? - I checked the answer is no)

con.list %>% filter(!M) %>% arrange(desc(abs(E - pred)))
# yoo these are actually insanely good?

