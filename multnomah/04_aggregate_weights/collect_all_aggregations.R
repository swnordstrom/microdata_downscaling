library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

write.logic = FALSE

language.lep = read.csv('multnomah/04_aggregate_weights/phaseII_out/languages_out.csv')

disabilities = read.csv('multnomah/04_aggregate_weights/phaseII_out/disability_out.csv')

poverty.stat = read.csv('multnomah/04_aggregate_weights/phaseII_out/poverty_out.csv')

vets.livcond = read.csv('multnomah/04_aggregate_weights/phaseII_out/vet-living-arrangement_out.csv')

housingcosts = read.csv('multnomah/04_aggregate_weights/phaseII_out/housing-cost_out.csv')

dim(disabilities)
dim(poverty.stat)
dim(vets.livcond)
dim(housingcosts)
dim(language.lep)

all.tables = merge(language.lep, disabilities) %>%
  merge(poverty.stat) %>%
  merge(vets.livcond) %>%
  merge(housingcosts)

dim(all.tables)
head(all.tables)
names(all.tables)

### Pivoting to long form

# Get a wide form of the tabular dataset
all.tables.wide = all.tables %>%
  pivot_longer(-tract, names_to = 'col', values_to = 'n') %>%
  separate_wider_delim(col, names = c('col', 'race', 'age'), delim = '_')

### Linking column names to estimate groups (generating the column key)

empty.cols = all.tables %>% select(-tract) %>% apply(2, \(x) !any(x > 0))

column.key = data.frame(
  # column name (minus the tract)
  colname  = names(all.tables)[-1],
  # index used for row ordering
  colorder = 2:ncol(all.tables) - 1
) %>%
  #### Somewhat inelegant way of assigning estimate groups to each column
  merge(
    data.frame(colname = names(language.lep)[-1], language.lep = TRUE),
    all.x = TRUE
  ) %>%
  merge(
    data.frame(colname = names(disabilities)[-1], disabilities = TRUE),
    all.x = TRUE
  ) %>%
  merge(
    data.frame(colname = names(poverty.stat)[-1], poverty.stat = TRUE),
    all.x = TRUE
  ) %>%
  merge(
    data.frame(colname = c(names(vets.livcond)[-1], names(housingcosts)[-1]), vets.housing = TRUE),
    all.x = TRUE
  ) %>%
  mutate(across(where(is.logical), ~ ifelse(is.na(.), FALSE, .))) %>%
  pivot_longer(where(is.logical), names_to = 'estimategroup', values_to = 'tf') %>%
  filter(tf) %>% select(-tf) %>%
  #### Separate column names into table/column, race, age (keeping original column name)
  separate_wider_delim(colname, delim = '_', names = c('table', 'reald', 'ages'), cols_remove = FALSE) %>%
  #### Do some reformatting
  mutate(
    ## Age
    ages.long = case_match(
      ages,
      'A' ~ 'ages 0-17',
      'B' ~ 'ages 18-54',
      'C' ~ 'ages 55-59',
      'D' ~ 'ages 60-84',
      'E' ~ 'ages 85 and above',
      'L' ~ 'all ages'
    ),
    ## Format REALD/race+ethnicity
    reald.long = case_match(
      reald,
      'W' ~ 'White',
      'A' ~ 'Asian',
      'N' ~ 'Indigenous',
      'B' ~ 'Black',
      'P' ~ 'NHPI',
      'E' ~ 'MENA',
      'H' ~ 'Hispanic or Latina/o',
      'O' ~ 'other or unspecified race/ethnicity',
      'L' ~ 'all races/ethnicities',
      .default = NA
    ),
    ## Format table names
    tablename = case_match(
      table,
      'TOTAL' ~  'Total population', 
      'LNGTOT' ~ 'Total language-use population (i.e. ages 5+)',
      'SEXM'   ~ 'Total number of males',
      'SEXF'   ~ 'Total number of females',
      'LNGARB' ~ 'speak Arabic',
      'LNGARBLEP' ~ 'speak Arabic with limited English proficiency',
      'LNGCHN' ~ 'speak Chinese including Mandarin or Cantonese',
      'LNGCHNLEP' ~ 'speak Chinese including Mandarin or Cantonese with limited English proficiency',
      'LNGENG' ~ 'speak English as first language',
      'LNGFRN' ~ 'speak French including Haitian',
      'LNGFRNLEP' ~ 'speak French including Haitian with limited English proficiency',
      'LNGGRM' ~ 'speak German including Dutch',
      'LNGGRMLEP' ~ 'speak German including Dutch with limited English proficiency',
      'LNGKRN' ~ 'speak Korean',
      'LNGKRNLEP' ~ 'speak Korean with limited English proficiency',
      'LNGOTH' ~ 'speak all other languages',
      'LNGOTHLEP' ~ 'speak all other languages with limited English proficiency',
      'LNGRUS' ~ 'speak Russian or other Slavic language',
      'LNGRUSLEP' ~ 'speak Russian or other Slavic language with limited English proficiency',
      'LNGSPN' ~ 'speak Spanish',
      'LNGSPNLEP' ~ 'speak Spanish with limited English proficiency',
      'LNGTGL' ~ 'speak Tagalog',
      'LNGTGLLEP' ~ 'speak Tagalog with limited English proficiency',
      'LNGVTN' ~ 'speak Vietnamese',
      'LNGVTNLEP' ~ 'speak Vietnamese with limited English proficiency',
      'IMM'    ~ 'Immigrants',
      'TOTCNI' ~ 'Total civilian non institutionalized population (i.e. disability denominator)', 
      'DISANY' ~ 'any disability', 
      'DISHEA' ~ 'hearing difficulty', 
      'DISVIS' ~ 'vision difficulty', 
      'DISCOG' ~ 'cognitive difficulty', 
      'DISAMB' ~ 'ambulatory difficulty', 
      'DISCAR' ~ 'self-care difficulty', 
      'DISIND' ~ 'independent living difficulty', 
      'POVTOT' ~ 'poverty is estimated (i.e. poverty denominator)', 
      'POV100' ~ 'below 100% FPL', 
      'POV250' ~ 'below 250% FPL', 
      'POV400' ~ 'below 400% FPL', 
      'VHCDENOM' ~ 'Denominator for veterans/housing cost/living arrangement estimate',
      'VET'    ~ 'Total number of veterans', 
      'VETDIS' ~ 'Veterans with any disability', 
      'VETFAM' ~ 'Veterans in family households', 
      'VETALO' ~ 'Veterans living alone', 
      'VETGRQ' ~ 'Veterans in group quarters', 
      'VETMUL' ~ 'Veterans in a multi-generational household', 
      'VETNFH' ~ 'Veterans in a non-family household', 
      'VETNVM' ~ 'Veterans never married', 
      'VETMAR' ~ 'Veterans currently married', 
      'VETDVW' ~ 'Veterans divorced or widowed',
      'FAM' ~    'living in a family household', 
      'ALO' ~    'living alone', 
      'GRQ' ~    'living in group quarters', 
      'MUL' ~    'living in a multi-generational household', 
      'NFH' ~    'living in a non-family household', 
      'NVM' ~    'never married', 
      'MAR' ~    'currently married', 
      'DVW' ~    'divorced or widowed', 
      'INS' ~    'living in institutions', 
      'HOM' ~    'living in a household', 
      'REN' ~    'renting their household', 
      'OWN' ~    'owning their household', 
      'REN30' ~  'renting their household and paying >30% of income on rent', 
      'OWN30' ~  'owning their household and paying >30% of income on housing',
    )
  ) %>%
  mutate(
    ## "People who" to format column descriptions
    people.who = case_when(
        grepl('^Veterans', tablename) ~ '',
        grepl('^Total', tablename) ~ '',
        grepl('^IMM', colname) ~ '',
        grepl('DENOM', colname) ~ '',
        grepl('^[Ll]iving', tablename) ~ 'People who are ',
        table %in% 'POVTOT' ~ 'People for whom ',
        table %in% c('MAR', 'DVW') ~ 'People who are ',
        table %in% 'NVM' ~ 'People who have ',
        grepl('^[Rr]ent', tablename) ~ 'People ',
        grepl('^[Oo]wn', tablename) ~ 'People ',
        grepl('^[Bb]elow', tablename) ~ 'People ',
        grepl('^DIS', colname) ~ 'People with ',
        grepl('^LNG', colname) ~ 'People who ',
      ),
    estimategroup = case_match(
      estimategroup,
      'language.lep' ~ 1,
      'disabilities' ~ 2,
      'poverty.stat' ~ 3,
      'vets.housing' ~ 4
    )
  ) %>%
  #### Re-arrange rows to match column output
  arrange(colorder) %>%
  mutate(
    reportname = paste0(people.who, tablename, '; ', reald.long, '; ', ages.long),
    empty = ifelse(empty.cols, '*', ''),
    reportname = paste0(reportname, empty)
  ) %>%
  select(colname, table, reald, ages, estimategroup, reportname)

# Check for no commas
# column.key %>% filter(grepl('\\,', reportname))

if (write.logic) { 

  # Export key for tables
  write.csv(
    column.key, row.names = FALSE,
    'multnomah/04_aggregate_weights/phaseII_out/column_table_key_2025-06-24.csv'
  )
  
  # Export data in wide form
  write.csv(
    all.tables, row.names = FALSE,
    'multnomah/04_aggregate_weights/phaseII_out/all_tables_2025-06-24.csv'
  )
  
  # Export data in wide from
  write.csv(
    all.tables.wide %>% rename(Table = col, Race.ethnicity = race, Ages = age, Estimate = n),
    row.names = FALSE,
    'multnomah/04_aggregate_weights/phaseII_out/all_tables_wide_2025-06-24.csv'
  )

}


# =====================================================================
# # Additional (missing) aggregations
# =====================================================================

if (write.logic) {
  
  tr.br.key = read.csv('multnomah/01_raw_data/CenPop2020_Mean_TR41_to_ADVSD_districts.csv') %>%
    select(tract = TRACTCE, branch = Branch)
  
  by.branch = all.tables %>%
    select(
      tract,
      contains('TOTCNI_L'),
      starts_with('LNG'),
      matches('VHCDENOM\\_[^L]'),
      matches('GRQ\\_[^\\L]'),
      matches('MUL\\_[^\\L]')
    ) %>%
    merge(tr.br.key) %>%
    group_by(branch) %>%
    summarise(across(-c(tract), sum))
  
  branch.L.counts = by.branch %>%
    pivot_longer(-branch, names_to = 'col', values_to = 'n') %>%
    mutate(col = gsub('\\_[A-Z]$', '_L', col)) %>%
    group_by(branch, col) %>%
    summarise(n = sum(n)) %>%
    pivot_wider(names_from = col, values_from = n) %>%
    ungroup() %>%
    rename_with(tolower) %>%
    mutate(across(where(is.numeric), ~ round(., 1)))
  
  branch.L.counts = rbind(
    branch.L.counts,
    branch.L.counts %>%
      summarise(across(-branch, sum)) %>%
      mutate(branch = 'TOTAL') %>%
      select(names(branch.L.counts))
  )
  
}


# Converting branch-level estimates to long-format

if (write.logic) {

  data7 = read.csv('~/Desktop/data7.csv') %>%
    rename_with(.cols = -branch, .fn = toupper)
  
  data7.long = data7 %>%
    pivot_longer(-branch, names_to = 'Table.name', values_to = 'People') %>%
    separate_wider_delim(
      Table.name, delim = '_', cols_remove = FALSE,
      names = c('Table.code', 'Race.Ethnicity', 'Age.group')
    ) %>%
    mutate(column.order = 1:nrow(.)) %>%
    mutate(
      Estimate.group = case_when(
        Table.code %in% c('TOTAL', 'SEXM', 'SEXF') ~ 1,
        grepl('LNG', Table.code) ~ 1,
        grepl('^IMM', Table.code) ~ 1,
        Table.code %in% c('NEN', 'LEP') ~ 1,
        grepl('POV', Table.code) ~ 2,
        grepl('^DIS', Table.code) ~ 3,
        Table.code %in% 'TOTCNI' ~ 3,
        grepl('VET', Table.code) ~ 4,
        Table.code %in% c(
          'GRQ', 'MUL', 'GRP', 'INS', 'HOM', 'ALO', 'FAM', 'NFH',
          'OWN', 'OWN30', 'REN', 'REN30', 'HCO30', 'NVM', 'MAR', 'DVW',
          'VHCDENOM'
        ) ~ 4
      )
    )
   
  table.descs = read.csv('98_data_checks/multco_advsd/prc_for_mcadsvd_v07_tables.csv')

  data7.out = merge(
    data7.long %>% mutate(Table.name.race = paste(Table.code, Race.Ethnicity, sep = '_')), 
    table.descs %>% select(Table.name.race = Table.code, Primary.Tables),
    by = 'Table.name.race', all.x = TRUE
  ) %>%
    mutate(
      age.code = case_match(
        Age.group,
        'L' ~ 'all ages',
        'A' ~ 'ages <18',
        'B' ~ 'ages 18-54',
        'C' ~ 'ages 55-59',
        'D' ~ 'ages 60-84',
        'E' ~ 'ages 85+'
      ),
      Primary.Tables = paste(Primary.Tables, age.code)
    ) %>%
    arrange(column.order) %>%
    select(
      Table.name, Table.code, Race.Ethnicity, Age.group, Branch = branch,
      People, Estimate.group, Table.desc = Primary.Tables,
    )
    
  write.csv(
    data7.out, row.names = FALSE, 
    'multnomah/04_aggregate_weights/phaseII_out/all_tables_wide_2025-06-30.csv'
  )
  
}


# =====================================================================
# # Looking for table consistencies
# =====================================================================



##### DISABILITY

tables.wide %>%
  filter(col %in% 'TOTCNI' | grepl('^DIS', col)) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  # Checking:
  # - disability counts greater than the CNI total
  # - any individual disability counts greater than the disability total
  filter(DISANY > TOTCNI | DISHEA > DISANY | DISVIS > DISANY | DISCOG > DISANY | DISCAR > DISANY | DISIND > DISANY)

##### POVERTY

tables.wide %>%
  filter(grepl('^POV', col)) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  # Are any of the individual poverty counts greater than the poverty total
  # or, are any of the poverty groupings nested incorrectly
  filter(POV100 > POVTOT | POV250 > POVTOT | POV400 > POVTOT | POV100 > POV250 | POV250 > POV400)

# good

##### LANGUAGE

tables.wide %>%
  # select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
  filter(col %in% c('TOTAL', 'IMM') | grepl('^SEX', col) | grepl('^LNG', col)) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  pivot_longer(-c(tract, race, age, TOTAL), names_to = 'col', values_to = 'n') %>%
  filter(!is.na(n), !is.na(TOTAL)) %>%
  filter(n > TOTAL)
# Good - no group is higher than the total

tables.wide %>%
  # select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
  filter(grepl('^LNG', col), !grepl('TOT', col), !grepl('ENG', col)) %>%
  mutate(lang = gsub('LEP', '', col)) %>%
  mutate(group = ifelse(grepl('LEP', col), 'lep', 'total')) %>%
  select(-col) %>%
  pivot_wider(names_from = group, values_from = n) %>%
  filter(lep > total)
# Good - no group where LEP exceeds language total (should be obvious/duh)

# Checking to make sure the totals are correct...

tables.wide %>%
  filter(grepl('^LNG', col), !grepl('LEP', col)) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  pivot_longer(-c(tract, race, age, LNGTOT), names_to = 'lang', values_to = 'n.lang') %>%
  group_by(tract, race, age, LNGTOT) %>%
  summarise(n.total = sum(n.lang)) %>%
  # ggplot(aes(x = LNGTOT, y = n.total)) +
  # annotate('segment', x = 0, xend = 5000, y = 0, yend = 5000, linetype = 2, colour = 'gray77') +
  # geom_point()
  ggplot(aes(x = LNGTOT - n.total)) +
  geom_histogram()
# it's all rounding error

##### VETS AND HOUSING

# Making sure nothing exceeds the denominator we set

tables.wide %>%
  filter(!(grepl('^POV', col) | grepl('^DIS', col) | grepl('TOT', col) | grepl('LNG', col))) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  pivot_longer(-c(tract, race, age, VHCDENOM), names_to = 'col', values_to = 'n') %>%
  filter(!is.na(VHCDENOM), !is.na(n)) %>%
  filter(VHCDENOM < n)

# ooh... that's cool and good

tables.wide %>%
  # select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
  filter(!(grepl('^POV', col) | grepl('^DIS', col) | col %in% 'TOTCNI')) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  # Are any of the veteran subgroupings greater than the vet totals
  filter(
    VETDIS > VET | VETALO > VET | VETFAM > VET | VETGRQ > VET | VETMUL > VET | VETNFH > VET |
    VETDVW > VET | VETMAR > VET | VETNVM > VET
  )

# Good

tables.wide %>%
  # select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
  filter(!(grepl('^POV', col) | grepl('^DIS', col) | col %in% 'TOTCNI')) %>%
  pivot_wider(names_from = col, values_from = n) %>%
  # Are the owner/renter rent burden counts exceeding the totals, or,
  # are the 
  filter(OWN < OWN30 | REN < REN30) 

# good

# Do the owner/renter categories sum to the number of people living in a household?
# Do family + nonfamily households give us the number of people living in a household?

tables.wide %>%
  filter(col %in% c('FAM', 'ALO', 'NFH', 'GRQ', 'HOM', 'REN', 'OWN')) %>% 
  # first do looking at non-reald only
  filter(!(race %in% 'L')) %>%
  pivot_wider(names_from = col, values_from = n, values_fill = 0) # %>%
  ### Compare household totals to renter+owners
  # ggplot(aes(x = REN + OWN, y = HOM)) +
  # annotate('segment', x = 0, xend = 3500, y = 0, yend = 3500, linetype = 2, col = 'gray55') +
  # geom_point()
  # # this looks good
  # ggplot(aes(x = HOM - REN - OWN)) +
  # geom_histogram()
  # # all differences are numerical/rounding error
  ### Compare household total to non-family vs. family
  # ggplot(aes(x = HOM, y = NFH + FAM)) +
  # annotate('segment', x = 0, xend = 3500, y = 0, yend = 3500, linetype = 2, col = 'gray55') +
  # geom_point()
  # # looks good...
  # ggplot(aes(x = HOM - NFH - FAM)) +
  # geom_histogram()
  # # yep these are also just rounding error
  
# Looking to see of people living in a household + living in group quarters
# gives us the VHC denominator
tables.wide %>%
  filter(col %in% c('VHCDENOM', 'HOM', 'GRQ')) %>% 
  pivot_wider(names_from = col, values_from = n, values_fill = 0) %>%
  # mutate(htype.sum = GRQ + HOM) %>%
  # ggplot(aes(x = VHCDENOM, y = htype.sum)) +
  # annotate('segment', x = 0, xend = 3500, y = 0, yend = 3500, linetype = 2, colour = 'gray77') +
  # geom_point()
  ggplot(aes(x = VHCDENOM - GRQ - HOM)) +
  geom_histogram()
# rounding error

# Checking to make sure our marital statuses (stati?) add up to the total
tables.wide %>%
  filter(col %in% c('VHCDENOM', 'MAR', 'DVW', 'NVM')) %>% 
  pivot_wider(names_from = col, values_from = n, values_fill = 0) %>%
  # ggplot(aes(x = VHCDENOM, y = MAR + DVW + NVM)) +
  # annotate('segment', x = 0, xend = 3500, y = 0, yend = 3500, linetype = 2, colour = 'gray77') +
  # geom_point()
  ggplot(aes(x = VHCDENOM - MAR - DVW - NVM)) +
  geom_histogram()
# also within rounding error

# vvv = all.tables %>%
#   select(
#     tract, starts_with('VET'), starts_with('DISANY'), starts_with('FAM'), starts_with('ALO'),
#     starts_with('MUL'), starts_with('NFH'), starts_with('NVM'), starts_with('MAR'),
#     starts_with('DVW'), starts_with('GRQ')
#   ) %>%
#   pivot_longer(-tract, names_to = 'tab', values_to = 'est') %>%
#   separate_wider_delim(tab, delim = '_', names = c('tab', 'reald', 'ages')) %>%
#   mutate(
#     is.vet = ifelse(grepl('VET', tab), 'vet', 'total'),
#     tab = gsub('VET([A-Z]+)', '\\1', tab),
#     tab = gsub('DISANY', 'DIS', tab)
#   ) %>%
#   pivot_wider(names_from = is.vet, values_from = est)
# 
# vvv %>% count(tab, reald, ages, vet.est = !is.na(vet), tot.est = !is.na(total))
# 
# vvv %>%
#   filter(!is.na(vet), !is.na(total)) %>%
#   filter(vet > total)
# # ah you know what, these are estimated from slightly different sets of constraints

# Assessing totals

tables.wide %>%
  pivot_wider(names_from = col, values_from = n) %>%
  pivot_longer(-c(tract, race, age, TOTAL), names_to = 'col', values_to = 'n') %>%
  filter(!is.na(n), !is.na(TOTAL)) %>%
  group_by(col) %>%
  count(problem = ifelse(n > TOTAL, 'is.problem', 'no.problem')) %>%
  pivot_wider(names_from = problem, values_from = n)
# Yeah... that's a bummer man.

compare.totals = tables.wide %>%
  pivot_wider(names_from = col, values_from = n) %>%
  mutate(TOTAL2 = HOM + GRQ + INS) %>%
  filter(!is.na(TOTAL), !is.na(TOTAL2))

compare.totals %>%
  ggplot(aes(x = TOTAL, y = TOTAL2)) +
  geom_point()
# Very closely correlated, but some error  

with(compare.totals, cor(TOTAL, TOTAL2))  
# yeah 99.8% correlation lmao

compare.totals %>%
  select(tract, race, age, TOTAL, TOTAL2) %>%
  mutate(diff = TOTAL - TOTAL2) %>%
  arrange(desc(abs(TOTAL - TOTAL2))) %>%
  print(n = 30)

# Yeah, unavoidable.

# total.compare.wide = at.wide %>%
#   pivot_wider(names_from = col, values_from = value, values_fill = 0) %>%
#   pivot_longer(-c(tract, race, age, TOTAL), names_to = 'col') %>%
#   filter(TOTAL > 0)
# 
# total.compare.wide %>%
#   count(col, wrongo = ifelse(value - TOTAL > .1, 'is.wrong', 'is.correct')) %>%
#   pivot_wider(names_from = wrongo, values_from = n, values_fill = 0) %>%
#   filter(is.wrong > 0)
# 
# diff.total.compare = at.wide %>%
#   pivot_wider(names_from = col, values_from = value, values_fill = 0) %>%
#   filter(TOTAL > 0, TOTALP > 0)
# 
# diff.total.compare %>%
#   mutate(sig.diff = abs(TOTAL - TOTALP) > 1) %>%
#   ggplot(aes(x = TOTAL, y = TOTALP, fill = race, shape = sig.diff)) +
#   annotate('segment', x = 0.1, y = 0.1, xend = 3500, yend = 3500, linetype = 2, colour = 'gray44') +
#   geom_point(alpha = 0.25, size = 3,) +
#   scale_shape_manual(values = c(21, 24))
# 
# diff.total.compare %>%
#   filter(TOTALP > 0.1, TOTAL > 0.1) %>%
#   ggplot(aes(x = TOTAL, y = TOTALP, fill = race)) +
#   annotate('segment', x = 0.1, y = 0.1, xend = 3500, yend = 3500, linetype = 2, colour = 'gray44') +
#   geom_point(alpha = 0.25, size = 3, shape = 21) +
#   scale_x_log10() + scale_y_log10() 
# 
# diff.total.compare %>%
#   ggplot(aes(x = TOTAL - TOTALP)) +
#   geom_histogram()
# 
# diff.total.compare %>% 
#   reframe(decile = quantile(TOTAL - TOTALP, probs = (0:10)/10)) %>%
#   mutate(probs = (0:10)/10)
# 
# # wanna see some of the poverty as a fraction of whole (grand or universe)
# 
# at.wide %>%
#   filter(grepl('POV', col) | col %in% 'TOTALP') %>%
#   pivot_wider(names_from = 'col', values_from = value) %>%
#   mutate(
#     p100 = POV100 / TOTALP,
#     p100.univ = POV100 / POVTOT
#   ) %>%
#   ggplot(aes(x = p100.univ)) +
#   geom_histogram() +
#   facet_grid(race ~ age)

# =====================================================================

# old very junky code

# # Doing a slow way of looking for identical columns
# lala = expand.grid(i = 1:(ncol(all.tables)-1), j = 1:(ncol(all.tables)-1)) %>%
#   filter(i>j) %>%
#   mutate(is.same = NA)
# 
# for (k in 1:nrow(lala)) lala$is.same[k] = identical(all.tables[,1+lala$i[k]], all.tables[,1+lala$j[k]])
# 
# lala %>% 
#   filter(is.same) %>%
#   merge(data.frame(i = 1:(ncol(all.tables)-1), iname = names(all.tables)[-1])) %>%
#   merge(data.frame(j = 1:(ncol(all.tables)-1), jname = names(all.tables)[-1])) %>%
#   arrange(iname)
# 
# # Okay I am at peace with this.
# # I suspect this is from cases where there is only one record to downscale within a group