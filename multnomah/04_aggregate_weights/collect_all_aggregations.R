library(dplyr)
library(tidyr)
library(purrr)

rm(list = ls())

disabilities = read.csv('multnomah/04_aggregate_weights/phaseII_out/disability_out.csv')

poverty.stat = read.csv('multnomah/04_aggregate_weights/phaseII_out/poverty_out.csv')

vets.livcond = read.csv('multnomah/04_aggregate_weights/phaseII_out/vet-living-arrangement_out.csv')

housingcosts = read.csv('multnomah/04_aggregate_weights/phaseII_out/housing-cost_out.csv')

dim(disabilities)
dim(poverty.stat)
dim(vets.livcond)
dim(housingcosts)

all.tables = merge(disabilities, poverty.stat) %>%
  merge(vets.livcond) %>%
  merge(housingcosts) %>%
  rename_with(~ gsub('VET\\_(\\D{3})\\_', 'VET\\1_', .)) %>%
  # Get rid of duplicated `Total2` column
  select(-all_of(starts_with('TOTAL2')))

dim(all.tables)

head(all.tables)

names(all.tables)

all.tables %>% apply(2, \(x) any(is.na(x))) %>% table()
# # no NAs
all.tables %>% apply(2, \(x) !any(x > 0)) %>% table()
# # 80 entirely blank columns (this is fine)
# 
# to.rm = all.tables %>% apply(2, \(x) !any(x>0)) %>% which()

dim(all.tables)

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


# all.cols = names(all.tables)[-1]
# all.cols = gsub('VET\\_[A-Z]{3}\\_', 'VET_', all.cols)
# n.unders = sapply(all.cols, \(x) length(gregexpr('\\_', x)[[1]]))

empty.cols = all.tables %>% select(-tract) %>% apply(2, \(x) !any(x > 0))

column.key = data.frame(colname = names(all.tables)[-1]) %>%
  separate_wider_delim(colname, delim = '_', names = c('table', 'reald', 'ages'), cols_remove = FALSE) %>%
  mutate(
    ages = case_match(
      ages,
      'A' ~ 'ages 0-17',
      'B' ~ 'ages 18-54',
      'C' ~ 'ages 55-59',
      'D' ~ 'ages 60-84',
      'E' ~ 'ages 85 and above',
      'L' ~ 'all ages'
    ),
    reald = case_match(
      reald,
      'W' ~ 'White',
      'A' ~ 'Asian',
      'N' ~ 'Indigenous',
      'B' ~ 'Black',
      'P' ~ 'NHPI',
      'E' ~ 'MENA',
      'H' ~ 'Hispanic or Latino/a/x',
      'O' ~ 'other or unspecified race/ethnicity',
      'L' ~ 'all races/ethnicities',
      .default = NA
    ),
    tablename = case_match(
      table,
      'TOTCNI' ~ 'Total civiliian non institutionalized population', 
      'DISANY' ~ 'any disability', 
      'DISHEA' ~ 'hearing difficulty', 
      'DISVIS' ~ 'vision difficulty', 
      'DISCOG' ~ 'cognitive difficulty', 
      'DISAMB' ~ 'ambulatory difficulty', 
      'DISCAR' ~ 'self-care difficulty', 
      'DISIND' ~ 'independent living difficulty', 
      'POVTOT' ~ 'Total poverty universe', 
      'POV100' ~ 'below 100% FPL', 
      'POV250' ~ 'below 250% FPL', 
      'POV400' ~ 'below 400% FPL', 
      'VET' ~    'Total number of veterans', 
      'VETDIS' ~ 'Veterans with any disability', 
      'VETFAM' ~ 'Veterans in family households', 
      'VETALO' ~ 'Veterans living alone', 
      'VETGQR' ~ 'Veterans in group quarters', 
      'VETMUL' ~ 'Veterans in a multi-generational household', 
      'VETNFH' ~ 'Veterans in a non-family household', 
      'VETNVM' ~ 'Veterans never married', 
      'VETMAR' ~ 'Veterans currently married', 
      'VETDVW' ~ 'Veterans divorced or widowed', 
      'TOTAL' ~  'Total population', 
      'FAM' ~    'living in a family household', 
      'ALO' ~    'living alone', 
      'GRQ' ~    'living in group quarters', 
      'MUL' ~    'living in a multi-generational household', 
      'NFH' ~    'living in a non-family household', 
      'NVM' ~    'never married', 
      'MAR' ~    'currently married', 
      'DVW' ~    'divorced or widowed', 
      'TOTAL2' ~ 'Total (minus non-institutionalized group quarters)', 
      'INS' ~    'living in institutions', 
      'HOM' ~    'living in a household', 
      'REN' ~    'renting their household', 
      'OWN' ~    'owning their household', 
      'REN30' ~  'renting their household and paying >30% of income on rent', 
      'OWN30' ~  'owning their household and paying >30% of income on housing',
    ),
    people.who = case_when(
        grepl('^Veterans', tablename) ~ '',
        grepl('^Total', tablename) ~ '',
        grepl('^[Ll]iving', tablename) ~ 'People who are ',
        table %in% c('MAR', 'DVW') ~ 'People who are ',
        table %in% 'NVM' ~ 'People who have ',
        grepl('^[Rr]ent', tablename) ~ 'People ',
        grepl('^[Oo]wn', tablename) ~ 'People ',
        grepl('^[Bb]elow', tablename) ~ 'People ',
        grepl('^DIS', colname) ~ 'People with '
      ),
    estimategroup = case_when(
      grepl('CNI', table) | grepl('^DIS', table) ~ 1,
      grepl('^POV', table) ~ 2,
      .default = 3
    )
  ) %>%
  mutate(
    reportname = paste0(people.who, tablename, '; ', reald, '; ', ages),
    empty = ifelse(empty.cols, '*', ''),
    reportname = paste0(reportname, empty)
  ) %>%
  select(colname, estimategroup, reportname)

# Check for no commas
# column.key %>% filter(grepl('\\,', reportname))


all.tables.out = all.tables %>% mutate(across(-tract, ~ round(., 1)))

# Export key for tables
write.csv(
  column.key, row.names = FALSE,
  'multnomah/04_aggregate_weights/phaseII_out/column_table_key_2025-05-31.csv'
)

# Export data
write.csv(
  all.tables.out, row.names = FALSE,
  'multnomah/04_aggregate_weights/phaseII_out/all_tables_2025-05-31.csv'
)

# =====================================================================
# # Looking for table consistencies
# =====================================================================
# 
# ##### DISABILITY
# 
# all.tables %>%
#   select(tract, starts_with('TOTCNI'), starts_with('DIS')) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   filter(reald %in% 'L' | ages %in% 'L') %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   filter(DISANY > TOTCNI | DISHEA > DISANY | DISVIS > DISANY | DISCOG > DISANY | DISCAR > DISANY | DISIND > DISANY)
# 
# # okay... not sure how to sum things up, but I will do it later
# 
# ##### POVERTY
# 
# all.tables %>%
#   select(tract, starts_with('POV')) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   filter(POV100 > POVTOT | POV250 > POVTOT | POV400 > POVTOT | POV100 > POV250 | POV250 > POV400)
# 
# # good  
# 
# ##### VETS AND HOUSING
# 
# all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   # first do looking at non-reald only
#   filter(reald %in% 'L') %>%
#   filter(
#     VETDIS > VET | VETALO > VET | VETFAM > VET | VETGRQ > VET | VETMUL > VET | VETNFH > VET | 
#     VETDVW > VET | VETMAR > VET | VETNVM > VET
#   )
#     
# # Good!
# 
# all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   # first do looking at non-reald only
#   filter(!(reald %in% 'L')) %>%
#   select(where(~ all(!is.na(.)))) %>%
#   filter(HOM - TOTAL > 1)
# # okay... not sure why HOM is equal to total here...
# # why is this only two columns... (oh because it's a lot of removed zeros)
# # okay whatever
# 
# all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   # first do looking at non-reald only
#   filter(!(reald %in% 'L')) %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   filter(if_any(FAM:OWN30, ~ . > TOTAL))
# 
# # Good
# 
# all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   # first do looking at non-reald only
#   filter(!(reald %in% 'L')) %>%
#   pivot_wider(names_from = tab, values_from = n) %>%
#   filter(OWN < OWN30 | REN < REN30 )
# 
# # good
# 
# all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   # first do looking at non-reald only
#   filter(!(reald %in% 'L')) %>%
#   pivot_wider(names_from = tab, values_from = n, values_fill = 0) %>%
#   mutate(
#     hou.total = TOTAL - (FAM + ALO + NFH + GRQ),
#     hom.total = HOM - (REN + OWN)
#   ) %>%
#   pivot_longer(c(hou.total, hom.total), names_to = 'to', values_to = 'x') %>%
#   ggplot() +
#   geom_histogram(aes(x = x)) +
#   facet_wrap(~ to, scales = 'free')
# 
# # housing total is good
# # homeowner total is not though...
# 
# xxx = all.tables %>%
#   select(-c(starts_with('POV'), starts_with('DIS'), starts_with('TOTCNI'))) %>%
#   pivot_longer(-tract, names_to = 'varb', values_to = 'n') %>%
#   separate_wider_delim(varb, delim = "_", names = c('tab', 'reald', 'ages')) %>%
#   # first do looking at non-reald only
#   filter(!(reald %in% 'L')) %>%
#   pivot_wider(names_from = tab, values_from = n, values_fill = 0) 
# 
# xxx %>%
#   ggplot(aes(x = TOTAL, y = (FAM + NFH + GRQ))) +
#   annotate('segment', x = 0, xend = 3000, y = 0, yend = 3000) +
#   geom_point(size = 3)
# 
# 
# xxx %>%
#   ggplot(aes(x = TOTAL, y = (MAR + NVM + DVW))) +
#   annotate('segment', x = 0, xend = 3000, y = 0, yend = 3000) +
#   geom_point(size = 3)
# # nice!
# 
# xxx %>% filter(INS > GRQ)
# # good
# 
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
# # ah you know what, these are estimated from slightly different sets of constraints!
# 
# # I think that's all we can check for now
# 
# =====================================================================
