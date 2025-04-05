library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

# Read in table with population estimates by tract
tra.pop = ipumsr::read_nhgis("01_raw_data/2018-2022_ACS_tract_population_sizes.zip") %>%
  filter(STUSAB %in% 'OR', COUNTYA %in% '051') %>%
  select(tract = TRACTA, TOTAL = AQNFE001) %>%
  mutate(tract = as.numeric(tract))

# Read in estimated tables
all.tab = paste0('03_downscale_out/', dir('03_downscale_out/')) %>%
  grep(pattern = '\\_raw\\.csv', x = ., value = TRUE) %>%
  lapply(read.csv)

sapply(all.tab, names)
sapply(all.tab, \(df) unique(df$table.code))

all.tab = do.call(rbind, all.tab)

all.tab = all.tab %>%
  mutate(allo = round(allo, 1)) %>%
  mutate(age.out = as.numeric(gsub('\\[(\\d{1,2})\\,.+', '\\1', age.out))) %>%
  merge(data.frame(age.let = LETTERS[1:5], age.out = c(0, 18, 55, 60, 85))) %>%
  select(-age.out) %>%
  arrange(tract, table.code, age.let) %>%
  pivot_wider(names_from = c(table.code, age.let), names_sep = '_', values_from = allo)

head(all.tab)

# Now merge estimated values with a total
all.tab = merge(all.tab, tra.pop, by = c('tract')) %>%
  arrange(tract) %>%
  select(tract, TOTAL, everything())

write.csv(all.tab, row.names = FALSE, file = '03_downscale_out/table_output.csv')

names(all.tab) %>%
  grep(pattern = '[^a-z]', x = ., value = TRUE) %>%
  gsub('\\_[A-Z]$', '', .) %>%
  unique() %>%
  data.frame(table.code = .) %>%
  write.csv('03_downscale_out/table_codelist.csv', row.names = FALSE)

### 2025 update

# Turns out the deliverable was language total + language-by-proficiency
# (before, I only gave language total + proficiency total)
# I re-ran the script (plus one change - including non-english speakers in the
# limited proficiency category) and here are the results. Here I'll prepare
# these and only these estimates for export.

# Readin
lang.leng.tab = read.csv('03_downscale_out/lang_and_prof_raw_spring2025.csv')

# Okay - I need to aggregate together the three 'other' groups into a single
# 'other' group
# I'll assign the language codes and the age-group codes during this step too.

lang.leng.out = lang.leng.tab %>%
  # Add in table codes etc. here
  mutate(
    lang.code = case_match(
      langd,
      'arabic' ~ 'ARB',
      'chinese' ~ 'CHN',
      'french.haitian.cajun' ~ 'FRN',
      'german.west.germanic' ~ 'GRM',
      'korean' ~ 'KRN',
      'russian.polish.slavic' ~ 'RUS',
      'spanish' ~ 'SPN',
      'tagalog' ~ 'TGL',
      'vietnamese' ~ 'VTN',
      'only.english' ~ 'ENG',
      c('asian.other', 'indo-european.other', 'lang.other') ~ 'OTH',
      .default = langd
    ),
    prof.code = case_match(
      group,
      'prof1' ~ 'PR1',
      'prof2' ~ 'PR2',
      'total' ~ 'TOT',
      .default = group
    ),
    age.code = case_match(
      age.out,
      '[0,18)' ~ 'A',
      '[18,55)' ~ 'B',
      '[55,60)' ~ 'C',
      '[60,85)' ~ 'D',
      '[85,Inf)' ~ 'E',
      .default = age.out
    )
  ) %>%
  group_by(tract, table.code = paste(lang.code, prof.code, age.code, sep = '_')) %>%
  summarise(n = sum(allo)) %>%
  pivot_wider(names_from = table.code, values_from = n) %>%
  ungroup()

write.csv(
  lang.leng.out, row.names = FALSE,
  '03_downscale_out/languages_languageprof_spring2025.csv'
)
