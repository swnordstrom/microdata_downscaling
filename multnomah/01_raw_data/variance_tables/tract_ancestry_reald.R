library(ggplot2)
library(dplyr)
library(tidyr)

rm(list = ls())

### Read in variance table

county.b0 = read.csv('multnomah/01_raw_data//variance_tables/B04006_multnomah_tract.csv')

head(county.b0)

# For estimates that are zero, need the k-value based on tract size
# k-value depends on the tract size
county.b0 %>% filter(grepl('[Tt]otal', TITLE)) %>% count(ESTIMATE < 5000)
county.b0 %>% filter(grepl('[Tt]otal', TITLE)) %>% count(ESTIMATE < 10000)
# Okay some are below 5K, some above... none above 10K

### Assign each group a RELD
county.b1 = county.b0 %>%
  # take out the super-groupings (Arab and SubSaharan Agrican totals) plus grand total
  filter(!grepl('\\:', TITLE)) %>%
  # Assign reald
  mutate(
    reald = case_match(
      ORDER,
      # East Europeans:
      # Albanians, Armenians, Estonians, Hungarians, Latvians, Lithuanians, Romanians, 
      c(3, 16, 37, 46, 52:53, 63) ~ 'ReEastEur',
      # West Europeans:
      # Alsatians, Austrians, Basques, Belgians, Brits, Celts, Danes, Dutch, English, Fins, French, Germans, German Russians (?), Greeks, Icelanders, Irish, Italians, Luxemborgers, Maltese, "Northern European", Norwegians, Porguguese, Scandanavians, Scots, Swedes, Swiss, Welsn
      c(4, 19:21, 23, 28, 33:34, 36, 39:40, 42:44, 47,
        49, 51, 54, 56, 58:59, 62, 65, 67, 89:90, 93) ~ 'ReWestEur',
      # North African:
      # Egyptians, Moroccans, 
      c(7, 11) ~ 'ReNoAfr',
      # Middle Eastern:
      # Afghans (?), Iraqis, Jordanians, Lebanese, Palestinians, Syrians, Arabs, Assyrians/Chaldeans/Syriacs, Cypriots, Iranians, Israelis, Turks
      c(2, 8:10, 12:14, 17, 30, 48, 50, 91) ~ 'ReMidEast',
      # Hispanic (Latin) South Americans:
      # Brazilians, Guyanese
      c(22, 45) ~ 'ReHisSou',
      # Slavic Europeans:
      # Bulgarians, Carpatho-Rusyns, Croats, Czechs, Czechoslovakians, Macedonians, Poles, Russians, Serbians, Slavs, Slovaks, Slovenes, Soviet Union, Ukranians, Yugoslavian
      c(24, 27, 29, 31:32, 55, 61, 64, 68:72, 92, 107) ~ 'ReSlavic',
      # Subsaharan African:
      # Cape Verdean, Ghanaian, Kenyan, Liberian, Nigerian, Senegalese, Sierra Leonean, South African (?), Sudanese, Ugandan, Zimbabwean, African, Other Subsaharan African
      c(74, 76:81, 83:86, 88) ~ 'ReAfrican',
      # Other african:
      # African (NOTE this one is uncertain...)
      87 ~ 'Other.african',
      # Ethiopian African
      # Ethiopian
      75 ~ 'ReEthiopian',
      # Somalian African
      # Somalian
      82 ~ 'ReSomalian',
      # Caribbean
      # Bahamian, Barbadian, Belizean, Bermudan, British West Indian, Dutch West Indian, Haitian, Jamaican, Trinidadian and Tobagonian, US VIian, West Indian, Other West Indian
      95:106 ~ 'ReCaribbean',
      # PLACEHOLDER: WhiteNAOther for North American white categories
      # American, Cajun, Canadian, French Canadian, Pennsylvania German, Scots-Irish
      c(5, 25:26, 41, 43, 60, 66) ~ 'Other.northam',
      # PLACEHOLDER: Anza for Australians + New Zealanders
      c(18, 57) ~ 'Other.anza',
      # PLACEHOLDER: misc/other European
      c(35, 38) ~ 'Other.european',
      # PLACEHOLDER: misc/other Arab (likely includes some North Africans...)
      15 ~ 'Other.arab',
      # All other
      108 ~ 'Other',
      # Unclassified
      109 ~ 'Unclassified.unknown',
      # Default is to its original name
      .default = TITLE
    )
  )

head(county.b1)

### Aggregate

county.b2 = county.b1 %>%
  mutate(NAME = gsub('Census\\sTract\\s(.+)\\,\\sMult.+', '\\1', NAME)) %>%
  group_by(GEOID, NAME, reald) %>%
  summarise(across(c(ESTIMATE, starts_with('Var_Rep')), sum)) %>%
  # Pivot out the variance replicates
  pivot_longer(starts_with('Var_Rep'), names_to = 'var.rep', values_to = 'n') %>%
  # Get sum of squares
  group_by(GEOID, NAME, reald, ESTIMATE) %>%
  summarise(varn = (4/80) * sqrt(sum((ESTIMATE - n)^2))) %>%
  # Get the average weight
  mutate(
    avg.weight = cut(ESTIMATE, breaks = c(0, 5000, 10000, Inf), right = FALSE, labels = c(4, 8, 10)),
    avg.weight = as.numeric(as.character(avg.weight)),
    varn = ifelse(!ESTIMATE, sqrt(16 * avg.weight), varn),
    MOE  = 1.645 * varn 
  ) %>%
  select(-c(avg.weight, varn)) %>%
  ungroup()

head(county.b2)

write.csv(county.b2, row.names = FALSE, 'multnomah/01_raw_data/variance_tables/tract_ancestry_reald.csv')
