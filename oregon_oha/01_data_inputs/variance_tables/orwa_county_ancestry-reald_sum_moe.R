library(ggplot2)
library(dplyr)
library(tidyr)

county.b0 = read.csv('oregon_oha/01_data_inputs/variance_tables/B04006_county.csv', encoding = 'latin1') %>%
  filter(grepl('\\,\\sOregon', NAME) | NAME %in% 'Clark County, Washington')

# Okay... unique codes to consider
unique(county.b0$TITLE)

# I'm confused by what is happening with the colons:
# e.g., Arab: I assumed would be the sum of all subcategories therein
# but the estimate reported under `Arab:` is consistently undercounting the sum
# of the subcategories...
# Maybe just ignore the supergroupings? 
# Actually one plausible explanation is that there are people here reporting multiple ancestries...
# In which case taking a sum is probably not a good idea... hnghhh

# This would also apply to the `Total` column (and does, I checked)
# Perhaps this is fine as long as thte `Total` column is kept in place?
# `Total` gives us the number of people, but the aggregates within each reald
# should give us the number of times a response in each one of these constructed
# groups is registered...

# Okay let's try the aggregation

county.c0 = county.b0 %>%
  # take out the Arab, Subsaharan African and West Indies super groupings
  filter(grepl('Total', TITLE) | !grepl('\\:', TITLE)) %>%
  mutate(
    reald.grp = case_match(
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
      # Default is to its original name
      .default = TITLE
    )
  )

# There likely will be some iterating for this going forward.

county.d0 = county.c0 %>%
  pivot_longer(cols = contains('Var_Rep'), names_pattern = 'Var\\_Rep(.+)', names_to = 'var_rep', values_to = 'var_val') %>%
  group_by(GEOID, NAME, reald.grp, var_rep) %>%
  summarise(
    estimate = sum(ESTIMATE), 
    rep_estm = sum(var_val),
    largestm = max(MOE)
  ) %>%
  group_by(GEOID, NAME, reald.grp) %>%
  summarise(
    estimate = estimate[1],
    variance = (4/80) * sum((rep_estm - estimate)^2),
    stderror = sqrt(variance),
    # Based on advice (p. 11 of techn guide) to use largest obsv. MOE
    mrgerror = ifelse(estimate > 0, 1.645 * stderror, largestm)
  )

nrow(county.d0)

write.csv(
  county.d0, row.names = FALSE,
  'oregon_oha/01_data_inputs/variance_tables/orwa_county_ancestry-reald_sum_moe.csv'
)
