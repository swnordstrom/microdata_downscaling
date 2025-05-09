library(dplyr)
library(tidyr)

# Puma combination

p20 = read.csv('multnomah/01_raw_data/2020_Census_Tract_to_2020_PUMA.csv')
p10 = read.csv('multnomah/01_raw_data/2010_Census_Tract_to_2010_PUMA.csv')

puma.county = rbind(
  p20 %>% mutate(time = 2020), 
  p10 %>% mutate(time = 2010)
) %>%
  filter(STATEFP %in% 41 | (STATEFP %in% 53 & COUNTYFP %in% 11)) %>%
  distinct(STATEFP, COUNTYFP, PUMA5CE, time)

head(puma.county)

# write.csv(puma.county, 'orwa_puma_county_combined_2010-2020.csv', row.names = FALSE)

puma.county = puma.county %>%
  mutate(
    # Use case_match to match counties to mega-pumas
    # note: this will originally assign Clark WA to the wrong PUMA,
    # we'll correct this in a subsq step
    megapuma = case_match(
      COUNTYFP,
      c(17, 13, 31, 23, 69, 1, 65, 27, 55, 21, 61, 49, 63, 59) ~ 'central.northeast',
      c(35, 37, 25, 45) ~ 'south.southeast',
      29 ~ 'jackson',
      c(11, 15, 33) ~ 'southwest',
      19 ~ 'douglas',
      39 ~ 'lane',
      c(3, 43) ~ 'lane.benton',
      c(71, 53, 9, 41, 7, 57) ~ 'northwest',
      47 ~ 'marion',
      5 ~ 'clackamas',
      67 ~ 'washington',
      51 ~ 'multnomah',
      .default = NA
    ),
    megapuma = ifelse(STATEFP %in% 53, 'clark.wa', megapuma)
  )

# did everybody get an assignment
puma.county %>% filter(is.na(megapuma))
# good

puma.county %>%
  rename(
    STATE = STATEFP,
    COUNTY = COUNTYFP,
    PUMA = PUMA5CE
  ) %>%
  write.csv('oregon_oha/01_data_inputs/orwa_county_megapuma_2010-2020.csv', row.names = FALSE)


