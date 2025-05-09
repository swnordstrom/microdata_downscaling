library(dplyr)
library(tidyr)

or = read.table(
  'oregon_oha/01_data_inputs/geogs/st41_or_cou2020.txt', 
  sep = '|', header = TRUE
)

wa = read.table(
  'oregon_oha/01_data_inputs/geogs/st53_wa_cou2020.txt', 
  sep = '|', header = TRUE
)

head(or)
head(wa)

rbind(
  or %>% select(-c(CLASSFP, FUNCSTAT)),
  wa %>% filter(grepl('Clark', COUNTYNAME)) %>% select(-c(CLASSFP, FUNCSTAT))
) %>%
  write.csv(
    'oregon_oha/01_data_inputs/orwa_county_name_fips_table.csv',
    row.names = FALSE
  )
