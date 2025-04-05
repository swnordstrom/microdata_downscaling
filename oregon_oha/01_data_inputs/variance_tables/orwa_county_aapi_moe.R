library(ggplot2)
library(dplyr)
library(tidyr)

county.b0 = read.csv('oregon_oha/01_data_inputs/variance_tables/B02001_county.csv', encoding = 'latin1')

county.aapi = county.b0 %>% 
  filter(grepl('\\,\\sOregon', NAME) | NAME %in% 'Clark County, Washington') %>%
  filter(ORDER %in% 5:6) %>%
  pivot_longer(cols = contains('Var_Rep'), names_pattern = 'Var\\_Rep(.+)', names_to = 'var_rep', values_to = 'var_val') %>%
  group_by(GEOID, NAME, var_rep) %>%
  mutate(diffsq = (sum(var_val) - sum(ESTIMATE))^2) %>%
  group_by(GEOID, NAME) %>%
  summarise(
    variance = (4/80) * sum(diffsq),
    stderror = sqrt(variance),
    mrgerror = 1.645 * stderror
  )

county.aapi %>% print(n = nrow(.))

# for comparison
county.b0 %>% 
  filter(grepl('\\,\\sOregon', NAME) | NAME %in% 'Clark County, Washington') %>%
  filter(ORDER %in% 5:6) %>%
  select(1:9)

write.csv(
  county.aapi, row.names = FALSE,
  'oregon_oha/01_data_inputs/variance_tables/orwa_county_aapi_moe.csv'
)
