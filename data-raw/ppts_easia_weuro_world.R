## Code that generates ppts_easia_weuro_world.rda
# Load library
library(tidyverse)

# A. Load the raw input file
ppts_global <- read_csv("data-raw/ppts_easia_weuro_world_raw.csv")

# B. relabel variables
ppts_global <- ppts_global %>%
    rename(stats_youthpop = youthpop,
           stats_student = students,
           stats_teacher = teachers,
           stats_school = schools,
           stats_gdp = gdp,
           stats_enroll_ratio = enroll_ratio)

# C. re-order variables
ppts_global <- ppts_global %>%
    select(country, countrycode, area_in_country,
           year,
           stats_youthpop,
           stats_student,
           stats_teacher,
           stats_school,
           stats_gdp,
           stats_enroll_ratio)

# D. country code, add for all, and within country name
ppts_global %>% distinct(country, countrycode)
# ppts_global <- ppts_global %>%
#   mutate(countrycode =
#            case_when(country == "Austria" ~ "AUT",
#                      country == "China" ~ "CHN",
#                      country == "France" ~ "FRA",
#                      country == "Germany" ~ "DEU",
#                      country == "Japan" ~ "JPN",
#                      country == "Korea" ~ "KOR",
#                      country == "Taiwan" ~ "TWN"))
ppts_global <- ppts_global %>%
  mutate(area_in_country =
           case_when(area_in_country == "urban" ~ "urban",
                     area_in_country == "Non-urban" ~ "non-urban",
                     TRUE ~ area_in_country))

# E. order rows
ppts_global %>%
    group_by(country, countrycode, area_in_country) %>% tally()
ppts_global <- ppts_global %>%
    arrange(country, area_in_country, year)
str(ppts_global)

# F. Skeleton years and merge
# Construct skeleton data-frame with entries for each country
# and spanning minimum to maximum years
# F.1 Countries with start and end years
ppts_global_dist <- ppts_global %>%
    arrange(country, area_in_country, year) %>%
    group_by(country, area_in_country) %>%
    mutate(year_start = dplyr::first(year),
           year_end = dplyr::last(year)) %>%
    select(country, countrycode, area_in_country,
           year_start, year_end) %>%
    slice_head(n=1) %>%
    mutate(year_span = year_end - year_start + 1)
print(ppts_global_dist)

# F.2 Uncount to panel
ppts_global_skeleton <- ppts_global_dist %>%
    uncount(year_span) %>%
    mutate(year = row_number() + year_start - 1) %>%
    select(-year_start, -year_end)
# View(ppts_global_skeleton)

# F.3 Merge skeleton with dataset
ppts_global <- ppts_global_skeleton %>%
  left_join(ppts_global,
            by=(c('country'='country',
                  'countrycode'='countrycode',
                  'area_in_country'='area_in_country',
                  'year'='year'))) %>%
  ungroup()
ppts_global %>%
    group_by(country, countrycode, area_in_country) %>% tally()
# View(ppts_global)

# G. Variable to Factors
ppts_easia_weuro_world <- ppts_global %>% 
    mutate(country = as.factor(country),
           countrycode = as.factor(countrycode),
           area_in_country = as.factor(area_in_country))

# Convert the csv file to a r file in the data folder
write_csv(ppts_easia_weuro_world, "data/ppts_easia_weuro_world.csv")
usethis::use_data(ppts_easia_weuro_world, overwrite = TRUE)
