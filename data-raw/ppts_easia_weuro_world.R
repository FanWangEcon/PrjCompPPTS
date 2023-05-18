## Code that generates ppts_easia_weuro_world.rda
# Load library
library(tidyverse)

# Generate skeleton frame for countries, starting year for all countries frame
it_min_year <- 1980

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
    select(location_name, location_code, location_level,
           year,
           stats_youthpop,
           stats_student,
           stats_teacher,
           stats_school,
           stats_gdp,
           stats_enroll_ratio)

# D. location_name code, add for all, and within location_name name
# ppts_global %>% distinct(location_name, location_code)
# ppts_global <- ppts_global %>%
#   mutate(location_code =
#            case_when(location_name == "Austria" ~ "AUT",
#                      location_name == "China" ~ "CHN",
#                      location_name == "France" ~ "FRA",
#                      location_name == "Germany" ~ "DEU",
#                      location_name == "Japan" ~ "JPN",
#                      location_name == "Korea" ~ "KOR",
#                      location_name == "Taiwan" ~ "TWN"))
# ppts_global <- ppts_global %>%
#   mutate(location_level =
#            case_when(location_level == "urban" ~ "urban",
#                      location_level == "Non-urban" ~ "non-urban",
#                      TRUE ~ location_level))

# E. order rows
print(ppts_global %>%
    arrange(location_level, location_code) %>%
    group_by(location_level, location_name, location_code) %>% tally(),
    n= 300)
ppts_global <- ppts_global %>%
    group_by(location_level, location_name, location_code)
str(ppts_global)

# F. Skeleton years and merge
# Construct skeleton data-frame with entries for each location_name
# and spanning minimum to maximum years
# F.1 Countries with start and end years
ppts_global_dist <- ppts_global %>%
    arrange(location_name, location_level, year) %>%
    group_by(location_name, location_level) %>%
    mutate(year_start = min(dplyr::first(year), it_min_year),
           year_end = dplyr::last(year)) %>%
    select(location_name, location_code, location_level,
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
            by=(c('location_name'='location_name',
                  'location_code'='location_code',
                  'location_level'='location_level',
                  'year'='year'))) %>%
  ungroup()
ppts_global %>%
    group_by(location_level, location_name, location_code) %>% tally()
# View(ppts_global)

# G. Variable to Factors
ppts_easia_weuro_world <- ppts_global %>%
    mutate(location_name = as.factor(location_name),
           location_code = as.factor(location_code),
           location_level = as.factor(location_level)) %>%
    arrange(location_level, location_code, location_name)

# Convert the csv file to a r file in the data folder
write_csv(ppts_easia_weuro_world, "data/ppts_easia_weuro_world.csv", na = "")
usethis::use_data(ppts_easia_weuro_world, overwrite = TRUE)
