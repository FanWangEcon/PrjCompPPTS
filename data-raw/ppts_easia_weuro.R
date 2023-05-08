## code to prepare `ppts_easia_weuro_chg` dataset goes here
# Load library
library(readr)


# A. Load the raw input file
ppts_easia_weuro <- read_csv("data-raw/ppts_easia_weuro_raw.csv")

# B. relabel variables
ppts_easia_weuro <- ppts_easia_weuro %>%
    rename(area_in_country = metro,
           stats_youthpop = youthpop,
           stats_student = students,
           stats_teacher = teachers,
           stats_school = schools,
           stats_gdp = gdp,
           stats_enroll_ratio = enrol_ratio)

# C. re-order variables
ppts_easia_weuro <- ppts_easia_weuro %>%
    select(country, countrycode, area_in_country,
           year,
           stats_youthpop,
           stats_student,
           stats_teacher,
           stats_school,
           stats_gdp,
           stats_enroll_ratio)

# D. country code, add for all, and within country name
ppts_easia_weuro %>% distinct(country, countrycode)
ppts_easia_weuro <- ppts_easia_weuro %>%
  mutate(countrycode =
           case_when(country == "Austria" ~ "AUT",
                     country == "China" ~ "CHN",
                     country == "France" ~ "FRA",
                     country == "Germany" ~ "DEU",
                     country == "Japan" ~ "JPN",
                     country == "Korea" ~ "KOR",
                     country == "Taiwan" ~ "TWN"))
ppts_easia_weuro <- ppts_easia_weuro %>%
  mutate(area_in_country =
           case_when(area_in_country == "Metro areas" ~ "urban",
                     area_in_country == "Non-metro areas" ~ "non-urban",
                     TRUE ~ "nationwide"))

# E. order rows
ppts_easia_weuro %>%
    group_by(country, countrycode, area_in_country) %>% tally()
ppts_easia_weuro <- ppts_easia_weuro %>%
    arrange(country, area_in_country, year)
str(ppts_easia_weuro)

# F. Skeleton years and merge
# Construct skeleton data-frame with entries for each country
# and spanning minimum to maximum years
# F.1 Countries with start and end years
ppts_easia_weuro_dist <- ppts_easia_weuro %>%
    arrange(country, area_in_country, year) %>%
    group_by(country, area_in_country) %>%
    mutate(year_start = dplyr::first(year),
           year_end = dplyr::last(year)) %>%
    select(country, countrycode, area_in_country,
           year_start, year_end) %>%
    slice_head(n=1) %>%
    mutate(year_span = year_end - year_start + 1)
print(ppts_easia_weuro_dist)

# F.2 Uncount to panel
ppts_easia_weuro_skeleton <- ppts_easia_weuro_dist %>%
    uncount(year_span) %>%
    mutate(year = row_number() + year_start - 1) %>%
    select(-year_start, -year_end)
View(ppts_easia_weuro_skeleton)

# F.3 Merge skeleton with dataset
ppts_easia_weuro <- ppts_easia_weuro_skeleton %>%
  left_join(ppts_easia_weuro,
            by=(c('country'='country',
                  'countrycode'='countrycode',
                  'area_in_country'='area_in_country',
                  'year'='year'))) %>%
  ungroup()
ppts_easia_weuro %>%
    group_by(country, countrycode, area_in_country) %>% tally()
View(ppts_easia_weuro)

# Convert the csv file to a r file in the data folder
write_csv(ppts_easia_weuro, "data-raw/ppts_easia_weuro.csv")
usethis::use_data(ppts_easia_weuro, overwrite = TRUE)
