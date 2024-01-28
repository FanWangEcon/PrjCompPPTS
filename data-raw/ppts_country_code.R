## Code that generates ppts_easia_weuro_world.rda
# Load library
library(tidyverse)

# Import
ppts_country_code <- read_csv("data-raw/ppts_country_code.csv")

# Convert
ppts_country_code <- ppts_country_code %>%
    rename(
        location_code = Code,
        location_name = Economy,
        location_region_group = Region,
        location_income_group = `Income group`,
        location_wblend_group = `Lending category`
    )

# Categories for regions
print(ppts_country_code %>% distinct(location_region_group))

# Refactoring the location_region_groups
# EAS = East Asia & Pacific
# ECS = Europe and Central Asia
# do not use ECA, which is "Europe & Central Asia (excluding high income),ECA"
region_levels_full <- c(
    "Sub-Saharan Africa (SSF)" = "Sub-Saharan Africa",
    "Middle East & North Africa (MEA)" = "Middle East & North Africa",
    "Latin America & Caribbean (LCN)" = "Latin America & Caribbean",
    "North America (NAC)" = "North America",
    "South Asia (SAS)" = "South Asia",
    "Europe & Central Asia (ECS)" = "Europe & Central Asia",
    "East Asia & Pacific (EAS)" = "East Asia & Pacific"
)
region_abb <- c(
    "1SSF" = "Sub-Saharan Africa",
    "2MEA" = "Middle East & North Africa",
    "3LCN" = "Latin America & Caribbean",
    "4NAC" = "North America",
    "5SAS" = "South Asia",
    "6ECS" = "Europe & Central Asia",
    "7EAS" = "East Asia & Pacific"
)
region_levels <- c(
    "1SSF" = region_abb[["1SSF"]],
    "2MEA" = region_abb[["2MEA"]],
    "3LCN" = region_abb[["3LCN"]],
    "4NAC" = region_abb[["4NAC"]],
    "5SAS" = region_abb[["5SAS"]],
    "6ECS" = region_abb[["6ECS"]],
    "7EAS" = region_abb[["7EAS"]]
)
ppts_country_code <- ppts_country_code %>%
    mutate(
        location_region_group_code =
            fct_recode(location_region_group, !!!region_levels),
        location_region_group =
            fct_recode(location_region_group, !!!region_levels_full),
    ) %>%
    mutate(location_region_group_code = as.character(location_region_group_code))

# Add in world region location_region_group_code 
# Write name with 0WORLD first to sort these first, then sort by the 
# same region sequence as country and regions.
ppts_country_code <- ppts_country_code %>% 
    mutate(location_code_adj = case_when(
        location_name == region_abb[["1SSF"]] ~ "0WORLD_1SSF",
        location_name == region_abb[["2MEA"]] ~ "0WORLD_2MEA",
        location_name == region_abb[["3LCN"]] ~ "0WORLD_3LCN",
        location_name == region_abb[["4NAC"]] ~ "0WORLD_4NAC",
        location_name == region_abb[["5SAS"]] ~ "0WORLD_5SAS",
        location_name == region_abb[["6ECS"]] ~ "0WORLD_6ECS",
        location_name == region_abb[["7EAS"]] ~ "0WORLD_7EAS",
        TRUE ~ location_code
    ))

print(ppts_country_code, n=300)

# Save
write_csv(ppts_country_code, "data/ppts_easia_weuro_world.csv", na = "")
usethis::use_data(ppts_country_code, overwrite = TRUE)
