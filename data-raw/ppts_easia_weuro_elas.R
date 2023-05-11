# Compute all potential ealsticities in this file
# elasticity of all possibile variables to each other
# Load library
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(usethis)

library(PrjCompPPTS)

# 10. Get all unique "variable" values, drop enroll_ratio
# 20. Generate "variable"-specific dataframes
# 30. Merge "variable"-specific dataframe to main
# 40. Compute elasticity, column id numerator
# 50. Drop percentages, keep elasticity, rename with prefix
# 60. Reshape from wide to long, rename variable numerator, variable denominator
# 70. Keep elasticity, and also keep percentage changes, and also keep levels, keep all. 
# Goal: Generate a figure where percentage change in pop is x-axis and elasticity is y-axis
# Country/Continnent/Acrynym key file.

# A. Review global percentage change data ------
# Review all dataframe variables
ppts_wrk <- ppts_easia_weuro_world_pchg_interp1
str(ppts_wrk)
# Review all unique values in variables

# 10. Get all unique "variable" values, drop enroll_ratio
ar_st_vars <- unique(
    ppts_wrk %>%
    filter(variable != "enroll_ratio") %>%
    pull(variable))
print(ar_st_vars)

ppts_wrk_jnt <- ppts_wrk %>% select(-value)
ar_st_cur_col_var <- ar_st_vars
for (st_cur_col_var in ar_st_cur_col_var) {
#   st_cur_col_var <- "eleagepop"
  st_new_col_var <- paste0("pchg_", st_cur_col_var)
  # 20. Generate "variable"-specific dataframes
  ppts_one_var <- ppts_wrk %>%
    filter(variable == st_cur_col_var) %>%
    select(-value, - variable) %>%
    rename(!!sym(st_new_col_var) := pchg_yrspan_interp1)

  # 30. Merge "variable"-specific dataframe to main
  ppts_wrk_jnt <- ppts_wrk_jnt %>%
    left_join(ppts_one_var,
        by = (c(
            "country" = "country",
            "area_in_country" = "area_in_country",
            "year_bins_type" = "year_bins_type",
            "year_bins" = "year_bins"
        )))
}

# 60. Reshape from wide to long, rename variable numerator, variable denominator
ppts_wrk_jnt_ ppts_wrk_jnt %>% 
  pivot_longer(cols = starts_with('zi'),
               names_to = c('zi'),
               names_pattern = paste0("zi(.*)"),
               values_to = "ev")

# 50. Drop percentages, keep elasticity, rename with prefix
# 40. Compute elasticity, column id numerator
# 70. Keep elasticity, and also keep percentage changes, and also keep levels, keep all. 