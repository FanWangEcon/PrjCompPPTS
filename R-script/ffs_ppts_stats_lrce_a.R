# We have the LRCE file, level, ratio, change, and elasticity.
# Step 1 of https://github.com/FanWangEcon/PrjCompPPTS/issues/7
# 'FLR'
# 1. Start with long file, keep all locations and location types, select a subst of years, select `teachers`, `schools`, `students`, `youthpop` so different ratios can be constructed, and levels we want kept
# 2. Vars kept from long to wide
#     - unit of obs: locType x loc x time
# 3. RATIO/LEVELS: Compute key ratios within time periods, year level/ratio stats done:
#     - Teacher over student: `var_rat_t2s`
#     - Teacher over youthpop: `var_rat_t2y`
#     - School over student: `var_rat_s2s`
#     - School over youthpop: `var_rat_sty`

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(PrjCompPPTS)

# Path root and data input file
spt_root_prj <- file.path(
    "C:", "Users", "fan",
    # "Documents",
    "Dropbox (UH-ECON)", "Comparative Consolidation",
    fsep = .Platform$file.sep
)
# Output folder path
spt_datatemp_folder <- file.path(spt_root_prj, "PrjCompPPTS", "data-temp",
    fsep = .Platform$file.sep
)
spt_output_folder <- spt_datatemp_folder
verbose <- TRUE
verbose_detail <- TRUE
it_verbose_detail_nrow <- 100
bl_temp_save <- FALSE
bl_main_save <- TRUE
# Load in hooked loan data

# We load in the key file and check
ppts_code_wrk <- ppts_country_code
# We load in the global population data.
ppts_wrk <- ppts_easia_weuro_world_pchg
if (verbose) {
    print(glue::glue("F-713479, S01"))
    print(colnames(ppts_wrk))
    print(ppts_wrk %>% distinct(variable))
    print(ppts_wrk %>% distinct(location_level))
    print(ppts_wrk %>% distinct(year_bins_type))
    print(ppts_wrk %>% distinct(year_bins, year_bins_type) %>%
        arrange(year_bins_type, year_bins), n = 200)
}
# We review all unique values in variables. We get all unique "variable" values, drop enroll_ratio
ar_st_vars <- unique(ppts_wrk %>% pull(variable))
ar_year_bins_type <- unique(ppts_wrk %>% pull(year_bins_type))
if (verbose) {
    print(glue::glue("F-713479, S02"))
    print(ar_st_vars)
    print(ar_year_bins_type)
}
# Conduct some basic sorting and other common operations on the file.
ppts_wrk <- ppts_wrk %>%
    arrange(location_level, location_code, year_bins)


# First, start with long file, keep all locations and location types, select a subst of years, select `teachers`, `schools`, `students`, `youthpop` so different ratios can be constructed, and levels we want kept
df_ysts <- ppts_wrk %>%
    filter(variable %in% c(
        "youthpop", "student",
        "teacher", "school"
    )) %>%
    filter(year_bins_type == "1940t2020i01") %>%
    mutate(year_bins = as.numeric(year_bins)) %>%
    filter(year_bins %in% c(1960, 1980, 1990, 2000, 2010, 2020))
# review years
if (verbose) {
    print(glue::glue("F-713479, S1"))
    print(unique(df_ysts %>% pull(year_bins)))
}

# Second, vars kept from long to wide: unit of obs: locType x loc x time
df_ysts_wide <- df_ysts %>%
    pivot_wider(
        id_cols = c("location_code", "location_level", "year_bins"),
        names_from = variable,
        names_prefix = "var_lvl_",
        names_sep = "_",
        values_from = c(value_interp1)
    )
if (verbose) {
    print(glue::glue("F-713479, S2"))
    print(glue::glue("dim of youth wide file: {dim(df_ysts_wide)}"))
}

# Third, RATIO/LEVELS: Compute key ratios within time periods, year level/ratio stats done:
# - Teacher over student: `var_rat_t2s`
# - Teacher over youthpop: `var_rat_t2y`
# - School over student: `var_rat_s2s`
# - School over youthpop: `var_rat_sty`
df_flr <- df_ysts_wide %>%
    mutate(
        var_rat_y2t = var_lvl_youthpop / var_lvl_teacher,
        var_rat_s2t = var_lvl_student / var_lvl_teacher,
        var_rat_y2s = var_lvl_youthpop / var_lvl_school,
        var_rat_s2s = var_lvl_student / var_lvl_school
    )
if (verbose) {
    print(glue::glue("F-713479, S3"))
    print(colnames(df_flr))
    print(glue::glue("dim of youth wide ratio file: {dim(df_flr)}"))
}

# Save file
if (bl_main_save) {
    spn_path <- file.path(
        spt_output_folder, "ppts_stats_flr.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_flr, spn_path)
    print(glue::glue("F-713479, S4"))
    print(glue::glue("File saved: {spn_path}"))
}