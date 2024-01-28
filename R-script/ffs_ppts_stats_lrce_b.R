# We have the LRCE file, level, ratio, change, and elasticity.
# Step 2 of https://github.com/FanWangEcon/PrjCompPPTS/issues/7
# FPC file

# 1. From levels/ratios file (keep ranks), variables are levels and ratios, convert all vars to long
#     - unit of obs: locType x loc x time x var(level/change)
# 2. Generate ranks, grouped by locType x loc x time x var(level/change)
# 3. Select subperiods of interest, time from long to wide
#     - unit of obs: locType x loc x var(level/change)
# 4. Compute stats ratios over time for level and change vars
#     - 1980 to 2020 change
#     - 80 to 00, 00 to 20

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
spt_input_folder <- spt_datatemp_folder
spt_output_folder <- spt_datatemp_folder
verbose <- TRUE
verbose_detail <- TRUE
it_verbose_detail_nrow <- 100
bl_temp_save <- FALSE
bl_main_save <- TRUE
# Load in hooked loan data
spn_input <- file.path(spt_input_folder,
    "ppts_stats_flr.csv",
    fsep = .Platform$file.sep
)
df_flr <- read_csv(spn_input)

# First, from levels/ratios file (keep ranks), variables are levels and ratios, convert all vars to long. unit of obs: locType x loc x time x var(level/change).
df_ysts_longer <- df_flr %>%
    pivot_longer(
        cols = matches("var"),
        names_to = c("vartype", "variable"),
        names_pattern = paste0("var_(.*)_(.*)"),
        values_to = "stat"
    ) %>%
    drop_na(stat)
if (verbose) {
    print(glue::glue("F-376864, S1"))
    print(glue::glue("dim of youth wide ratio file: {dim(df_ysts_longer)}"))
}

# Second, generate ranks, grouped by locType x time x var(level/change). Ranking across locations.
df_ysts_longer_rank <- df_ysts_longer %>%
    group_by(location_level, variable, year_bins) %>%
    arrange(stat, ..by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    arrange(location_level, variable, year_bins, location_code) %>%
    select(-stat, -vartype) %>%
    mutate(vartype = "rank") %>%
    rename(stat = rank)
if (verbose) {
    print(glue::glue("F-376864, S2"))
    print(glue::glue(
        "dim of youth wide ratio file: {dim(df_ysts_longer_rank)}"
    ))
}


# Third, select subperiods of interest, time from long to wide: unit of obs: locType x loc x var(level/change).
ar_it_years_chg <- c(1960, 1980, 2000, 2020)
df_ysts_longer_wide <- bind_rows(df_ysts_longer, df_ysts_longer_rank) %>%
    filter(year_bins %in% ar_it_years_chg) %>%
    pivot_wider(
        id_cols = c("location_code", "location_level", "vartype", "variable"),
        names_from = year_bins,
        names_prefix = "year",
        names_sep = "_",
        values_from = c(stat)
    )
if (verbose) {
    print(glue::glue("F-376864, S3"))
    print(glue::glue(
        "dim of youth wide ratio file: {dim(df_ysts_longer_wide)}"
    ))
}

# Fourth, compute stats ratios over time for level and change vars. 1980 to 2020 change, 80 to 00, 00 to 20.
ls_chg_years <- list(
    "chg_80v60" = c(1960, 1980),
    "chg_00v80" = c(1980, 2000),
    "chg_20v00" = c(2000, 2020),
    "chg_20v80" = c(1980, 2020)
)
ar_st_names <- names(ls_chg_years)
# Initialize dataframe
df_fpc <- df_ysts_longer_wide
for (it_chg_ctr in seq(1, length(ls_chg_years))) {
    st_chg_var_name <- ar_st_names[it_chg_ctr]
    ar_it_year_bounds <- ls_chg_years[[it_chg_ctr]]
    st_var_earlier <- paste0("year", ar_it_year_bounds[1])
    st_var_later <- paste0("year", ar_it_year_bounds[2])

    df_fpc <- df_fpc %>%
        mutate(
            !!sym(st_chg_var_name) :=
                (!!sym(st_var_later) -
                    !!sym(st_var_earlier)) / !!sym(st_var_earlier)
        )

    if (verbose_detail) {
        print(glue::glue("F-376864, SD3"))
        print(st_chg_var_name)
        print(st_var_earlier)
        print(st_var_later)
        print(glue::glue("dim FPC: {dim(df_fpc)}"))
    }
}
# df_fpc <- df_ysts_longer_wide %>%
#     mutate(
#         chg_80v60 = (year1980 - year1960) / year1960,
#         chg_00v80 = (year2000 - year1980) / year1980,
#         chg_20v00 = (year2020 - year2000) / year2000,
#         chg_20v80 = (year2020 - year1980) / year1980
#     )
if (verbose) {
    print(glue::glue("F-376864, S4"))
    print(glue::glue("dim FPC: {dim(df_fpc)}"))
}

# Save
if (bl_main_save) {
    spn_path <- file.path(
        spt_output_folder, "ppts_stats_fpc.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_fpc, spn_path)
    print(glue::glue("F-376864, S5"))
    print(glue::glue("File saved: {spn_path}"))
}
