# We have the LRCE file, level, ratio, change, and elasticity.
# Step 1 of https://github.com/FanWangEcon/PrjCompPPTS/issues/7
# `FEL`

# 1. From `fpc`, keep percentage changes as stats vars, drop level and rank vars
# 2. From percentage change file, variables are changes over time, convert all change spans to long
#     - unit of obs: locType x loc x vars(level/change) x changeSpan
# 3. Convert vars(level/change) to wide
#     - unit of obs: locType x loc x changeSpan
# 4. ELASTICITY: ratio of percentage changes across vars
# 5. Convert to standard format
#     - unit of obs: locType x loc x vars, and year spans as columns

library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(PrjCompPPTS)

# Note we also generate elasticity information on [Compute Elasticities of School Resources to Changes in Populations](https://fanwangecon.github.io/PrjCompPPTS/articles/ffv_gen_elasticities.html). It is done more automatically there, across all possible combinations of variables, here, we generate a more selected subset of elasticities, note that this is manual because we specify below what the numerator and denominator for each elasticity is.

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
    "ppts_stats_fpc.csv",
    fsep = .Platform$file.sep
)
df_fpc <- read_csv(spn_input)

# First, from `fpc`, keep percentage changes as stats vars, drop level and rank vars.
print(df_fpc %>% distinct(vartype))
df_fpc_base <- df_fpc %>%
    filter(vartype == "lvl") %>%
    select(-contains("year"), -vartype)
if (verbose) {
    print(glue::glue("F-307302, S1"))
    print(glue::glue("dim df_fpc_base: {dim(df_fpc_base)}"))
}

# Second, from percentage change file, variables are changes over time, convert all change spans to long. unit of obs: locType x loc x vars(change) x changeSpan.
df_fel_long <- df_fpc_base %>%
    pivot_longer(
        cols = matches("chg"),
        names_to = c("yearcomp"),
        names_pattern = paste0("chg_(.*)"),
        values_to = "change"
    ) %>%
    drop_na(change)
if (verbose) {
    print(glue::glue("F-307302, S2"))
    print(glue::glue("dim df_fel_long: {dim(df_fel_long)}"))
}

# Third, convert vars(change) to wide. unit of obs: locType x loc x changeSpan.
df_fel_wide <- df_fel_long %>%
    pivot_wider(
        id_cols = c("location_code", "location_level", "yearcomp"),
        names_from = variable,
        names_prefix = "var_chg_",
        names_sep = "_",
        values_from = c(change)
    )
if (verbose) {
    print(glue::glue("F-307302, S3"))
    print(glue::glue("dim df_fel_wide: {dim(df_fel_wide)}"))
}

# Fourth, following our prior notations, we compute the percentage changes in education resources over percentage changes in the population. When changes in teachers is in the denominator, we call this the population-teacher elasticity. When the number of schools is in the denominator, we call this this population-school elasticity. In labor economics, the labor supply elasticity refers to the change in labor supply given a change in wages, which could also be referred to as the wage-labor-supply elasticity. We follow this form of terminology here.
#
# We use the following variable names for the following elasticities:
# - Teacher over student : `var_elsa_t2s`
# - Teacher over youthpop: `var_elas_t2y`
# - School over student: `var_elas_s2s`
# - School over youthpop: `var_elas_sty`
df_fel_elas <- df_fel_wide %>%
    mutate(
        var_elas_y2t = var_chg_teacher / var_chg_youthpop,
        var_elas_s2t = var_chg_teacher / var_chg_student,
        var_elas_y2s = var_chg_school / var_chg_youthpop,
        var_elas_s2s = var_chg_school / var_chg_student
    )
if (verbose) {
    print(glue::glue("F-307302, S4"))
    print(glue::glue("dim df_fel_elas: {dim(df_fel_elas)}"))
}

# Fifth, we clean file and convert to standard format where unit of obs is locType x loc x vars, and year spans are columns.
df_fel <- df_fel_elas %>%
    select(-contains("var_chg")) %>%
    pivot_longer(
        cols = matches("var_elas"),
        names_to = c("variable"),
        names_pattern = paste0("var_elas_(.*)"),
        values_to = "elasticity"
    ) %>%
    drop_na(elasticity) %>%
    pivot_wider(
        id_cols = c("location_code", "location_level", "variable"),
        names_from = yearcomp,
        names_prefix = "elas_",
        names_sep = "_",
        values_from = c(elasticity)
    )
if (verbose) {
    print(glue::glue("F-307302, S4"))
    print(glue::glue("dim FEL: {dim(df_fel)}"))
}

# Save
if (bl_main_save) {
    spn_path <- file.path(
        spt_output_folder, "ppts_stats_fel.csv",
        fsep = .Platform$file.sep
    )
    write_csv(df_fel, spn_path)
    print(glue::glue("F-307302, S5"))
    print(glue::glue("File saved: {spn_path}"))
}