#' Generate FLR file
#'
#' @description Generate ratios
#'
#' @param df_ppts Dataframe input.
#' @param st_year_bins_type String category string
#' @param ar_it_years Array of Integers subset of year bins to select
#' @param ar_st_vars Array of Strings variables to be selected from `df_ppts`,
#' all variables used in string lists in `ls_rat_vars` need to be included.
#' @param ls_rat_vars List of Array each array with numerator and denominator
#' variables for ratios to be computed list contains new ratio name suffix
#' @param verbose Boolean print details
#'
#' @return A dataset with ratios and levels.
#' @import dplyr tidyr
#' @export
#' @references
#' \href{https://github.com/FanWangEcon/PrjCompPPTS/issues/7}{PrjThaiHFID-issue-7, Step 1}
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ff_ppts_lrce_flr <- function(
    df_ppts,
    st_year_bins_type = "1940t2020i01",
    ar_it_years = c(1960, 1980, 1990, 2000, 2010, 2020),
    ar_st_vars = c("youthpop", "student", "teacher", "school"),
    ls_rat_vars = list(
        "y2t" = c("youthpop", "teacher"),
        "s2t" = c("student", "teacher"),
        "y2s" = c("youthpop", "school"),
        "s2s" = c("student", "school")
    ),
    verbose = FALSE) {
    # First, start with long file, keep all locations and location types, select a subst of years, select `teachers`, `schools`, `students`, `youthpop` so different ratios can be constructed, and levels we want kept
    df_ysts <- df_ppts %>%
        filter(variable %in% ar_st_vars) %>%
        # filter(year_bins_type == "1940t2020i01") %>%
        filter(year_bins_type == st_year_bins_type) %>%
        mutate(year_bins = as.numeric(year_bins)) %>%
        # filter(year_bins %in% c(1960, 1980, 1990, 2000, 2010, 2020))
        filter(year_bins %in% ar_it_years)
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
    bl_ratio_hardcode <- FALSE
    # - Teacher over student: `var_rat_t2s`
    # - Teacher over youthpop: `var_rat_t2y`
    # - School over student: `var_rat_s2s`
    # - School over youthpop: `var_rat_sty`
    if (bl_ratio_hardcode) {
        df_flr <- df_ysts_wide %>%
            mutate(
                var_rat_y2t = var_lvl_youthpop / var_lvl_teacher,
                var_rat_s2t = var_lvl_student / var_lvl_teacher,
                var_rat_y2s = var_lvl_youthpop / var_lvl_school,
                var_rat_s2s = var_lvl_student / var_lvl_school
            )
    } else {
        # Fourth, compute stats ratios over time for level and change vars. 1980 to 2020 change, 80 to 00, 00 to 20.
        ar_st_names <- names(ls_rat_vars)
        # Initialize dataframe
        df_flr <- df_ysts_wide
        for (it_rat_ctr in seq(1, length(ls_rat_vars))) {
            st_rat_var_name <- paste0("var_rat_", ar_st_names[it_rat_ctr])
            ar_it_vars_ratio <- ls_rat_vars[[it_rat_ctr]]
            st_var_numerator <- paste0("var_lvl_", ar_it_vars_ratio[1])
            st_var_denominator <- paste0("var_lvl_", ar_it_vars_ratio[2])

            df_flr <- df_flr %>%
                mutate(
                    !!sym(st_rat_var_name) := !!sym(st_var_numerator) / !!sym(st_var_denominator)
                )

            if (verbose) {
                print(glue::glue("F-713479, S2"))
                print(st_rat_var_name)
                print(st_var_numerator)
                print(st_var_denominator)
                print(glue::glue("dim FLR: {dim(df_flr)}"))
            }
        }
    }
    if (verbose) {
        print(glue::glue("F-713479, S3"))
        print(colnames(df_flr))
        print(glue::glue("dim of youth wide ratio file: {dim(df_flr)}"))
    }

    return(df_flr)
}


#' Generate FPC file
#'
#' @description Compute percentages changes across time
#'
#' @param df_flr Dataframe input, output of `PrjCompPPTS::ff_ppts_lrce_flr`
#' @param ar_it_years_chg Array of integer, years to select for computing changes
#' @param ls_chg_year List of Array each array with start and end date and list contains
#' new variable names
#' @param verbose Boolean print details
#'
#' @return A dataset with ratios, levels, and percentage changes.
#' @import dplyr tidyr
#' @export
#' @references
#' \href{https://github.com/FanWangEcon/PrjCompPPTS/issues/7}{PrjThaiHFID-issue-7, Step 2}
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ff_ppts_lrce_fpc <- function(
    df_flr,
    ar_it_years_chg = c(1960, 1980, 2000, 2020),
    ls_chg_years = list(
        "chg_80v60" = c(1960, 1980),
        "chg_00v80" = c(1980, 2000),
        "chg_20v00" = c(2000, 2020),
        "chg_20v80" = c(1980, 2020)
    ),
    verbose = FALSE) {
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
    # ar_it_years_chg <- c(1960, 1980, 2000, 2020)
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

        if (verbose) {
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

    return(df_fpc)
}


#' Generate FEL file
#'
#' @description Generate elasticities
#'
#' @param df_fpc Dataframe input, output of `PrjCompPPTS::ff_ppts_lrce_fpc`
#' \href{https://github.com/FanWangEcon/PrjThaiHFID/issues/1}{PrjThaiHFID-issue-1}.
#' 
#' @param verbose Boolean print details
#'
#' @return A dataset with ratios, levels, percentage changes, and elasticities.
#' @import dplyr tidyr
#' @export
#' @references
#' \href{https://github.com/FanWangEcon/PrjCompPPTS/issues/7}{PrjThaiHFID-issue-7, Step 3}
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ff_ppts_lrce_fel <- function(
    df_fpc,
    ls_rat_vars = list(
        "y2t" = c("youthpop", "teacher"),
        "s2t" = c("student", "teacher"),
        "y2s" = c("youthpop", "school"),
        "s2s" = c("student", "school")
    ),
    verbose = FALSE) {
    # First, from `fpc`, keep percentage changes as stats vars, drop level and rank vars.
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
    bl_ratio_hardcode <- FALSE
    if (bl_ratio_hardcode) {
        df_fel_elas <- df_fel_wide %>%
            mutate(
                var_elas_y2t = var_chg_teacher / var_chg_youthpop,
                var_elas_s2t = var_chg_teacher / var_chg_student,
                var_elas_y2s = var_chg_school / var_chg_youthpop,
                var_elas_s2s = var_chg_school / var_chg_student
            )
    } else {
        # Fourth, compute stats ratios over time for level and change vars. 1980 to 2020 change, 80 to 00, 00 to 20.
        ar_st_names <- names(ls_rat_vars)
        # Initialize dataframe
        df_fel_elas <- df_fel_wide
        for (it_rat_ctr in seq(1, length(ls_rat_vars))) {
            st_rat_var_name <- paste0("var_elas_", ar_st_names[it_rat_ctr])
            ar_it_vars_ratio <- ls_rat_vars[[it_rat_ctr]]
            st_var_numerator <- paste0("var_chg_", ar_it_vars_ratio[1])
            st_var_denominator <- paste0("var_chg_", ar_it_vars_ratio[2])

            df_fel_elas <- df_fel_elas %>%
                mutate(
                    !!sym(st_rat_var_name) := !!sym(st_var_numerator) / !!sym(st_var_denominator)
                )

            if (verbose) {
                print(glue::glue("F-307302, S3"))
                print(st_rat_var_name)
                print(st_var_numerator)
                print(st_var_denominator)
                print(glue::glue("dim FEL: {dim(df_fel_elas)}"))
            }
        }
    }
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

    return(df_fel)
}
