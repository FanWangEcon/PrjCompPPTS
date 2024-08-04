#' Linearly interpolate missing data in-between dates with observed data
#'
#' @description A file, `df_data`, has grouping variables `ar_svr_group`
#' and a data variable `svr_data` (e.g., population), measured at sequential
#' `svr_date` (e.g., year, month) dates. There are nonconsecutive gaps in `svr_data`
#' measurements, with missing values in some dates. We use the rate of change
#' between the closest dates in which there is data to linearly interpolate
#' `svr_data` in dates with missing values. The output is stored in variable
#' `svr_interp`.
#'
#' @param df_data data frame with various time-series/panel with missing data
#' @param ar_svr_group array of grouping variable names, panel groups
#' @param svr_data string variable name for data variable
#' @param svr_date string variable name for date variable
#' @param svr_interp string variable name for variable with interpolated values
#' @param verbose Boolean print details
#'
#' @return A dataset with interpolated values
#' @import dplyr tidyr
#' @export
#' @references
#' \href{https://github.com/FanWangEcon/PrjCompPPTS/issues/16}{PrjThaiHFID-issue-16}
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ff_ppts_interp_linear <- function(df_data,
                             ar_svr_group = c("location", "variable"),
                             svr_data = c("population"),
                             svr_date = c("year"),
                             svr_interp = c("population_interp1"),
                             verbose = FALSE) {
  # 1. Compute percentage annual changes between dates (such as years) with gaps
  # compute linearly interpolated annual change create new sorting var, to take difference across dates (such as dates) even if not consecutive
  # date_withgap_ctr: row_number() is new sorting variable, +1 regardless of date (such as year) gap
  # pchg_span_interp1: compute percentage change over span and dates (such as years) gap over span
  # pchg_yr1_interp1: linear-interpolated annualized (with compounding) percentage change
  df_data_pchg <- df_data %>%
    drop_na(!!sym(svr_data)) %>%
    mutate(date_with_gap_ctr = row_number()) %>%
    arrange(!!!syms(ar_svr_group), date_with_gap_ctr) %>%
    group_by(!!!syms(ar_svr_group)) %>%
    mutate(
      pchg_span_interp1 = (!!sym(svr_data) - lag(!!sym(svr_data))) / lag(!!sym(svr_data)),
      span_date = (!!sym(svr_date) - lag(!!sym(svr_date)))
    ) %>%
    mutate(pchg_yr1_interp1 = abs(1 + pchg_span_interp1) ^ (1 / span_date) - 1) %>%
    select(
      !!!syms(ar_svr_group),!!sym(svr_date),
      span_date,
      !!sym(svr_data),
      pchg_yr1_interp1
    )

  # 2. Expand frame to have a rwo for each consecutive date (such as year)
  # the pchg_yr1_interp1 variable is only shown at start date (such as year),
  # due to drop_na(value) earlier
  # need to expand to all dates (such as years)
  df_data_expand <- df_data_pchg %>%
    ungroup() %>% mutate(span_date_dup = span_date) %>%
    drop_na(span_date) %>%
    tidyr::uncount(span_date) %>%
    group_by(!!!syms(ar_svr_group),!!sym(svr_date)) %>%
    mutate(date_adj = row_number() +!!sym(svr_date) - span_date_dup,
           gap_ctr = row_number()) %>%
    ungroup() %>%
    select(
      !!!syms(ar_svr_group),
      date_adj,
      !!sym(svr_data),
      pchg_yr1_interp1,
      gap_ctr,
      span_date_dup
    ) %>%
    rename(!!sym(svr_date) := date_adj)

  # 3. Generate data in missing dates (such as years)
  # Conditions A and B are both gap_ctr == span_date_dup
  # value_interp1 L1: A. value is correct, originally not NA
  # value_interp1 L2: B. value is also correct, next available date (such as year) with non NA
  # value_interp1 L3: C. in-between dates (such as years), where values were NA
  df_interp1 <- df_data_expand %>%
    arrange(!!!syms(ar_svr_group),!!sym(svr_date)) %>%
    group_by(!!!syms(ar_svr_group)) %>%
    mutate(
      value_interp1 = case_when(
        gap_ctr == 1 & span_date_dup == 1 ~ !!sym(svr_data),
        gap_ctr == span_date_dup &
          span_date_dup > 1 ~ !!sym(svr_data),
        gap_ctr < span_date_dup ~ (!!sym(svr_data) / ((1 + pchg_yr1_interp1) ^ (span_date_dup - gap_ctr)
        ))
      )
    ) %>%
    ungroup() %>%
    select(-gap_ctr,-span_date_dup,-!!sym(svr_data))

  # 4. Merge interpolated and actual data as separate columns.
  ls_svr_group_merge <- vector(mode = "character", length = 0)
  for (svr_group in ar_svr_group) {
    ls_svr_group_merge[[svr_group]] <- svr_group
  }
  ls_svr_group_merge[[svr_date]] <- svr_date
  df_data_interp1 <- df_data %>%
    left_join(df_interp1, by = (ls_svr_group_merge)) %>%
    rename(!!sym(svr_interp) := value_interp1) %>%
    mutate(!!sym(svr_interp) := if_else(
      !is.na(!!sym(svr_data)) &
        is.na(!!sym(svr_interp)),
      !!sym(svr_data),
      !!sym(svr_interp)
    ))

  # Sort 
  df_data_interp1 <- df_data_interp1 %>% 
    arrange(!!!syms(ar_svr_group), !!sym(svr_date))

  # return
  return(df_data_interp1)
}

#' Compute per-period (e.g., annualized) percentage changes
#'
#' @description A file, `df_data`, has grouping variables `ar_svr_group`
#' and a data variable `svr_data` (e.g., population), measured at sequential
#' `svr_date` (e.g., year, month) dates. There might be non-consecutive gaps in 
#' `svr_data` measurements, with missing values in some dates. We compute per-period
#' changes, annualized if the date variable is yearly, between all dates when data
#' is available. 
#'
#' @param df_data data frame with various time-series/panel with missing data
#' @param ar_svr_group array of grouping variable names, panel groups
#' @param svr_data string variable name for data variable
#' @param svr_date string variable name for date variable
#' @param svr_pchg string variable name for variable with storing the per-period
#' percentage change (e.g., annualized changes)
#' @param verbose Boolean print details
#'
#' @return A dataset with interpolated values
#' @import dplyr tidyr
#' @export
#' @references
#' \href{https://github.com/FanWangEcon/PrjCompPPTS/issues/17}{PrjThaiHFID-issue-17}
#' @author Fan Wang, \url{http://fanwangecon.github.io}
#'
ff_ppts_pchg <- function(df_data,
                             ar_svr_group = c("location", "variable"),
                             svr_data = c("population"),
                             svr_date = c("year"),
                             svr_pchg = c("population_pchg_yr"),
                             verbose = FALSE) {

  # Compute percentage annual changes between dates (such as years) with gaps
  # compute linearly interpolated annual change create new sorting var, to take difference across dates (such as dates) even if not consecutive

  # 1. date_withgap_ctr: row_number() is new sorting variable, +1 regardless of date (such as year) gap
  df_data_clean <- df_data %>%
    drop_na(!!sym(svr_data)) %>%
    mutate(date_with_gap_ctr = row_number())

  # 2. pchg_span_interp1: compute percentage change over span and dates (such as years) gap over span
  df_data_chg <- df_data_clean %>%
    arrange(!!!syms(ar_svr_group), date_with_gap_ctr) %>%
    group_by(!!!syms(ar_svr_group)) %>%
    mutate(
      pchg_span_interp1 = (!!sym(svr_data) - lag(!!sym(svr_data))) / lag(!!sym(svr_data)),
      span_date = (!!sym(svr_date) - lag(!!sym(svr_date)))
    )

  # 3. svr_pchg: linear-interpolated annualized (with compounding) percentage change
  df_data_pchg <- df_data_chg %>%
    mutate(!!sym(svr_pchg) := abs(1 + pchg_span_interp1) ^ (1 / span_date) - 1) %>%
    select(
      !!!syms(ar_svr_group),!!sym(svr_date),
      span_date,
      !!sym(svr_data),
      !!sym(svr_pchg)
    )

  # Sort 
  df_data_pchg <- df_data_pchg %>% 
    arrange(!!!syms(ar_svr_group), !!sym(svr_date))

  # return
  return(df_data_pchg)
}