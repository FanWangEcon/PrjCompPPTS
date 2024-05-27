#' Country, region, and subcountry code and names
#'
#' @format A data frame with 266 rows and 7 variables:
#' \describe{
#'   \item{location_name}{Location full name}
#'   \item{location_code}{Short string code for country and regions}
#'   \item{location_region_group}{World bank seven regions}
#'   \item{location_income_group}{World bank country income groups}
#'   \item{location_wblend_group}{World bank lending category}
#'   \item{location_region_group_code}{3 Let. WB 7 region + digit sorter}
#'   \item{location_code_adj}{Allow for region then country by region sort}
#' }
#' @source World bank
#' @examples
#' data(ppts_country_code)
#' summary(ppts_country_code)
#' print(ppts_country_code)
"ppts_country_code"

#' Panel data on school age population, students, teachers, and schools
#'
#' Annual data available for different time frames across countries.
#'  
#' Data on school age population and teachers available globally.
#' Data on school age population, teachers and schools available 
#' for Western European and Eastern Asian countries. 
#' 
#' Aggregate national for countries except in the case of Korea. 
#' 
#' For Korea, within country data aggregated to compare between
#' metropolitan and non-metropolitan areas as well as province-specific data.
#'
#' @format A data frame with 9 rows and 5 variables:
#' \describe{
#'   \item{Country}{Country name}
#'   \item{countrycode}{Three letter country code}
#'   \item{countrycode}{Values equal to nationwide, urban and non-urban for Korea}
#'   \item{stats_youthpop}{Youth population age 0 to 15}
#'   \item{stats_student}{Number of students in primary school}
#'   \item{stats_teacher}{Number of teachers in primary school}
#'   \item{stats_school}{Number of primary schools}
#'   \item{stats_gdp}{Annual GDP}
#'   \item{stats_enroll_ratio}{Primary school enrollment ratio}
#' }
#' @source \href{https://jeonghyeok-kim.github.io/assets/GlobalChildrenTeachersSchools_HannumKimWang.pdf}{See online appendix B of Hannum, Kim, and Wang (2024).}
#' @examples
#' data(ppts_easia_weuro_world)
#' summary(ppts_easia_weuro_world)
#' print(ppts_easia_weuro_world)
"ppts_easia_weuro_world"

#' Interpolated and extrapolated levels and changes panel across time bins
#' 
#' Panel data on school age population, students, teachers, and schools, levels and changes, interpolated and extrapolated
#'
#' @format A data frame with 266 rows and 7 variables:
#' \describe{
#'   \item{location_code}{Short string code for country and regions}
#'   \item{location_level}{Country, or regional or sub-country}
#'   \item{variable}{Statistical variable}
#'   \item{year_bins_type}{The type of year binning, 1 year, 10 years, 20 years, etc.}
#'   \item{year_bins}{Year bins given year binning, labels of different years, decades, bi-decades, etc.}
#'   \item{value}{Level at start to end of bin}
#'   \item{value_interp1}{`value` with interpolation and extrapolation}
#'   \item{pchg}{Percentage change from start to end of bin}
#'   \item{pchg_interp1}{`pchg` with interpolation and extrapolation}
#' }
#' @source \href{https://fanwangecon.github.io/PrjCompPPTS/articles/ffv_gen_percent_changes.html}{articles/ffv_gen_percent_changes}, \href{https://jeonghyeok-kim.github.io/assets/GlobalChildrenTeachersSchools_HannumKimWang.pdf}{See online appendix C of Hannum, Kim, and Wang (2024).}
#' @examples
#' data(ppts_easia_weuro_world_pchg)
#' summary(ppts_easia_weuro_world_pchg)
#' print(ppts_easia_weuro_world_pchg)
"ppts_easia_weuro_world_pchg"

#' Elasticities across all potential variable pairs and for different time frames, non-interpolated
#' 
#' Compute elasticities for different numerator and denominator combinations, using non-interpolated data.
#'
#' @format A data frame:
#' \describe{
#'   \item{location_code}{Short string code for country and regions}
#'   \item{location_level}{Country, or regional or sub-country}
#'   \item{variable}{The variable associated with `value`, also the variable, whose percentage change is computed, in the denominator}
#'   \item{year_bins_type}{The type of year binning, 1 year, 10 years, 20 years, etc.}
#'   \item{year_bins}{Year bins given year binning, labels of different years, decades, bi-decades, etc.}
#'   \item{pchg}{Percentage change from start to end of bin associated with variable (for the denominator)}
#'   \item{value}{Level values for variables}
#'   \item{variable_numerator}{The variable, whose percentage change is computed, in the numerator}
#'   \item{pchg_numerator}{`pchg` but for the `variable_numerator` variable}
#'   \item{elasticity}{`pchg_numerator` over `pchg` to compute elasticities over different windows for all possible variable `variable_numerator` and `variable` combinations.}
#' }
#' @source World bank, national statistics websites of W. Europe and E. Asia countries
#' @examples
#' data(ppts_easia_weuro_world_elas_raw)
#' summary(ppts_easia_weuro_world_elas_raw)
#' print(ppts_easia_weuro_world_elas_raw)
"ppts_easia_weuro_world_elas_raw" 

#' Elasticities across all potential variable pairs and for different time frames, non-interpolated
#' 
#' Compute elasticities for different numerator and denominator combinations, using non-interpolated data.
#' 
#' @format A data frame:
#' \describe{
#'   \item{location_code}{Short string code for country and regions}
#'   \item{location_level}{Country, or regional or sub-country}
#'   \item{variable}{Statistical variable}
#'   \item{year_bins_type}{The type of year binning}
#'   \item{year_bins}{Year bins given year binning}
#'   \item{pchg_interp1}{Percentage change from start to end of bin associated with variable (for the denominator), with interpolation and extrapolation}
#'   \item{value_interp1}{Level values for variables, with interpolation and extrapolation}
#'   \item{variable_numerator}{The variable, whose percentage change is computed, in the numerator}
#'   \item{pchg_interp1_numerator}{`pchg` but for the `variable_numerator` variable, with interpolation and extrapolation}
#'   \item{elasticity_interp1}{`pchg_interp1_numerator` over `pchg_interp1` to compute elasticities over different windows for all possible variable `variable_numerator` and `variable` combinations.}
#' }
#' @source World bank, national statistics websites of W. Europe and E. Asia countries
#' @examples
#' data(ppts_easia_weuro_world_elas_interp1)
#' summary(ppts_easia_weuro_world_elas_interp1)
#' print(ppts_easia_weuro_world_elas_interp1)
"ppts_easia_weuro_world_elas_interp1"