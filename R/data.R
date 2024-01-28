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
#' For Korea, have with-in country data aggregated to compare between
#' metropolitan and non-metropolitan areas. Also has data available in
#' Korea for all regions
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
#' @source \href{Cross-nationa data on school age popultion, students, teachers, and schools}{https://fanwangecon.github.io/}
#' @examples
#' data(ppts_easia_weuro_world)
#' print(ppts_easia_weuro_world)
"ppts_easia_weuro_world"

#' Level and changes, interpolated and extraplated
#'
#' @format A data frame with 266 rows and 7 variables:
#' \describe{
#'   \item{location_code}{Short string code for country and regions}
#'   \item{location_level}{Country, or regional or sub-country}
#'   \item{variable}{Statistical variable}
#'   \item{year_bins_type}{The type of year binning}
#'   \item{year_bins}{Year bins given year binning}
#'   \item{value}{Level at start to end of bin}
#'   \item{value_interp1}{`value` with interpolation and extrapolation}
#'   \item{pchg}{Percentage change from start to end of bin}
#'   \item{pchg_interp1}{`pchg` with interpolation and extrapolation}
#' }
#' @source World bank, national statistics websites of W. Europe and E. Asia countries
#' @examples
#' data(ppts_easia_weuro_world_pchg)
#' print(ppts_easia_weuro_world_pchg)
"ppts_easia_weuro_world_pchg"


#' Country, region and subcountry code and names
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
#' print(ppts_country_code)
"ppts_country_code"
