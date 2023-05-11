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
