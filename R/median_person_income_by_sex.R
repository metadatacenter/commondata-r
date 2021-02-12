#' Return the median person income per age group for the given the geographical
#' names (i.e., zip codes, state codes, state names, county names) and the 
#' observation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names.
#' @param location_type optional, string indicating the location type of the
#'    geographical names. NA by default.
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return  A named list with each list item is a data frame containing the 
#'    median income of each region per age group. The data frame is identified 
#'    by the observation year and the sex category.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the median person income in the specified ZIP codes
#' median_person_income_by_sex(zips)
#' 
#' # Count the median person income from 2012 to 2015
#' median_person_income_by_sex(zips, start_year=2012, end_year=2015)
#' 
#' # Count the median person income in 2012
#' median_person_income_by_sex(zips, year=2012)
#' 
#' # Count the median person income in California state
#' median_person_income_by_sex(c("California")) # State name
#' median_person_income_by_sex(c("CA")) # State code
#' median_person_income_by_sex(c("06")) # 2-digit state FIPS code
median_person_income_by_sex <- function(geo_names,
                                 location_type=c(NA, "zip", "county", "state"),
                                 start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list()
  for (sex_category in CENSUS_SEX_CATEGORIES) {
    statvar_map[[sex_category]] <- 
      sapply(CENSUS_PERSON_WITH_INCOME_AGE_GROUPS, 
             function(x) paste0("Median_Income_Person_", x, "_", sex_category, "_WithIncome"), 
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
  return (.get_statistical_data(geo_map, statvar_map, start_year, end_year, year))
}