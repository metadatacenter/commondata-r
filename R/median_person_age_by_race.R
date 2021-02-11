#' Return the median age of the population of different race groups for the 
#' given the geographical names (i.e., zip codes, state codes, state names, 
#' county names, or school district) and the observation year period.
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
#'    median age observations in each region. The data frame is identified by 
#'    the observation year and race group.
#'
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Get the median age per race group in the specified ZIP codes
#' count_household_income_by_race(zips)
#' 
#' # Get the median age per race group from 2012 to 2015
#' count_household_income_by_race(zips, start_year=2012, end_year=2015)
#' 
#' # Get the median age per race group in 2012
#' count_household_income_by_race(zips, year=2012)
#' 
#' # Get the median age per race group in the state of California
#' count_household_income_by_race(c("California")) # State name
#' count_household_income_by_race(c("CA")) # State code
#' count_household_income_by_race(c("06")) # 2-digit state FIPS code
median_person_age_by_race <- function(geo_names,
                                location_type=c(NA, "zip", "county", "state", "school"),
                                start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list()
  for (race_group in CENSUS_RACE_GROUPS) {
    statvar <- paste0("Median_Age_Person_", race_group)
    statvar_map[[race_group]] <- list(age = statvar)
  }
  
  return (.get_statistical_data(geo_map, statvar_map, start_year, end_year, year))
}