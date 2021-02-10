#' Return the householder count per income bracket and per age group for the
#' given the geographical names (i.e., zip codes, state codes, state names, 
#' county names) and theobservation year period.
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
#'    household income observations of each region per income bracket. The data
#'    frame is identified by the observation year and age group.
#'
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the householders per income and age group in the specified ZIP codes
#' count_household_income_by_age(zips)
#' 
#' # Count the householders from 2012 to 2015
#' count_household_income_by_age(zips, start_year=2012, end_year=2015)
#' 
#' # Count the householders in 2012
#' count_household_income_by_age(zips, year=2012)
#' 
#' # Count the householders in the state of California
#' count_household_income_by_age(c("California")) # State name
#' count_household_income_by_age(c("CA")) # State code
#' count_household_income_by_age(c("06")) # 2-digit state FIPS code
count_household_income_by_age <- function(geo_names,
                                          location_type=c(NA, "zip", "county", "state"),
                                          start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_with_denominator_map <- list()
  for (age_bracket in CENSUS_B19049_AGE_GROUPS) {
    statvar_with_denominator_map[[age_bracket]] <- 
      sapply(CENSUS_INCOME_BRACKETS,
             function(x) paste0("Count_Household_HouseholderAge", age_bracket, "_IncomeOf", x),
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
  return (.count_household_income_by_denominator(geo_map, statvar_with_denominator_map,
                                                 start_year, end_year, year))
}

#' Return the householder count per income bracket and per race group for the 
#' given the geographical names (i.e., zip codes, state codes, state names, 
#' county names) and the observation year period.
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
#'    household income observations of each region per income bracket. The data
#'    frame is identified by the observation year and race group.
#'
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the householders per income and race group in the specified ZIP codes
#' count_household_income_by_race(zips)
#' 
#' # Count the householders from 2012 to 2015
#' count_household_income_by_race(zips, start_year=2012, end_year=2015)
#' 
#' # Count the householders in 2012
#' count_household_income_by_race(zips, year=2012)
#' 
#' # Count the householders in the state of California
#' count_household_income_by_race(c("California")) # State name
#' count_household_income_by_race(c("CA")) # State code
#' count_household_income_by_race(c("06")) # 2-digit state FIPS code
count_household_income_by_race <- function(geo_names,
                                           location_type=c(NA, "zip", "county", "state"),
                                           start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_with_denominator_map <- list()
  for (race_category in CENSUS_RACE_CATEGORIES) {
    statvar_with_denominator_map[[race_category]] <- 
      sapply(CENSUS_INCOME_BRACKETS,
             function(x) paste0("Count_Household_HouseholderRace", race_category, "_IncomeOf", x),
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
  return (.count_household_income_by_denominator(geo_map, statvar_with_denominator_map,
                                                 start_year, end_year, year))
}

.count_household_income_by_denominator <- function(geo_map, statvar_with_denominator_map,
                                                   start_year, end_year, year) {
  
  return (.get_statistical_data(geo_map, statvar_with_denominator_map,
                                start_year, end_year, year))
}