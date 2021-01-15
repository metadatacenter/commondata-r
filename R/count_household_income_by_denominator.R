#' Return the householders count per income bracket and per age group for the
#' given ZIP codes and observation year(s) period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
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
count_household_income_by_age <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_with_denominator_map <- list()
  for (age_bracket in CENSUS_MOD3_AGE_BRACKETS) {
    statvar_with_denominator_map[[age_bracket]] <- 
      sapply(CENSUS_INCOME_BRACKETS,
             function(x) paste0("Count_Household_HouseholderAge", age_bracket, "_IncomeOf", x),
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
  return (.count_household_income_by_denominator(geo_map, statvar_with_denominator_map,
                                                 start_year, end_year, year))
}

#' Return the householders count per income bracket and per race group for the
#' given ZIP codes and observation year(s) period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return  A named list with each list item is a data frame containing the 
#'    household income observations of each region per income bracket. The data
#'    frame is identified by the observation year and rage group.
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
count_household_income_by_race <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
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
  
  start_year <- if (!is.na(year)) year else start_year
  end_year <- if (!is.na(year)) year else end_year
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_with_denominator_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <- .http_post(DCAPI_STAT_ALL, body);
  
  output <- list()
  for (year in as.character(start_year:end_year)) {
    data <- list()
    for (denominator in names(statvar_with_denominator_map)) {
      statvar_map <- statvar_with_denominator_map[[denominator]]
      observation_table <- .get_observation_table(http_response, geo_map, statvar_map, year)
      provenance_table <- .get_provenance_table(http_response, geo_map, statvar_map)
      observation_table <- merge(x=observation_table, y=provenance_table, by="geoName", all.x=TRUE)
      rownames(observation_table) <- observation_table$geoName
      data[[denominator]] <- observation_table
    }
    output[[year]] <- data
  }
  return (output)
}