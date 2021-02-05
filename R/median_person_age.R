#' Return the median age of the population for the given the geographical
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
#'    median age of the population of each region. The data frame is identified 
#'    by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Get the median age in the specified ZIP codes
#' median_person_income(zips)
#' 
#' # Get the median age from 2012 to 2015
#' median_person_income(zips, start_year=2012, end_year=2015)
#' 
#' # Get the median age in 2012
#' median_person_income(zips, year=2012)
#' 
#' # Get the median age in California state
#' median_person_income(c("California")) # State name
#' median_person_income(c("CA")) # State code
#' median_person_income(c("06")) # 2-digit state FIPS code
median_person_age <- function(geo_names,
                              location_type=c(NA, "zip", "county", "state"),
                              start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list(age = "Median_Age_Person")
  
  return (.median_person_age(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the median age of male population for the given the geographical
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
#'    median age of male population of each region. The data frame is identified 
#'    by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Get the median male age in the specified ZIP codes
#' median_male_person_age(zips)
#' 
#' # Get the median male age from 2012 to 2015
#' median_male_person_age(zips, start_year=2012, end_year=2015)
#' 
#' # Get the median male age in 2012
#' median_male_person_age(zips, year=2012)
#' 
#' # Get the median male age in California state
#' median_male_person_age(c("California")) # State name
#' median_male_person_age(c("CA")) # State code
#' median_male_person_age(c("06")) # 2-digit state FIPS code
median_male_person_age <- function(geo_names,
                                   location_type=c(NA, "zip", "county", "state"),
                                   start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list(age = "Median_Age_Person_Male")
  
  return (.median_person_age(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the median age of female population for the given the geographical
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
#'    median age of female population of each region. The data frame is identified 
#'    by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Get the median female age in the specified ZIP codes
#' median_female_person_age(zips)
#' 
#' # Get the median female age from 2012 to 2015
#' median_female_person_age(zips, start_year=2012, end_year=2015)
#' 
#' # Get the median female age in 2012
#' median_female_person_age(zips, year=2012)
#' 
#' # Get the median female age in California state
#' median_female_person_age(c("California")) # State name
#' median_female_person_age(c("CA")) # State code
#' median_female_person_age(c("06")) # 2-digit state FIPS code
median_female_person_age <- function(geo_names,
                                     location_type=c(NA, "zip", "county", "state"),
                                     start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list(age = "Median_Age_Person_Female")
  
  return (.median_person_age(geo_map, statvar_map, start_year, end_year, year))
}

.median_person_age <- function(geo_map, statvar_map, start_year, end_year, year) {
  
  return (.get_statistical_data(geo_map, statvar_map, start_year, end_year, year))
}