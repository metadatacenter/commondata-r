#' Return the median person income per age group for the given the geographical
#' names (i.e., zip codes, state codes, state names, county names) and the 
#' observation year period.
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
#'    median income of each region per age group. The data frame is identified 
#'    by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the median person income in the specified ZIP codes
#' median_person_income(zips)
#' 
#' # Count the median person income from 2012 to 2015
#' median_person_income(zips, start_year=2012, end_year=2015)
#' 
#' # Count the median person income in 2012
#' median_person_income(zips, year=2012)
#' 
#' # Count the median person income in California state
#' median_person_income(c("California")) # State name
#' median_person_income(c("CA")) # State code
#' median_person_income(c("06")) # 2-digit state FIPS code
median_person_income <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_MOD4_AGE_BRACKETS, 
                        function(x) {
                          if (x == "15OrMoreYears")
                            paste0("Median_Income_Person") else
                            paste0("Median_Income_Person_", x)
                        }, 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.median_person_income(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the median person income per age group for male population for the 
#' given the geographical names (i.e., zip codes, state codes, state names,
#' county names) and the observation year period.
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
#'    median income for male population of each region per age group. The data 
#'    frame is identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the median person income for male population in the specified ZIP codes
#' median_male_person_income(zips)
#' 
#' # Count the median person income for male population from 2012 to 2015
#' median_male_person_income(zips, start_year=2012, end_year=2015)
#' 
#' # Count the median person income for male population in 2012
#' median_male_person_income(zips, year=2012)
#' 
#' # Count the median person income for male population in California state
#' median_male_person_income(c("California")) # State name
#' median_male_person_income(c("CA")) # State code
#' median_male_person_income(c("06")) # 2-digit state FIPS code
median_male_person_income <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_MOD4_AGE_BRACKETS, 
                        function(x) paste0("Median_Income_Person_", x, "_Male_WithIncome"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.median_person_income(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the median person income per age group for female population for the 
#' given the geographical names (i.e., zip codes, state codes, state names, 
#' county names) and the observation year period.
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
#'    median income for female population of each region per age group. The data 
#'    frame is identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the median person income for female population in the specified ZIP codes
#' median_female_person_income(zips)
#' 
#' # Count the median person income for female population from 2012 to 2015
#' median_female_person_income(zips, start_year=2012, end_year=2015)
#' 
#' # Count the median person income for female population in 2012
#' median_female_person_income(zips, year=2012)
#' 
#' # Count the median person income for female population in California state
#' median_female_person_income(c("California")) # State name
#' median_female_person_income(c("CA")) # State code
#' median_female_person_income(c("06")) # 2-digit state FIPS code
median_female_person_income <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_MOD4_AGE_BRACKETS, 
                        function(x) paste0("Median_Income_Person_", x, "_Female_WithIncome"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.median_person_income(geo_map, statvar_map, start_year, end_year, year))
}

.median_person_income <- function(geo_map, statvar_map, start_year, end_year, year) {
  
  start_year <- if (!is.na(year)) year else start_year
  end_year <- if (!is.na(year)) year else end_year
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <- .http_post(DCAPI_STAT_ALL, body);
  
  output <- list()
  for (year in as.character(start_year:end_year)) {
    observation_table <- .get_observation_table(http_response, geo_map, statvar_map, year)
    provenance_table <- .get_provenance_table(http_response, geo_map, statvar_map)
    observation_table <- merge(x=observation_table, y=provenance_table, by="geoName", all.x=TRUE)
    rownames(observation_table) <- observation_table$geoName
    output[[year]] <- observation_table
  }
  return (output)
}