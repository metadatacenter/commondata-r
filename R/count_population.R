#' Return the human population count per age group for the given the geographical
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
#'    human population count of each region per age group. The data frame is 
#'    identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the human population in the specified ZIP codes
#' count_population(zips)
#' 
#' # Count the human population from 2012 to 2015
#' count_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the human population in 2012
#' count_population(zips, year=2012)
#' 
#' # Count the human population in California state
#' count_population(c("California")) # State name
#' count_population(c("CA")) # State code
#' count_population(c("06")) # 2-digit state FIPS code
count_population <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_MOD2_AGE_BRACKETS, 
                        function(x) paste0("Count_Person_", x), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_population(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the male human population count per age group for the given the
#' geographical names (i.e., zip codes, state codes, state names, county names) 
#' and theobservation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return A named list with each list item is a data frame containing the 
#'    male population count of each region per age group. The data frame is 
#'    identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the male population in the specified ZIP codes
#' count_male_population(zips)
#' 
#' # Count the male population from 2012 to 2015
#' count_male_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the male population in 2012
#' count_male_population(zips, year=2012)
#' 
#' # Count the male population in the state of California
#' count_male_population(c("California")) # State name
#' count_male_population(c("CA")) # State code
#' count_male_population(c("06")) # 2-digit state FIPS code
count_male_population <- function(geo_names, start_year=2011, end_year=2018, year=NA) {

  geo_map <- .create_geo_dcid_map(geo_names)

  statvar_map <- sapply(CENSUS_MOD1_AGE_BRACKETS, 
                        function(x) paste0("Count_Person_", x, "_Male"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_population(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the female human population count per age group for the given the 
#' geographical names (i.e., zip codes, state codes, state names, county names)
#' and the observation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return A named list with each list item is a data frame containing the 
#'    female population count of each region per age group. The data frame is 
#'    identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the female population in the specified ZIP codes
#' count_male_population(zips)
#' 
#' # Count the female population from 2012 to 2015
#' count_male_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the female population in 2012
#' count_male_population(zips, year=2012)
#' 
#' # Count the female population in the state of California
#' count_male_population(c("California")) # State name
#' count_male_population(c("CA")) # State code
#' count_male_population(c("06")) # 2-digit state FIPS code
count_female_population <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_MOD1_AGE_BRACKETS, 
                        function(x) paste0("Count_Person_", x, "_Female"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_population(geo_map, statvar_map, start_year, end_year, year))
}

.count_population <- function(geo_map, statvar_map, start_year, end_year, year) {
  
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
