#' Return the male population by 5-year age brackets, given the ZIP codes and
#' the observation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param level optional, string indicating the geographical level. Should be
#'    one of {"zip", "state"}. "zip" by default.
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a point of year of observation. If
#'    this parameter has a value then start_year=year and end_year=year.
#'    NA by default.
#' @return A named list with each list item is a data frame of the male
#'    population observations in a specific year. Each row in the data frame
#'    details the 5-year age brackets of a particular zip code.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the male population in a specific ZIP codes
#' count_male_population(zips)
#' 
#' # Count the male population from 2012 to 2015
#' count_male_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the male population in 2012
#' count_male_population(zips, year=2012)
#' 
#' # Count the male population in California state
#' count_male_population(c("California"), level="state") # State name
#' count_male_population(c("CA"), level="state") # State code
#' count_male_population(c("06"), level="state") # 2-digit state FIPS code
count_male_population <- function(geo_names, level=c(default="zip", "state"), 
                                  start_year=2011, end_year=2018, year=NA) {

  geo_map <- .create_geo_dcid_map(geo_names, match_arg(level))

  statvar_map <- sapply(CENSUS_5_YEARS_BRACKETS, 
                        function(x) paste0("Count_Person_", x, "_Male"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_population_by_gender(geo_map, statvar_map, start_year, end_year, year))
}

#' Return the female population by 5-year age brackets, given the ZIP codes and
#' the observation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param level optional, string indicating the geographical level. Should be
#'    one of {"zip", "state"}. "zip" by default.
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a point of year of observation. If
#'    this parameter has a value then start_year=year and end_year=year.
#'    NA by default.
#' @return A named list with each list item is a data frame of the male
#'    population observations in a specific year. Each row in the data frame
#'    details the 5-year age brackets of a particular zip code.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the male population in a specific ZIP codes
#' count_male_population(zips)
#' 
#' # Count the male population from 2012 to 2015
#' count_male_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the male population in 2012
#' count_male_population(zips, year=2012)
#' 
#' # Count the female population in California state
#' count_female_population(c("California"), level="state") # State name
#' count_female_population(c("CA"), level="state") # State code
#' count_female_population(c("06"), level="state") # 2-digit state FIPS code
count_female_population <- function(geo_names, level=c(default="zip", "state"), 
                                    start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names, match_arg(level))
  
  statvar_map <- sapply(CENSUS_5_YEARS_BRACKETS, 
                        function(x) paste0("Count_Person_", x, "_Female"), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_population_by_gender(geo_map, statvar_map, start_year, end_year, year))
}

.count_population_by_gender <- function(geo_map, statvar_map, start_year, end_year, year) {
  
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
