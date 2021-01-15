#' Return the householders count per income bracket for the given ZIP codes and
#' observation year(s) period.
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
#'    frame is identified by the observation year.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the householders per income bracket in the specified ZIP codes
#' count_household_income(zips)
#' 
#' # Count the householders from 2012 to 2015
#' count_household_income(zips, start_year=2012, end_year=2015)
#' 
#' # Count the householders in 2012
#' count_household_income(zips, year=2012)
#' 
#' # Count the householders in the state of California
#' count_household_income(c("California")) # State name
#' count_household_income(c("CA")) # State code
#' count_household_income(c("06")) # 2-digit state FIPS code
count_household_income <- function(geo_names, start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names)
  
  statvar_map <- sapply(CENSUS_INCOME_BRACKETS, 
                        function(x) paste0("Count_Household_IncomeOf", x), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  start_year <- if (!is.na(year)) year else start_year
  end_year <- if (!is.na(year)) year else end_year
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <<- .http_post(DCAPI_STAT_ALL, body);
  
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