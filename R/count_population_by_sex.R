#' Return the human population count per age group and per sex category for the
#' given the geographical names (i.e., zip codes, state codes, state names,
#' county names, or school district) and the observation year period.
#' 
#' @param geo_names required, vector of string(s) of geographical names
#' @param location_type optional, string indicating the location type of the
#'    geographical names. NA by default.
#' @param start_year optional, integer indicating the start year of observation.
#'    2011 by default.
#' @param end_year optional, integer indicating the end year of observation.
#'    2018 by default.
#' @param year optional, integer indicating a single year of observation.
#'    This parameter overrides the start_year and end_year parameters when
#'    it is not NA, such that start_year=year and end_year=year. NA by default.
#' @return A named list with each list item is a data frame containing the 
#'    human population count of each region per age group. The data frame is 
#'    identified by the observation year and the sex category.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' 
#' # Count the population in the specified ZIP codes
#' count_population(zips)
#' 
#' # Count the population from 2012 to 2015
#' count_population(zips, start_year=2012, end_year=2015)
#' 
#' # Count the population in 2012
#' count_population(zips, year=2012)
#' 
#' # Count the population in the state of California
#' count_population(c("California")) # State name
#' count_population(c("CA")) # State code
#' count_population(c("06")) # 2-digit state FIPS code
count_population <- function(geo_names,
                                  location_type=c(NA, "zip", "county", "state", "school"),
                                  start_year=2011, end_year=2018, year=NA) {
  
  location_type <- match.arg(location_type)
  geo_map <- .create_geo_dcid_map(geo_names, location_type)
  
  statvar_map <- list()
  for (sex_category in CENSUS_SEX_CATEGORIES) {
    statvar_map[[sex_category]] <- 
      sapply(CENSUS_PERSON_AGE_GROUPS,
             function(x) paste0("Count_Person_", x, "_", sex_category),
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
  return (.get_statistical_data(geo_map, statvar_map, start_year, end_year, year))
}