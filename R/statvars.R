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



# Helpers ----------------------------------------------------------------------------------

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
    # initialize main data frame
    df1 <- data.frame(geoName=names(geo_map))
    for (age_bracket in names(statvar_map)) {
      # initialize data frame to store each statistical variable per zip code
      df2 <- data.frame(geoName=names(geo_map))
      statvar_values <- c()
      for (geoid in names(geo_map)) {
        geo_dcid <- geo_map[[geoid]]
        place_data <- .get_place_data(http_response, geo_dcid)
        
        statvar_dcid <- statvar_map[[age_bracket]]
        statvar_data <- .get_statvar_data(place_data, statvar_dcid)
        
        value <- .get_statvar_value_from_year(statvar_data, year)
        statvar_values <- c(statvar_values, value)
      }
      df2[, age_bracket] <- factor(statvar_values)
      df1 <- merge(x=df1, y=df2, by="geoName", all.x=TRUE)
    }
    provenance_df <- .get_provenance_info(http_response, geo_map, statvar_map)
    
    df1 <- merge(x=df1, y=provenance_df, by="geoName", all.x=TRUE)
    rownames(df1) <- df1$geoName
    output[[year]] <- df1
  }
  return (output)
}

.get_provenance_info <- function(obj, geo_map, statvar_map) {
  
  output <- data.frame(geoName=names(geo_map), measurementMethod=NA,
                       provenanceDomain=NA, provenanceUrl=NA)
  
  measurement_methods <- c()
  provenance_domains <- c()
  provenance_urls <- c()
  for (geoid in names(geo_map)) {
    geo_dcid <- geo_map[[geoid]]
    place_data <- .get_place_data(obj, geo_dcid)
    measurement_method <- NA
    provenance_domain <- NA
    provenance_url <- NA
    for (age_bracket in names(statvar_map)) {
      statvar_dcid <- statvar_map[[age_bracket]]
      statvar_data <- .get_statvar_data(place_data, statvar_dcid)
      
      measurement_method <- .coalesce(measurement_method, 
                                      .get_statvar_measurement_method(statvar_data))
      provenance_domain <- .coalesce(provenance_domain, 
                                     .get_statvar_provenance_domain(statvar_data))
      provenance_url <- .coalesce(provenance_url, 
                                  .get_statvar_provenance_url(statvar_data))
    }
    measurement_methods <- c(measurement_methods, measurement_method)
    provenance_domains <- c(provenance_domains, provenance_domain)
    provenance_urls <- c(provenance_urls, provenance_url)
  }
  output$measurementMethod <- factor(measurement_methods)
  output$provenanceDomain <- factor(provenance_domains)
  output$provenanceUrl <- factor(provenance_urls)
  
  return (output)
}

.http_post <- function(api, body) {
  response <- httr::POST(api, body = body)
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  return (.parse(response))
}

.parse <- function(response) {
  return (httr::content(response, "parsed"))
}

.create_zip_dcid_map <- function(zips) {
  zip_map <- sapply(zips, 
                    function(x) paste0("zip/", x), 
                    simplify = FALSE, USE.NAMES = TRUE)
  return (zip_map)
}

.create_state_dcid_map <- function(states) {
  state_map <- sapply(states,
                      function(x) {
                        fips <- usmap::fips(x)
                        paste0("geoId/", if(is.na(fips)) x else fips)},
                      simplify = FALSE, USE.NAMES = TRUE)
  return (state_map)
}

.create_geo_dcid_map <- function(geo_names, level) {
  switch(level,
         zip = .create_zip_dcid_map(geo_names),
         state = .create_state_dcid_map(geo_names))
}

.get_place_data <- function(obj, zip_dcid) {
  return (obj[[PLACE_DATA]][[zip_dcid]])
}

.get_statvar_data <- function(obj, statvar_dcid) {
  return (obj[[STATVAR_DATA]][[statvar_dcid]])
}

.get_source_series <- function(obj) {
  return (obj[1][[SOURCE_SERIES]][1][[1]])
}

.get_statvar_value_from_year <- function(obj, year) {
  source_series_values <- .get_source_series(obj)[[VAL]]
  value <- source_series_values[[year]]
  return (.check_not_empty(value))
}

.get_statvar_measurement_method <- function(obj) {
  return (.get_source_series(obj)[[MEASUREMENT_METHOD]])
}

.get_statvar_provenance_domain <- function(obj) {
  return (.get_source_series(obj)[[PROVENANCE_DOMAIN]])
}

.get_statvar_provenance_url <- function(obj) {
  return (.get_source_series(obj)[[PROVENANCE_URL]])
}

.check_not_empty <- function(value) {
  return (if (is.null(value)) NA else value)
}

.coalesce <- function(v1, v2) {
  v1 <- .check_not_empty(v1)
  v2 <- .check_not_empty(v2)
  return (dplyr::coalesce(v1, v2))
}
