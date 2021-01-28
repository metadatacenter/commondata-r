#' Return the latest statistical data given the list of zip codes and the list
#' of the statistical variables.
#' 
#' @param zips required, vector of string(s) of geographical names
#' @param statvars required, vector of statistical variables. The variable list
#'    can be found at https://docs.datacommons.org/statistical_variables.html
#' @param verbose optional, boolean whether the time information should be
#'    included in the output. True by default.
#' @return  A data frame containing the values of each statistical variable
#'    per zip code.
#'     
#' @export
#' @examples
#' zips <- c("94035","94039","94040","94041","94042","94043")
#' statvars <-("Count_Person", "Median_Age_Person")
#' latest_statistical_data(zips, statvars)
latest_statistical_data <- function(zips, statvars, verbose=TRUE) {
  
  geo_map <- .create_geo_dcid_map(zips)
  
  statvar_map <- sapply(statvars, 
                        function(x) x, 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  df <- .get_latest_statistical_data(geo_map, statvar_map)
  
  if (!verbose) {
    df <- df[, -grep("_Date", colnames(df))]
  }
  
  return(df)
}

.get_latest_statistical_data <- function(geo_map, statvar_map) {
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <- .http_post(DCAPI_STAT_SET, body);
  
  observation_table <- .get_latest_observation_table(http_response, geo_map, statvar_map)
  return (observation_table)
}

.get_latest_observation_table <- function(obj, geo_map, statvar_map) {
  
  output <- data.frame(zipCode=names(geo_map))
  for (observation in names(statvar_map)) {
    obs_df <- data.frame(zipCode=names(geo_map))
    statvar_values <- c()
    statvar_temporal <- c()
    for (geo_name in names(geo_map)) {
      geo_dcid <- geo_map[[geo_name]]
      statvar_dcid <- statvar_map[[observation]]
      
      statvar_data <- .get_statvar_data(obj, statvar_dcid)
      place_data <- .get_place_data(statvar_data, geo_dcid)
      
      value <- .get_statvar_value(place_data)
      temporal <- .get_statvar_date(place_data)
      
      statvar_values <- c(statvar_values, value)
      statvar_temporal <- c(statvar_temporal, temporal)
    }
    obs_df[, observation] <- statvar_values
    obs_df[, paste0(observation, "_Date")] <- statvar_temporal
    output <- merge(x=output, y=obs_df, by="zipCode", all.x=TRUE)
  }
  return (output)
}