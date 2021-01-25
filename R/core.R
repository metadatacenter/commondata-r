.get_statistical_data <- function(geo_map, statvar_map, start_year, end_year, year) {
  
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
    output[[year]] <- observation_table
  }
  return (output)
}

.get_statistical_data_with_denominator <- function(geo_map, statvar_with_denominator_map,
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
      data[[denominator]] <- observation_table
    }
    output[[year]] <- data
  }
  return (output)
}

.get_observation_table <- function(obj, geo_map, statvar_map, year) {
  output <- data.frame(geoName=names(geo_map))
  for (observation in names(statvar_map)) {
    obs_df <- data.frame(geoName=names(geo_map))
    statvar_values <- c()
    for (geo_name in names(geo_map)) {
      geo_dcid <- geo_map[[geo_name]]
      place_data <- .get_place_data(obj, geo_dcid)
      
      statvar_dcid <- statvar_map[[observation]]
      statvar_data <- .get_statvar_data(place_data, statvar_dcid)
      
      value <- .get_statvar_value_from_year(statvar_data, year)
      statvar_values <- c(statvar_values, value)
    }
    obs_df[, observation] <- statvar_values
    output <- merge(x=output, y=obs_df, by="geoName", all.x=TRUE)
  }
  return (output)
}

.get_provenance_table <- function(obj, geo_map, statvar_map) {
  
  output <- data.frame(geoName=names(geo_map), measurementMethod=NA,
                       provenanceDomain=NA, provenanceUrl=NA)
  
  measurement_methods <- c()
  provenance_domains <- c()
  provenance_urls <- c()
  for (geo_name in names(geo_map)) {
    geo_dcid <- geo_map[[geo_name]]
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