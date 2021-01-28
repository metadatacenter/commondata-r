.get_statistical_data <- function(geo_map, statvar_map, start_temporal,
                                  end_temporal, temporal_point) {
  
  start_temporal <- if (!is.na(temporal_point)) temporal_point else start_temporal
  end_temporal <- if (!is.na(temporal_point)) temporal_point else end_temporal
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <- .http_post(DCAPI_STAT_ALL, body);
  
  output <- list()
  temporal_range <- seq(start_temporal, end_temporal, by=1)
  for (temporal in as.character(temporal_range)) {
    observation_table <- .get_observation_table(http_response, geo_map, statvar_map, temporal)
    if (.has_observations(observation_table)) {
      provenance_table <- .get_provenance_table(http_response, geo_map, statvar_map, temporal)
      observation_table <- merge(x=observation_table, y=provenance_table, by="geoName", all.x=TRUE)
      output[[temporal]] <- observation_table
    }
  }
  return (output)
}

.get_statistical_data_with_denominator <- function(geo_map, statvar_with_denominator_map,
                                                   start_temporal, end_temporal,
                                                   temporal_point) {
  
  start_temporal <- if (!is.na(temporal_point)) temporal_point else start_temporal
  end_temporal <- if (!is.na(temporal_point)) temporal_point else end_temporal
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_with_denominator_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <- .http_post(DCAPI_STAT_ALL, body);
  
  output <- list()
  temporal_range <- seq(start_temporal, end_temporal, by=1)
  for (temporal in as.character(temporal_range)) {
    data <- list()
    has_observations = c()
    for (denominator in names(statvar_with_denominator_map)) {
      statvar_map <- statvar_with_denominator_map[[denominator]]
      observation_table <- .get_observation_table(http_response, geo_map, statvar_map, temporal)
      has_observation <- .has_observations(observation_table)
      if (has_observation) {
        provenance_table <- .get_provenance_table(http_response, geo_map, statvar_map, temporal)
        observation_table <- merge(x=observation_table, y=provenance_table, by="geoName", all.x=TRUE)
        data[[denominator]] <- observation_table
      }
      has_observations <- c(has_observations, has_observation)
    }
    if (any(has_observations)) {
      output[[temporal]] <- data
    }
  }
  return (output)
}

.get_observation_table <- function(obj, geo_map, statvar_map, temporal) {
  output <- data.frame(geoName=names(geo_map))
  for (observation in names(statvar_map)) {
    obs_df <- data.frame(geoName=names(geo_map))
    statvar_values <- c()
    for (geo_name in names(geo_map)) {
      geo_dcid <- geo_map[[geo_name]]
      place_data <- .get_place_data(obj, geo_dcid)
      
      statvar_dcid <- statvar_map[[observation]]
      statvar_data <- .get_statvar_data(place_data, statvar_dcid)
      
      value <- .get_statvar_value(statvar_data, temporal)
      statvar_values <- c(statvar_values, value)
    }
    obs_df[, observation] <- statvar_values
    output <- merge(x=output, y=obs_df, by="geoName", all.x=TRUE)
  }
  return (output)
}

.get_provenance_table <- function(obj, geo_map, statvar_map, temporal) {
  
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
    for (statvar in names(statvar_map)) {
      statvar_dcid <- statvar_map[[statvar]]
      statvar_data <- .get_statvar_data(place_data, statvar_dcid)
      
      statvar_value <- .get_statvar_value(statvar_data, temporal)
      if (!is.na(statvar_value)) {
        measurement_method <- .coalesce(measurement_method, 
                                        .get_statvar_measurement_method(statvar_data))
        provenance_domain <- .coalesce(provenance_domain, 
                                       .get_statvar_provenance_domain(statvar_data))
        provenance_url <- .coalesce(provenance_url, 
                                    .get_statvar_provenance_url(statvar_data))
      }
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

.has_observations <- function(observation_table) {
  observations <- observation_table[,-seq_len(1)]
  return (!all(is.na(observations)))
}