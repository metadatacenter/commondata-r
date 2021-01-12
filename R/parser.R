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

.get_provenance_info <- function(obj, geo_map, statvar_map) {
  
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

.get_statvar_measurement_method <- function(obj) {
  return (.get_source_series(obj)[[MEASUREMENT_METHOD]])
}

.get_statvar_provenance_domain <- function(obj) {
  return (.get_source_series(obj)[[PROVENANCE_DOMAIN]])
}

.get_statvar_provenance_url <- function(obj) {
  return (.get_source_series(obj)[[PROVENANCE_URL]])
}