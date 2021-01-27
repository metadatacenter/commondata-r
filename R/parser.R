.get_place_data <- function(obj, geo_dcid) {
  return (obj[[PLACE_DATA]][[geo_dcid]])
}

.get_statvar_data <- function(obj, statvar_dcid) {
  return (obj[[STATVAR_DATA]][[statvar_dcid]])
}

.get_source_series <- function(obj) {
  return (obj[1][[SOURCE_SERIES]][1][[1]])
}

.get_statvar_value <- function(obj, temporal) {
  source_series_values <- .get_source_series(obj)[[VAL]]
  value <- source_series_values[[temporal]]
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