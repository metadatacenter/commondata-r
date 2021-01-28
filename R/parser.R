.get_place_data <- function(obj, geo_dcid) {
  obj_names <- names(obj)
  if (PLACE_DATA %in% obj_names) {
    return (obj[[PLACE_DATA]][[geo_dcid]])  
  } else if (STAT %in% obj_names) {
    return (obj[[STAT]][[geo_dcid]])
  } else {
    return (obj)
  }
}

.get_statvar_data <- function(obj, statvar_dcid) {
  obj_names <- names(obj)
  if (STATVAR_DATA %in% obj_names) {
    return (obj[[STATVAR_DATA]][[statvar_dcid]])
  } else if (DATA %in% obj_names) {
    return (obj[[DATA]][[statvar_dcid]])
  } else {
    return (obj)
  }
}

.get_source_series <- function(obj) {
  return (obj[1][[SOURCE_SERIES]][1][[1]])
}

.get_statvar_value <- function(obj, temporal) {
  source_series_values <- .get_source_series(obj)[[VAL]]
  value <- source_series_values[[temporal]]
  return (.check_not_empty(value))
}

.get_statvar_value <- function(obj) {
  value <- (obj[[VALUE]])
  return (.check_not_empty(value))
}

.get_statvar_date <- function(obj) {
  date <- (obj[[DATE]])
  return (.check_not_empty(date))
}

.get_metadata <- function(obj) {
  return (obj[[METADATA]])
}

.get_statvar_measurement_method <- function(obj) {
  return (.get_provenance_info(obj, MEASUREMENT_METHOD))
}

.get_statvar_provenance_domain <- function(obj) {
  return (.get_provenance_info(obj, PROVENANCE_DOMAIN))
}

.get_statvar_provenance_url <- function(obj) {
  return(.get_provenance_info(obj, PROVENANCE_URL))
}

.get_provenance_info <- function(obj, provenance_name) {
  obj_names <- names(obj)
  if (SOURCE_SERIES %in% obj_names) {
    return (.get_source_series(obj)[[provenance_name]])
  } else if (METADATA %in% obj_names) {
    return (.get_metadata(obj)[[provenance_name]])
  } else {
    return (obj)
  }
}