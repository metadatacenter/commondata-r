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

.create_county_dcid_map <- function(counties) {
  county_map <- sapply(counties,
                      function(x) {
                        county <- trimws(gsub("^(.*?),.*", "\\1", x))
                        state <- trimws(gsub("^.*,(.*)", "\\1", x))
                        fips <- usmap::fips(county = county, state = state)
                        paste0("geoId/", if(is.na(fips)) x else fips)},
                      simplify = FALSE, USE.NAMES = TRUE)
  return (state_map)
}

.create_school_district_dcid_map <- function(zips) {
  zip_map <- sapply(zips, 
                    function(x) paste0("geoId/sch", x), 
                    simplify = FALSE, USE.NAMES = TRUE)
  return (zip_map)
}

.create_geo_dcid_map <- function(geo_names, location_type=NA) {
  if (is.na(location_type)) {
    sample_name = geo_names[1]
    location_type <- .determine_location_type(sample_name)
  }
  switch(location_type,
         zip = .create_zip_dcid_map(geo_names),
         state = .create_state_dcid_map(geo_names),
         county = .create_county_dcid_map(geo_names),
         school = .create_school_district_dcid_map(geo_names))
}

.determine_location_type <- function(geo_name) {
  if (grepl("\\d{5}", geo_name)) {
    return ("zip")
  } else if (grepl("[A-Z]{2}", geo_name)) {
    return ("state")
  } else if (grepl("\\d{2}", geo_name)) {
    return ("state")
  } else if (geo_name %in% US_STATES) {
    return ("state")
  } else if (grepl(",", geo_name)) {
    return ("county")
  } else {
    stop("Unable to determine the location type automatically. Consider to specifying it using the 'location_type' argument")
  }
}