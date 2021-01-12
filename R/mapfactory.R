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