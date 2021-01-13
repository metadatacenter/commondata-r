count_household_income_by_age <- function(geo_names, level=c(default="zip", "state"), 
                                          start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names, match_arg(level))
  
  statvar_with_denominator_map <- list()
  for (age_bracket in CENSUS_HOUSEHOLDER_AGE_BRACKETS) {
    statvar_with_denominator_map[[age_bracket]] <- 
      sapply(CENSUS_INCOME_BRACKETS,
             function(x) paste0("Count_Household_HouseholderAge", age_bracket, "_IncomeOf", x),
             simplify = FALSE, USE.NAMES = TRUE)
  }
  
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
      rownames(observation_table) <- observation_table$geoName
      data[[denominator]] <- observation_table
    }
    output[[year]] <- data
  }
  return (output)
}