count_all_household_income <- function(geo_names, level=c(default="zip", "state"), 
                                       start_year=2011, end_year=2018, year=NA) {
  
  geo_map <- .create_geo_dcid_map(geo_names, match_arg(level))
  
  statvar_map <- sapply(CENSUS_INCOME_BRACKETS, 
                        function(x) paste0("Count_Household_IncomeOf", x), 
                        simplify = FALSE, USE.NAMES = TRUE)
  
  return (.count_household_income(geo_map, statvar_map, start_year, end_year, year))
}

.count_household_income <- function(geo_map, statvar_map, start_year, end_year, year) {
  
  start_year <- if (!is.na(year)) year else start_year
  end_year <- if (!is.na(year)) year else end_year
  
  body <- jsonlite::toJSON(list(
    stat_vars = as.vector(unlist(statvar_map)), 
    places = as.vector(unlist(geo_map))), 
    auto_unbox = TRUE)
  
  http_response <<- .http_post(DCAPI_STAT_ALL, body);
  
  output <- list()
  for (year in as.character(start_year:end_year)) {
    # initialize main data frame
    df1 <- data.frame(geoName=names(geo_map))
    for (age_bracket in names(statvar_map)) {
      # initialize data frame to store each statistical variable per zip code
      df2 <- data.frame(geoName=names(geo_map))
      statvar_values <- c()
      for (geo_name in names(geo_map)) {
        geo_dcid <- geo_map[[geo_name]]
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