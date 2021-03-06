.get_state_fips <- function(state_name) {
  result <- acs::geo.lookup(state=state_name)
  if (nrow(result) == 1) {
    return (.get_fips(as.list(result), "state", 2))
  } else {
    best_match = .get_best_match(result, "state.name", state_name)
    return (.get_fips(as.list(best_match), "state", 2))
  }
}

.get_county_fips <- function(county_with_state_name) {
  county_name <- trimws(gsub("^(.*?),.*", "\\1", county_with_state_name))
  state_name <- trimws(gsub("^.*,(.*)", "\\1", county_with_state_name))
  result <- acs::geo.lookup(state=state_name, county=county_name)
  state_fips = .get_state_fips(state_name)
  if (nrow(result) == 1) {
    return (paste0(state_fips, .get_fips(as.list(result), "county", 3)))
  } else {
    best_match = .get_best_match(result, "county.name", county_name)
    return (paste0(state_fips, .get_fips(as.list(best_match), "county", 3)))
  }
}

.get_city_fips <- function(city_with_state_name) {
  city_name <- trimws(gsub("^(.*?),.*", "\\1", city_with_state_name))
  state_name <- trimws(gsub("^.*,(.*)", "\\1", city_with_state_name))
  result <- acs::geo.lookup(state=state_name, place=city_name)
  state_fips = .get_state_fips(state_name)
  if (nrow(result) == 1) {
    return (paste0(state_fips, .get_fips(as.list(result), "place", 5)))
  } else {
    best_match = .get_best_match(result, "place.name", city_name)
    return (paste0(state_fips, .get_fips(as.list(best_match), "place", 5)))
  }
}

.get_school_district_fips <- function(school_district_with_state_name) {
  school_district_name <- trimws(gsub("^(.*?),.*", "\\1", school_district_with_state_name))
  state_name <- trimws(gsub("^.*,(.*)", "\\1", school_district_with_state_name))
  result <- acs::geo.lookup(state=state_name, school.district=school_district_name)
  state_fips = .get_state_fips(state_name)
  if (nrow(result) == 1) {
    return (paste0(state_fips, .get_fips(as.list(result), "school.district", 5)))
  } else {
    best_match = .get_best_match(result, "school.district.name", school_district_name)
    return (paste0(state_fips, .get_fips(as.list(best_match), "school.district", 5)))
  }
}

.get_fips <- function(list, variable, fips_digit) {
  fips = list[[variable]]
  return (str_pad(fips, fips_digit, pad="0"))
}

.get_best_match <- function(df, variable, match_string) {
  for (i in rownames(df)) {
    list <- df[i,]
    found_string <- list[[variable]]
    if (is.na(found_string)) {
      next
    } else {
      if (startsWith(found_string, match_string)) {
        return (list)
      }
    }
  }
  stop(paste0("Unable to find ", variable, " = '", match_string, "'"))
}