#' Combine two commondata data tables by aligning the table columns properly.
#' The data are then grouped based on the group_cols and then aggregated using
#' the function FUN parameters. The result is a new combined commondata data 
#' table.
#' 
#' @param d1 required, the first commondata data table.
#' @param d2 required, the second commondata data table.
#' @param group_cols optional, the column names for the group operation.
#'   "geoNames" and "provenanceDomain" columns are used by default.
#' @param FUN optional, the function to aggregate the numerical data. The sum
#'    operation is used by default.
#' @return A new commondata data table
#' 
#' @export
#' @example 
#' # Combine the count data between male and female populations in Palo Alto
#' combine(count_female_population(PALO_ALTO_CITY_CA), count_male_population(PALO_ALTO_CITY_CA))
combine <- function(d1, d2, group_cols=c("geoName","provenanceDomain"), FUN=sum) {
  
  d <- list()
  temporal <- names(d1)
  for (t in temporal) {
    df <- bind_rows(d1[[t]], d2[[t]]) %>%
          group_by(across(all_of(group_cols))) %>%
          summarise_if(is.numeric, FUN)
    d[[t]] <- as.data.frame(df)
  }
  return(d)
}