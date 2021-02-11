#' Summarize multiple data frames by aggregating the numerical data of same
#' grouping columns.
#' 
#' @param ... required, the list of data frames.
#' @param group_cols optional, the column names for applying the "group by" 
#'   operation. The columns "geoNames" and "provenanceDomain" by default.
#' @param FUN optional, the function to aggregate the numerical data. Sum by 
#'   default.
#' @return A new data frame
summarize <- function(..., group_cols=c("geoName","provenanceDomain"), FUN=sum) {
  
  df <- bind_rows(...) %>%
        group_by(across(all_of(group_cols))) %>%
        summarise_if(is.numeric, FUN) %>%
        as.data.frame()
  return(df)
}

#' Collapse a commondata table by aggregating the most end denominator.
#' 
#' @param table required, the commondata table.
#' @param group_cols optional, the column names for applying the "group by" 
#'   operation. The columns "geoNames" and "provenanceDomain" by default.
#' @param FUN optional, the function to aggregate the numerical data. Sum by 
#'   default.
#' @return A new commondata table
collapse <- function(table, group_cols=c("geoName","provenanceDomain"), FUN=sum) {
  
  contains_only_data_frames <- all(sapply(table, is.data.frame))
  if (contains_only_data_frames) {
    return(summarize(unname(table), group_cols=group_cols, FUN=FUN))
  } else {
    output <- list()
    for (path in names(table)) {
      output[[path]] <- collapse(table[[path]], group_cols, FUN)
    }
    return(output)
  }
}