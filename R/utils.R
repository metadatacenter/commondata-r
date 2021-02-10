.check_not_empty <- function(v) {
  return (if (is.null(v)) NA else v)
}

.is_nested <- function(l) {
  stopifnot(is.list(l))
  for (i in l) {
    if (is.list(i)) return(TRUE)
  }
  return(FALSE)
}

.coalesce <- function(v1, v2) {
  v1 <- .check_not_empty(v1)
  v2 <- .check_not_empty(v2)
  return (dplyr::coalesce(v1, v2))
}

match_arg <- base::match.arg
body(match_arg)[[c(4, 3, 2, 3)]] <- quote(
  return (
    if ("default" %in% names(arg)) {
      arg[["default"]]
    }
    else {
      arg[[1L]]
    })
)