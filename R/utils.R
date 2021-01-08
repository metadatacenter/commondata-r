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