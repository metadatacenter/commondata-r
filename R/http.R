.http_post <- function(api, body) {
  response <- httr::POST(api, body = body)
  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  return (.parse(response))
}

.parse <- function(response) {
  return (httr::content(response, "parsed"))
}