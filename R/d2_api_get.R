
#' @title Execute and return a DATIM API query.
#' @param path Should exclude the "api" portion of the HTTP request. This will
#' be added as part of the GET request.
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logining in to datim with loginToDATIM
#' @return Result the result of the API query as a list.
#'
#' Simplified version of the original api_get function from datimutils.
d2_api_get <- function(path,
                    d2session,
                    timeout = 60) {

  base_url <- d2session$base_url
  handle <- d2session$handle

  url <- paste0(d2session$base_url, "api/", path)

  if (is.null(d2session$token)) {
    r <- httpcache::GET(url, httr::timeout(timeout),
                   handle = handle)
  } else {
    print("Using token")
    r <- httpcache::GET(url,
                   httr::timeout(timeout),
                   handle = handle,
                   httr::add_headers(Authorization =
                                       paste("Bearer",
                                             d2session$token$credentials$access_token, sep = " ")))
  }

  if (r$status_code == 200L && httr::http_type(r) == "application/json") {
    httr::content(r, "text") %>%
      jsonlite::fromJSON(., flatten = TRUE)
  } else {
    warning(paste(
      "Could not retreive",
      url,
      "with error code",
      r$status_code
    ))
    return(NULL)
  }


}
