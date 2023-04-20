
#' @title Execute and return a DATIM API query.
#' @description Gets and flattens DATIM API query as dataframe.
#' @param path Should begin with api/ and contain the query
#' @param d2_session the d2Session object, default is "d2_default_session",
#' it will be made upon logining in to datim with loginToDATIM
#' @param retry number of times to try in case of failure,
#' default will not try again
#' @param timeout how long should a reponse be waited for
#' @param api_version defaults to current but can pass in version number
#' @param verbose return raw content with data
#' @param quiet Echo the URL which is called to the console.
#' @return Result of DATIM API query returned as named list.
#'
#' Copied almost verbatim from datimutils
d2_api_get <- function(path,
                    d2session,
                    timeout = 60) {
  
  base_url <- d2session$base_url
  handle <- d2session$handle

  url <- paste0(d2session$base_url,"api/", path)
  
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
  
  if (r$status == 200L) {
    httr::content(r, "text") %>%
      jsonlite::fromJSON(., flatten = TRUE) } else {
        stop(paste( "Could not retreive", url, "with error code",
          httr::content(r$status)))
      }
  
  
}