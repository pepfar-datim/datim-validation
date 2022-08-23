
#' @export
#' @title API Version
#'
#' @description Internal utility function to get the current API version
#' of the DATIM server
#'
#' @return Version of the API.
#'
api_version <- function() "33"


getHTTPTimeout <- function() {
  if (is.numeric(Sys.getenv("HTTP_TIMEOUT"))) {
    return(Sys.getenv("HTTP_TIMEOUT"))
  }
  300L
}
