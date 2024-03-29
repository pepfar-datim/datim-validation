#' @export
#' @title Utility function which provides a list of valid operating units
#'
#' @inheritParams datim_validation_params
#'
#' @description Utility function which provides a list of valid operating units
#'
#' @return Returns a data frame of operating units and their UIDs

getValidOperatingUnits <- function(d2session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {
  r <-
    httpcache::GET(utils::URLencode(
      paste0(
        d2session$base_url,
        "api/",
        api_version(),
        "/organisationUnits?level=3&fields=id,name&paging=false"
      )
    ), httr::timeout(getHTTPTimeout()), handle = d2session$handle)

  if (r$status == 200) {
     httr::content(r, "text") %>%
     jsonlite::fromJSON(., flatten = TRUE) %>%
     purrr::pluck("organisationUnits")
  } else {
    stop(paste(
      "Could not retreive valid operating units",
      httr::content(r$status)))
  }
}
