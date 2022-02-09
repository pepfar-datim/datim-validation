#' @export
#' @title Utility function which provides a list of valid operating units
#'
#' @inheritParams datim_validation_params
#'
#' @description Utility function which provides a list of valid operating units
#' 
#' @return Returns a data frame of operating units and their UIDs

getValidOperatingUnits <- function(d2session = d2_default_session) {
  r <-
    httr::GET(URLencode(
      paste0(
        d2session$base_url,
        "api/",
        api_version(),
        "/organisationUnits?level=3&fields=id,name&paging=false"
      )
    ), httr::timeout(300), handle = d2session$handle)
  
  if (r$status == 200) {
     httr::content(r, "text") %>% 
     jsonlite::fromJSON(. , flatten = TRUE) %>%
     rlist::list.extract(.,"organisationUnits")
  } else {
    stop(paste(
      "Could not retreive valid operating units",
      httr::content(r$status)))
  }
}
