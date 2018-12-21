#' @export
#' @title Utility function which provides a list of valid operating units
#' 
#' @description Utility function which provides a list of valid operating units
#' 
#' @return Returns a data frame of operating units and their UIDs

getValidOperatingUnits <- function() {
  r <-
    httr::GET(URLencode(
      paste0(
        getOption("baseurl"),
        "api/",
        api_version(),
        "/organisationUnits?level=3&fields=id,name&paging=false"
      )
    ), httr::timeout(60))
  
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
