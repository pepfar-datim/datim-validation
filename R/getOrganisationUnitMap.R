#' @export
#' @title Get Organisation Unit Map
#'
#' @description Utility function to produce a map of valid mechanisms
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo
#' (which is the UID of interest)
#'
#' @examples \dontrun{
#'     # For a specific operating unit
#'     ou_map <- getOrganisationUnitMap("KKFzPM8LoXs")
#'
#'     # Based on your login credentials
#'     ou_map_mine <- getOrganisationUnitMap()
#' }
#'
getOrganisationUnitMap <- function(organisationUnit = NA,
                                   d2session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {
  if (is.na(organisationUnit)) {
    organisationUnit <- d2session$user_orgunit
  }
  url <- utils::URLencode(paste0(d2session$base_url, "api/", api_version(),
                                 "/organisationUnits.json?&filter=path:like:",
                                 organisationUnit,
                                 "&fields=id,code,name,shortName&paging=false"))


  r <- httpcache::GET(url, httr::timeout(getHTTPTimeout()), handle = d2session$handle)
  if (r$status == 200L) {
    r <-  httr::content(r, "text")
    sites <- jsonlite::fromJSON(r, flatten = TRUE)[[1]]
     } else {
      stop(paste("Could not retreive site listing", httr::content(r, "text")))
     }

  return(sites)
}
