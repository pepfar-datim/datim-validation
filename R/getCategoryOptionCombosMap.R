#' @export
#' @title Get Category Option Combos Map
#'
#' @description Utility function to produce a map of category option combos
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of code, name, id, and shortName
#' of all categoryOptionCombos
#' @examples \dontrun{
#' coc_map <- getCategoryOptionCombosMap()
#' }

getCategoryOptionCombosMap <- function(d2session = dynGet("d2_default_session",
                                                        inherits = TRUE)) {

  url <- utils::URLencode(
    paste0(d2session$base_url, "api/", api_version(),
           "/categoryOptionCombos?fields=id,name,shortName,code&paging=false"))
  r <- httpcache::GET(url,
               httr::timeout(300),
               handle = d2session$handle)
  if (r$status == 200L) {
    r <-  httr::content(r, "text")
    cocs <- jsonlite::fromJSON(r, flatten = TRUE)[[1]]
    return(cocs)
  } else {
    stop(paste("Could not retreive category option combos map",
               httr::content(r, "text")))
  }
}
