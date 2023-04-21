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

  path <- "categoryOptionCombos?fields=id,name,shortName,code&paging=false"

  d2_api_get(path, d2session = d2session) %>%
    purrr::pluck("categoryOptionCombos")

}
