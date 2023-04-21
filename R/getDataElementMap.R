#' @export
#' @title Get Data Element Map
#'
#' @description Utility function of extraction of data element ids, codes,
#' shortNames, and names
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of id, code, shortName, and name
#' @examples \dontrun{
#' de_map <- getDataElementMap()
#' }

getDataElementMap <- function(d2session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
  path <- paste0("dataElements?fields=id,code,shortName,name,",
                 "valueType,optionSet[id],zeroIsSignificant&paging=false")
  d2_api_get(path, d2session = d2session) %>%
    purrr::pluck("dataElements")

}
