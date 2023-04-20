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
    d2_api_get("organisationUnits?level=3&fields=id,name&paging=false", d2session) %>% 
    purrr::pluck("organisationUnits")

}
