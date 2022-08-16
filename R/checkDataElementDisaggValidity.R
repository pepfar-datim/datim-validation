#' @export
#' @title Check Data Element Disagg Validity
#'
#' @description Utility function to produce a data frame of
#' invalid data elements based on current
#' DATIM form specification.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName",
#' "dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data, if any.
#' If there are no violations, a boolean TRUE is returned.
#' @examples \dontrun{
#'   d <- d2Parser("myfile.csv",type="csv")
#'   ds <- getCurrentMERDataSets(type="RESULTS")
#'   checkDataElementDisaggValidity(d,ds)
#' }
checkDataElementDisaggValidity <- function(data,
                                           datasets = NA,
                                           return_violations = TRUE,
                                           d2session =
                                             dynGet("d2_default_session",
                                                    inherits = TRUE)) {

  des <- getValidDataElements(datasets, d2session = d2session) %>%
         dplyr::select(dataElement = dataelementuid,
                       categoryOptionCombo = categoryoptioncombouid)

  data_des_cocs_bad <- dplyr::anti_join(data,
                                        des,
                                        by = c("dataElement",
                                               "categoryOptionCombo"))

  if (NROW(data_des_cocs_bad) > 0) {
    warning("Invalid data element / category option combos found!")
    if (return_violations) {
      return(data_des_cocs_bad)
    }
  } else {
    return(TRUE)
  }

}
