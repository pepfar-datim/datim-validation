#' @export
#' @title Check Mechanism Period Validity
#'
#' @description This function will return an object of invalid mechanisms and
#' periods. All data which is reported
#' must have a period within the valid start and end dates of the
#' attribute option combination to which it is assigned.
#' The mechanism must also be associated with the operating unit.
#' If either of these two conditions
#' are not met, the data will be flagged as being invalid.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of invalid period-mechanism combinations.
#' A warning will also be issued. Returns TRUE if there are no violations.
#'
#' @examples \dontrun{
#'      d <- d2Parser("myfile.csv",type="csv")
#'      ds <- getCurrentMERDataSets(type="RESULTS")
#'      checkMechanismValidity(data=d, organisationUnit = "f5RoebaDLMx")
#' }
#'
checkMechanismValidity <- function(data,
                                   organisationUnit = NA,
                                   return_violations = TRUE,
                                   d2session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {

  if (is.na(organisationUnit)) {
    organisationUnit <- d2session$user_orgunit
  }

  data_mechs_periods <- data %>%
      dplyr::select(attributeOptionCombo, period) %>%
    dplyr::distinct()

  period_info <-
    do.call(rbind.data.frame,
            lapply(unique(data_mechs_periods$period), getPeriodFromISO))

  data_mechs_periods <- merge(data_mechs_periods,
                              period_info,
                              by.x = "period",
                              by.y = "iso")

  mechanism_map <-
    getMechanismsMap(organisationUnit = organisationUnit,
                     d2session = d2session) %>%
    dplyr::select(id, code, startDate, endDate)

  if (is.null(mechanism_map)) {
    stop("No valid mechanisms were found!")
  }

  names(mechanism_map) <-
    c("attributeOptionCombo",
      "code",
      "startDate_mech",
      "endDate_mech")

  data_mechs_periods <-
    dplyr::left_join(data_mechs_periods,
                     mechanism_map,
                     by = "attributeOptionCombo")

  data_mechs_periods <- data_mechs_periods %>%
    dplyr::mutate(is_valid = (startDate >= startDate_mech &
                                endDate <= endDate_mech)) %>%
    dplyr::mutate(is_valid = ifelse(is.na(is_valid), FALSE, is_valid)) %>%
    dplyr::filter(!is_valid)


  if (NROW(data_mechs_periods) > 0) {

    warning("Invalid mechanisms found!")

    if (return_violations) {
      return(data_mechs_periods)
    }

  } else {
    return(TRUE)
  }

}
