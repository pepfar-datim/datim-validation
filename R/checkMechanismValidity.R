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
#'      ds <- getCurrentDataSets(type="RESULTS")
#'      checkMechanismValidity(data=d, organisationUnit = "f5RoebaDLMx")
#' }
#'
checkMechanismValidity <- function(d,
                                   organisationUnit = NA,
                                   return_violations = TRUE,
                                   d2session = dynGet("d2_default_session",
                                                      inherits = TRUE)) {

  if (is.na(organisationUnit)) {
    organisationUnit <- d2session$user_orgunit
  }

  data_mechs_periods <- d$data$import %>%
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
    dplyr::select(id, code, startDate, endDate) %>%
    #Filter only mechanisms with star tdate end date
    #We will check the default mechanism elsewhere
    dplyr::filter(!is.na(startDate) & !is.na(endDate))

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

    msg <- paste("ERROR! The following invalid mechanisms found were found: ",
                 paste(data_mechs_periods$code), sep = "", collapse = ", ")
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    d$tests$invalid_mechanisms <- data_mechs_periods
  } else {
    msg <- paste("All mechanisms appear to be valid.",
                 paste(data_mechs_periods$code), sep = "", collapse = ", ")
    d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
  }

  d
}
