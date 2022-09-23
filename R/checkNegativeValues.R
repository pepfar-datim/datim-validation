#' @export
#' @title Check Negative Values
#'
#' @description In general, data values which are negative will not be imported.
#'
#' @inheritParams datim_validation_params
#'
#' @return A filtered D2 compliant data frame of invalid values, otherwise,
#' returns TRUE if there are no violations. A warning will also be issued if
#' there are any negative values found in non-dedupe mechanisms.
#'
#' @examples \dontrun{
#'  d <- d2Parser("myfile.csv",type="csv")
#'  d <- checkNegativeValues(d)
#' }
#'
checkNegativeValues <- function(d,
                                d2session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {

  numeric_types <- c("INTEGER",
                     "INTEGER_ZERO_OR_POSITIVE",
                     "PERCENTAGE",
                     "NUMBER",
                     "INTEGER_POSITIVE")
  dedupe_mechs <- c("X8hrDf6bLDC", "YGT1o7UxfFu")

  des_numeric <- getDataElementMap(d2session = d2session) %>%
    dplyr::filter(valueType %in% numeric_types) %>%
    dplyr::pull(id)

  data <- d$data$import %>%
    dplyr::filter(dataElement %in% des_numeric)

  if (NROW(data) == 0) {
    return(d)
  }

  value_is_negative <- grepl("^-", stringr::str_trim(data$value))
  mech_is_dedupe <- data$attributeOptionCombo %in% dedupe_mechs
  is_invalid <- value_is_negative & !mech_is_dedupe

  tests_negative_values <- data[is_invalid, ]

  if (NROW(tests_negative_values) > 0) {
    msg <- paste("ERROR!", NROW(tests_negative_values), "negative values found in non-dedupe mechanisms!")
    d$info$messages <-
      appendMessage(d$info$messages, msg, "ERROR")
    d$tests$negative_values <- tests_negative_values
  } else {
    msg <- "No negative values detected."
    d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
  }

  d

}
