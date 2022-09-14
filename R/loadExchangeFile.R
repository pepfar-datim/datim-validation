#' Title
#'
#' @param filename
#' @param type
#' @param datastream
#' @param organisationUnit
#' @param dataElementIdScheme
#' @param orgUnitIdScheme
#' @param idScheme
#' @param invalidData
#' @param hasHeader
#' @param isoPeriod
#' @param d2session
#'
#' @return
#' @export
#'
loadExchangeFile <- function(filename,
                             type,
                             datastream,
                             organisationUnit = NA,
                             dataElementIdScheme = "code",
                             orgUnitIdScheme = "id",
                             idScheme = "id",
                             invalidData = FALSE,
                             hasHeader = TRUE,
                             isoPeriod = NA,
                             d2session = dynGet("d2_default_session", inherits = TRUE)) {

  # d ----
  d <- list(
    info = list(
      filename = filename,
      type = type,
      dataElementIdScheme = dataElementIdScheme,
      orgUnitIdScheme = orgUnitIdScheme,
      idScheme = idScheme,
      invalidData = invalidData,
      hasHeader = hasHeader,
      isoPeriod = isoPeriod,
      messages = MessageQueue(),
      has_error = FALSE
  ))


  d$data$parsed <- d2Parser(   filename = filename,
                               type = type,
                               datastream = datastream,
                               organisationUnit = organisationUnit,
                               dataElementIdScheme = dataElementIdScheme,
                               orgUnitIdScheme = orgUnitIdScheme,
                               idScheme = idScheme,
                               invalidData = FALSE,
                               csv_header = TRUE,
                               d2session = d2session)

  if (datastream == "SIMS") {
    d <- shiftSIMSData(d)
  } else  {
    d$data$import <- d$data$parsed
  }

  d

}
