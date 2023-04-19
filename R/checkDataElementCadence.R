#' @export
#' @title Check Data Element Cadence
#'
#' @description Utility function to produce a data frame of
#' invalid data value based on the so-called data element cadence.
#' Quarterly data elements can be submitted each quarter.
#' SAPR data elements should only be submiteted in FYQ2 and FYQ4.
#' APR data elements should only be submitted in FYQ4.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of "dataElement","period",
#' of invalid data elements which are present the the data, if any.
#' If there are no violations, a boolean TRUE is returned.
#' @examples \dontrun{
#'   d <- d2Parser("myfile.csv",type="csv")
#'   checkDataElementCadence(d)
#' }
checkDataElementCadence <- function(d,
                                    d2session = dynGet("d2_default_session",
                                                       inherits = TRUE)) {

  data <- d$data$import
  #Get a listing of periods which are present in the data
  des_periods <- unique(data[, c("period", "dataElement")])

  cadence_maps <-
    purrr::map_dfr(
      unique(des_periods$period),
      ~getDataElementCadenceMapForPeriod(., d2session = d2session))

  #Not exactly sure how to handle this right now.
  #This likely needs to be reimplemented with a message
  #Queue so that we can inform the user that there was
  #some problem with checking the cadence.
  if (NROW(cadence_maps) == 0) {
    msg <- "Could not get cadence maps for any periods."
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    return(d)
  }

  cadence_maps %<>%
      dplyr::select(period, dataElement = uid)


  data_des_periods_bad <- dplyr::anti_join(des_periods,
                                           cadence_maps,
                                           by = c("period", "dataElement"))

  if (NROW(data_des_periods_bad) > 0) {

    msg <- "ERROR! Invalid data element / period combinations found!"
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    d$tests$invalid_des_periods <- data_des_periods_bad

  } else {
    msg <- "No invalid data element/period combinations found."
    d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
  }

  d

}


getDataElementCadenceMapForPeriod <- function(period,
                                              d2session =
                                                dynGet("d2_default_session",
                                                       inherits = TRUE)) {
  url <-
    utils::URLencode(
      paste0(
        d2session$base_url,
        "api/",
        api_version(),
        "/dataStore/dataElementCadence/",
        period
      )
    )

    r <- httpcache::GET(url, httr::timeout(getHTTPTimeout()), handle = d2session$handle)
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      cadence_map_json <- jsonlite::fromJSON(r)
      if (length(cadence_map_json$dataElements) == 0L) {
        return(NULL)
      } else {
        cadence_map <- cadence_map_json$dataElements
        cadence_map$period <- cadence_map_json$period        
      }

    } else {
      warning("Could not retreive data element cadence map for period ", period)
    return(NULL)
    }

  cadence_map
}
