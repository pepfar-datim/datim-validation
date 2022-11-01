#' @export
#' @title Get Mechanisms Map
#'
#' @description Utility function to produce a map of valid mechanisms. If no
#' organisation unit is provided, assumed to be global.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo
#' (which is the UID of interest) along with start and endDates of
#' the mechanisms
#'
getMechanismsMap <- function(organisationUnit = NA,
                             include_default = TRUE,
                             d2session = dynGet("d2_default_session",
                                                inherits = TRUE)) {
  if (is.na(organisationUnit)) {
    organisationUnit <- d2session$user_orgunit
    # warning(paste("No organisation unit specified.",
    #               "Using orgunit", organisationUnit))
  }
  #Determine if we should filter by OU
  ou_filter <- !(organisationUnit == "ybg3MO3hcf4")
  #Special cases when global OU is used, particularly for SIMS Import
  url <- paste0(d2session$base_url, "api/", api_version(),
                "/categoryOptionCombos?filter=categoryCombo.id:eq:wUpfppgjEza&",
                "fields=code,name,id,categoryOptions[startDate,endDate]&",
                "paging=false")
  if (ou_filter) {
    url <- paste0(url,
                  paste0("&filter=categoryOptions.organisationUnits.id:eq:",
                         organisationUnit))
  }
  url <- utils::URLencode(url)

    r <- httpcache::GET(url, httr::timeout(getHTTPTimeout()), handle = d2session$handle)
    if (r$status_code == 200L) {
      r <- httr::content(r, "text")
      mechs <- jsonlite::fromJSON(r, flatten = TRUE)[[1]]
      if (length(mechs) == 0) {
        return(NULL)
      }

      min_start_date <- "1900-01-01"
      max_end_date <- "2099-12-31"

      #Need to unwind the dates
      mechs$startDate <-
        as.Date(sapply(mechs$categoryOptions,
          \(x) ifelse(is.null(x$startDate), min_start_date, x$startDate)),
          "%Y-%m-%d")
      mechs$endDate <-
        as.Date(sapply(mechs$categoryOptions,
          \(x) ifelse(is.null(x$endDate), max_end_date, x$endDate)),
          "%Y-%m-%d")
      mechs <- mechs[, -which(names(mechs) == "categoryOptions")]

      if (include_default) {
        #Add the default mech, if needed
        default_mech <- list(code = "default",
                             name = "default",
                             id = "HllvX50cXC0",
                             startDate = as.Date(min_start_date),
                             endDate = as.Date(max_end_date))
        mechs <- rbind(mechs, default_mech)
      }

    } else {
      stop(paste("Could not retreive mechanisms", httr::content(r, "text")))
    }

    mechs
}
