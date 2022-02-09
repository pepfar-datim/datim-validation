#' @export
#' @title Get Period Info
#' @description Get information like start date, end date and period type from an ISO period string
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of  ISO, startDate, endDate,and periodtype.
#'
getPeriodInfo <- function(ISO = NA,
                            d2session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

  url <- utils::URLencode(paste0(d2session$base_url, "api/", api_version(), "/sqlViews/TTM90ytCCdY/data.json"))
  sig <- digest::digest(paste0(url), algo = "md5", serialize = FALSE)
  p <- getCachedObject(sig)

  if (is.null(p)) {
    r <- httr::GET(url, httr::timeout(300), handle = d2session$handle)
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      r <- jsonlite::fromJSON(r)
      if (length(r$rows) > 0) {
        p <- as.data.frame(r$rows, stringsAsFactors = FALSE)
        names(p) <- r$headers$name
        p$enddate <- as.Date(p$enddate, "%Y-%m-%d")
        p$startdate <- as.Date(p$startdate, "%Y-%m-%d")
        saveCachedObject(p, sig)
      } else {
        stop(paste0("Period with ISO identifier", ISO, "not found"))
      }

    }
    else {
      stop("Could not retreive period information")
    }
  }
  if (!is.na(ISO)) {
    assertthat::assert_that(length(ISO) == 1)
    p <- p[p$iso == ISO, ]
  }
  return(p)
}
