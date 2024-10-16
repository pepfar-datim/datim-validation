#' @export
#' @title Get Period Info
#' @description Get information like start date, end date and period
#' type from an ISO period string
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of  ISO, startDate, endDate,and periodtype.
#'
getPeriodInfo <- function(ISO = NA,
                            d2session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

   path <- "sqlViews/TTM90ytCCdY/data.json?paging=false"

    r <- d2_api_get(path, d2session = d2session)

    if (!is.null(r)) {

        p <- as.data.frame(r$listGrid$rows, stringsAsFactors = FALSE)
        names(p) <- r$listGrid$headers$name
        p$enddate <- as.Date(p$enddate, "%Y-%m-%d")
        p$startdate <- as.Date(p$startdate, "%Y-%m-%d")

        if (!is.na(ISO)) {

          p <- p %>%
            dplyr::filter(iso %in% ISO)

        }

        return(p)

    } else {
      stop("Could not retreive period information")
    }

}
