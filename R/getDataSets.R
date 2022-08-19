#' @export
#' @title Get Data Sets
#'
#' @description Utility function to produce a data frame of datasets.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of name, id and formtype of all datasets.
#' @examples
#'  \dontrun{
#'  ds <- getDataSets()
#'  }
#'
getDataSets <- function(d2session = dynGet("d2_default_session",
                                           inherits = TRUE)) {
  url <-
    utils::URLencode(
      paste0(
        d2session$base_url,
        "api/",
        api_version(),
        "/dataSets?fields=name,id,formType&paging=false"
      )
    )
  sig <- digest::digest(paste0(url), algo = "md5", serialize = FALSE)
  ds <- getCachedObject(sig)
  if (is.null(ds)) {
    r <- httr::GET(url, httr::timeout(300), handle = d2session$handle)
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      r <- jsonlite::fromJSON(r)
      ds <- as.data.frame(r$dataSets)
      ds <- ds[with(ds, order(name)), ]
      saveCachedObject(ds, sig)
    } else {
      stop("Could not get a list of datasets")
    }
  }

  ds
}

#' @title Get Current MER Data Sets
#'
#' @param type Should be either RESULTS or TARGETS
#' @inheritParams datim_validation_params
#'
#' @return Returns a list of dataset UIDs of the given type
#' @export
#'
#' @examples
#'   \dontrun{
#'  ds <- getCurrentMERDataSets(type="RESULTS", d2session = d2session)
#'  }
#'
getCurrentMERDataSets <- function(type = "RESULTS",
                                d2session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {

  if (!(type %in% c("RESULTS", "TARGETS"))) {
    stop("Type must be either RESULTS or TARGETS")
    }
  ds <- getDataSets(d2session = d2session)

  if (type == "RESULTS") {
  want <-
    c(
      "MER Results: Facility Based",
      "MER Results: Community Based",
      "MER Results: Medical Store",
      "MER Results: Community Based - DoD ONLY",
      "MER Results: Facility Based - DoD ONLY",
      "Host Country Results: Facility (USG)"
    )
    } else if (type == "TARGETS") {
    want <- c(
      "Host Country Targets: COP Prioritization SNU (USG)",
      "MER Target Setting: PSNU (Facility and Community Combined)",
      "MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY"
    )
    } else {
      stop("Unknown dataset group. Must be one of RESULTS, TARGETS")
    }
 ds[ds$name %in% want, "id"]

}
