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

    r <- httpcache::GET(url, httr::timeout(getHTTPTimeout()), handle = d2session$handle)
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      r <- jsonlite::fromJSON(r)
      ds <- as.data.frame(r$dataSets)
      ds <- ds[with(ds, order(name)), ]
    } else {
      stop("Could not get a list of datasets")
    }
  ds
}

#' @title Get Current datasets provided a data stream
#'
#' @param datastream One of "RESULTS", "TARGETS", "NARRATIVES", "SIMS".
#' RESULTS and TARGETS refer to the current MER Results and targets datasets
#' NARRATIVES refer to the current MER Results Narratives
#' SIMS refer to the current SIMS datasets.
#' @inheritParams datim_validation_params
#'
#' @return Returns a list of dataset UIDs of the given type
#' @export
#'
#' @examples
#'   \dontrun{
#'  ds <- getCurrentDataSets(type="RESULTS", d2session = d2session)
#'  }
#'
getCurrentDataSets <- function(datastream = "RESULTS",
                                d2session = dynGet("d2_default_session",
                                                   inherits = TRUE)) {

  dataset_groups <- c("RESULTS", "TARGETS", "NARRATIVES", "SIMS")

  if (!(datastream %in% dataset_groups)) {
    stop(paste("Datastream must be one of", paste(dataset_groups, sep = "", collapse = ", ")))
    }

  ds <- getDataSets(d2session = d2session)


  mer_results <-   want <-
    c(
      "MER Results: Facility Based",
      "MER Results: Community Based",
      "MER Results: Medical Store",
      "MER Results: Community Based - DoD ONLY",
      "MER Results: Facility Based - DoD ONLY",
      "Host Country Results: Facility (USG)",
      "MER Results: Operating Unit Level (IM)",
      "Host Country Results: Operating Unit Level (USG)"
    )

  mer_targets <-  c(
    "Host Country Targets: COP Prioritization SNU (USG)",
    "MER Target Setting: PSNU (Facility and Community Combined)",
    "MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY"
  )

  mer_narratives <- c("Host Country Results: Narratives (USG)",
                      "MER Results: Narratives (IM)")

  sims <- c("SIMS 4: Above Site Based",
            "SIMS 4: Site Based")

  want <-  switch(datastream,
          "RESULTS" = mer_results,
          "TARGETS"  = mer_targets,
          "NARRATIVES" = mer_narratives,
          "SIMS" = sims
  )

  ds[ds$name %in% want, "id"]

}
