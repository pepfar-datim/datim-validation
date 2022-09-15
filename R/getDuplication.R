#' @export
#' @title Get Exact Duplicates
#'
#' @description Returns a listing of exact duplicates by dataElement,
#' period,orgunit,categoryOptionCombo and attributeOptionCombo
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of d2 data, with exact duplicates.
#'   A warning will be issued if duplicates are detected.
#'
#' @examples \dontrun{
#'  d <- d2Parser("myfile.csv",type="csv",header=TRUE)
#'  dups <- getExactDuplicates(d)
#'  dups
#'  }
getExactDuplicates <- function(d) {
  dups <- d$data$import %>%
    dplyr::group_by(.,
                    dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(count > 1)

  if (NROW(dups) > 0) {
    d$tests$exact_duplicates <- dups
    msg <- paste("ERROR! Your data contains", NROW(dups), "exact duplicates!")
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
  }

  d
}

#' @export
#' @title Get Pure Duplicates
#'
#' @description Returns a listing of pure duplicates by dataElement, period,
#' orgunit, categoryOptionCombo. For the purposes of de-duplication, these are
#' considered to be pure duplicates (only the attributeOptionCombo differ).
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of d2 data, with pure duplicates used for
#' de-duplication purposes.
#' @examples \dontrun{
#'     d <- d2Parser("myfile.csv",type="csv",header=TRUE)
#'     dups <- getPureDuplicates(d)
#'     dups
#'  }
#'
getPureDuplicates <- function(d) {

  pure_dupes <- d$data$import %>%
    dplyr::group_by(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo) %>%
    dplyr::summarise(count = dplyr::n()) %>%
    dplyr::filter(count > 1)

  if (NROW(pure_dupes) > 0) {

    msg <- paste(NROW(pure_dupes),"combinations requiring duplication found.")
    d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
  }

  d
}

#' @export
#' @title Get Crosswalk Map
#'
#' @description Utility function which returns a map of
#' DSD / TA data elements UIDs
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a map of DSD/TA data element UIDs.
#' @examples \dontrun{
#'     cw_map <- getCrosswalkMap()
#'  }
#'
getCrosswalkMap <- function(d2session = dynGet("d2_default_session",
                                               inherits = TRUE)) {

  r <-
    httpcache::GET(
      utils::URLencode(
        paste0(d2session$base_url,
               "api/sqlViews/UTlIicJBZFg/data.json&paging=false")),
      timeout = getHTTPTimeout(),
      handle = d2session$handle)
  if (r$status == 200L) {
    r <- httr::content(r, "text")
    r <- jsonlite::fromJSON(r, flatten = TRUE)
    cw <- as.data.frame(r$rows, stringsAsFactors = FALSE)
    names(cw) <- r$header$name
    return(cw[, c("dsd_de_uid", "ta_de_uid")])
  } else {
    print(paste("Could not retrieve crosswalk map", httr::content(r, "text")))
    stop()
  }
}

#' @export
#' @title Get Crosswalk Mechanism
#'
#' @description Utility function which returns the crosswalk mechanism UID
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a UID of the crosswalk de-duplication mechanism
#' @examples \dontrun{
#'     cw_uid <- getCrosswalkMechanism()
#'  }
#'
getCrosswalkMechanism <- function(d2session = dynGet("d2_default_session",
                                                     inherits = TRUE)) {
  r <-
    httpcache::GET(
      utils::URLencode(
        paste0(d2session$base_url,
               "api/categoryOptionCombos?filter=name:like:00001")),
      timeout = getHTTPTimeout(),
      handle = d2session$handle)
  if (r$status == 200L) {
    r <- httr::content(r, "text")
    r <- jsonlite::fromJSON(r, flatten = TRUE)
    return(r$categoryOptionCombos$id)
  } else {
    print(paste("Could not retreive crosswalk mechanism ID",
                httr::content(r, "text")))
    stop()
  }
}

#' @export
#' @title Get Crosswalk Duplicates
#'
#' @description Utility function which returns the crosswalk mechanism UID
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of all crosswalk mechanism values,
#'  along with default de-deudplication adjustments.
#' @examples \dontrun{
#'     cw_dups <- getCrossWalkDuplicates(d)
#'  }
#'
#'
getCrossWalkDuplicates <- function(d,
                                 d2session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  cw <- getCrosswalkMap(d2session = d2session)
  #TA values
  ta <- merge(d$data$import, cw, by.x = "dataElement", by.y = "ta_de_uid")
  ta <- ta[, -which(names(ta) %in% c("dsd_de_uid"))]
  names(ta)[which(names(ta) %in% c("value"))] <- "value_ta"
  names(ta)[which(names(ta) %in% c("attributeOptionCombo"))] <- "attributeOptionCombo_ta" #nolint
  #DSD Values
  dsd <- merge(d$data$import, cw, by.x = "dataElement", by.y = "dsd_de_uid")
  #Swtich the DSD data element
  names(dsd)[which(names(dsd) %in% c("value"))] <- "value_dsd"
  names(dsd)[which(names(dsd) %in% c("attributeOptionCombo"))] <- "attributeOptionCombo_dsd" #nolint
  names(dsd)[which(names(dsd) %in% c("dataElement"))] <- "dataElement_dsd"
  names(dsd)[which(names(dsd) %in% c("ta_de_uid"))] <- "dataElement"

  dupes <- merge(ta, dsd, by = c("dataElement",
                                 "period",
                                 "orgUnit",
                                 "categoryOptionCombo"))

  dsd.ta.agg <- function(x, y) { ifelse(y >= x, -x, -y) } #nolint
  dupes$crosswalk_mechanism <- getCrosswalkMechanism(d2session = d2session)
  dupes$auto_resolve <- mapply(dsd.ta.agg,
                               as.numeric(dupes$value_dsd),
                               as.numeric(dupes$value_ta))
  d$tests$crosswalk_duplicates <- dupes

}
