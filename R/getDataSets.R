#' @export
#' @title getDataSets()
#' 
#' @description Utility function to produce a data frame of
#'  datasets. 
#'  
#' @return Returns a data frame  of name, id and formtype of all datasets.
#' @examples 
#'  \dontrun{
#'  ds<-getDataSets()
#'  }
#'
getDataSets <- function() {
  url <-
    URLencode(
      paste0(
        getOption("baseurl"),
        "api/",
        api_version(),
        "/dataSets?fields=name,id,formType&paging=false"
      )
    )
  sig <- digest::digest(paste0(url), algo = 'md5', serialize = FALSE)
  ds <- getCachedObject(sig)
  if (is.null(ds)) {
    r <- httr::GET(url , httr::timeout(60))
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      r <- jsonlite::fromJSON(r)
      ds <- as.data.frame(r$dataSets)
      ds <- ds[with(ds, order(name)),]
      saveCachedObject(ds, sig)
    }
    
    else {
      stop("Could not get a list of datasets")
    }
  }
  
  ds
}

#' @export
#' @title getCurrentMERDataSets(type)
#' 
#' @description Get a list of MER datasets which are currently open for data entry. 
#' @param type Should be either RESULTS or TARGETS
#' @return Returns a list of dataset UIDs of the given type.
#' @examples 
#'  \dontrun{
#'  ds<-getCurrentMERDataSets(type="RESULTS")
#'  }
getCurrentMERDataSets<-function(type="RESULTS") {
  
  if ( !(type %in% c("RESULTS","TARGETS"))) {stop("Type must be either RESULTS or TARGETS")}
  ds<-getDataSets()  
  
  if ( type == "RESULTS" ) {
  want <-
    c(
      "MER Results: Facility Based",
      "MER Results: Community Based",
      "MER Results: Medical Store",
      "MER Results: Community Based - DoD ONLY",
      "MER Results: Facility Based - DoD ONLY"
    ) } else if (type == "TARGETS") {
    want <- c(
      "MER Targets: Community Based",
      "MER Targets: Facility Based",
      "MER Targets: Community Based - DoD ONLY",
      "MER Targets: Facility Based - DoD ONLY"
    )
    } else {
      stop("Unknown dataset group. Must be one of RESULTS, TARGETS")
    }
 ds[ds$name %in% want,"id"]
 
}

