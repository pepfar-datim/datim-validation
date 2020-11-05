#' @export
#' @title getDataSets(creds)
#' 
#' @description Utility function to produce a data frame of
#'  datasets. 
#'  @param creds DHISLogin object.
#' @return Returns a data frame  of name, id and formtype of all datasets.
#' @examples 
#'  \dontrun{
#'  ds<-getDataSets()
#'  }
#'
getDataSets <- function(creds) {
  url <-
    URLencode(
      paste0(
        creds$baseurl,
        "api/",
        api_version(),
        "/dataSets?fields=name,id,formType&paging=false"
      )
    )
  sig <- digest::digest(paste0(url), algo = 'md5', serialize = FALSE)
  ds <- getCachedObject(sig)
  if (is.null(ds)) {
    r <- httr::GET(url , httr::timeout(300), handle = creds$handle)
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



#' @title getCurrentMERDataSets(type)
#' 
#'
#' @param type Should be either RESULTS or TARGETS
#' 
#' @param creds DHISLogin object.  
#' @return Returns a list of dataset UIDs of the given type
#' @export
#'
#' @examples
#'   \dontrun{
#'  ds<-getCurrentMERDataSets(type="RESULTS", creds = creds)
#'  }
#'  
getCurrentMERDataSets<-function(type="RESULTS", creds) {
  
  if ( !(type %in% c("RESULTS","TARGETS"))) {stop("Type must be either RESULTS or TARGETS")}
  ds<-getDataSets(creds = creds)  
  
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
      "Host Country Targets: COP Prioritization SNU (USG)",
      "MER Target Setting: PSNU (Facility and Community Combined)",
      "MER Target Setting: PSNU (Facility and Community Combined) - DoD ONLY"
    )
    } else {
      stop("Unknown dataset group. Must be one of RESULTS, TARGETS")
    }
 ds[ds$name %in% want,"id"]
 
}

