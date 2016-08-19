#' @export
#' @title getOrganisationUnitMap(operatingUnit)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param organisationUnit of question specified as a UID
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' 
getOrganisationUnitMap<-function(organisationUnit=NA) {
  if ( is.na(organisationUnit) ) { organisationUnit<-getOption("organisationUnit") }
  url<-URLencode(paste0(getOption("baseurl"),"api/organisationUnits.json?&filter=path:like:",organisationUnit,"&fields=id,code,name,shortName&paging=false"))
  sig<-digest::digest(url,algo='md5', serialize = FALSE)
  sites<-getCachedObject(sig)
  if (is.null(sites)){
  r<-httr::GET(url,httr::timeout(600))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    sites<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
    saveCachedObject(sites,sig)
     } else {
      print(paste("Could not retreive site listing",httr::content(r,"text")))
      stop()
    }
  }
  
  return( sites )
}