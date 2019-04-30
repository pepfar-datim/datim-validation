#' @export
#' @title getOrganisationUnitMap(operatingUnit)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param organisationUnit of question specified as a UID
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' @examples \dontrun{
#'     ou_map<-getOrganisationUnitMap("KKFzPM8LoXs") #For a specific operating unit
#'     ou_map_mine<-getOrganisationUnitMap() #Based on your login credentials
#' }
getOrganisationUnitMap<-function(organisationUnit=NA) {
  if ( is.na(organisationUnit) ) { organisationUnit<-getOption("organisationUnit") }
  url<-URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/organisationUnits.json?&filter=ancestors.id:in:[",organisationUnit,"]&fields=id,code,name,shortName&paging=false"))
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