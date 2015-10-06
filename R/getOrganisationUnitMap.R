#' @title getOrganisationUnitMap(base.url,username,passowrd,operatingUnit)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param organisationUnit of question specified as a UID
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' 
getOrganisationUnitMap<-function(base.url,username,password,organisationUnit) {
  
  r<-httr::GET(URLencode(paste0(base.url,"api/organisationUnits.json?&filter=path:like:",organisationUnit,"&fields=id,code,name,shortName&paging=false")),
             httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    sites<-jsonlite::fromJSON(r,flatten=TRUE)[[1]] 
    return( sites ) } else {
      print(paste("Could not retreive site listing",httr::content(r,"text")))
      stop()
    }
  
  
}