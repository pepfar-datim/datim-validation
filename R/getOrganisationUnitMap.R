#' @title getOrgansationUnitMap(base.url,username,passowrd,operatingUnit)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param operatingUnit of question specified as a UID
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' 
getOrgansationUnitMap<-function(base.url,username,password,operatingUnit) {
  
  r<-GET(URLencode(paste0(base.url,"api/organisationUnits.json?&filter=path:like:",organisationUnit,"&fields=id,code,name,shortName&paging=false")),
             authenticate(username,password),timeout(60))
  if (r$status == 200L ){
    r<- content(r, "text")
    sites<-jsonlite::fromJSON(r,flatten=TRUE)[[1]] 
    return( sites ) } else {
      print(paste("Could not retreive site listing",content(r,"text")))
      stop()
    }
  
  
}