#' @export
#' @title Utility function which provides a list of valid operating units
#' 
#' @description Utility function which provides a list of valid operating units
#' 
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame of operating units and their UIDs
#' @note
#' getValidOperatingUnits("https://www.datim.org","admin","district")
#' will remap organisation units specified as codes to UIDs
getValidOperatingUnits<-function() {
  base.url <- getOption("baseurl")
  r<-httr::GET(URLencode(paste0(base.url,"api/organisationUnits?level=3&fields=id,name&paging=false")),httr::timeout(60))
  if (r$status == 200 ) {
  r<- httr::content(r, "text")
  sites<-jsonlite::fromJSON(r,flatten=TRUE)$organisationUnits
  return(sites) } else { print(paste("Could not retreive valid operating units",httr::content(r$status))); stop() }
}
