#' @title getMechanismsMap(base.url,username,passowrd)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param endDate Date which marks the mechanism as valid. Must be specified in a valid R date syntax
#' @param organisationUnit Operating unit in question specified as a UID
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' 
getMechanismsMap<-function(base.url,username,password,organisationUnit,endDate="2016-09-29") {
  
  r<-httr::GET(URLencode(paste0(base.url,"/api/categoryOptions?filter=organisationUnits.id:eq:",organisationUnit,"&fields=name,id,code,categoryOptionCombos[id],endDate&paging=false")),
             httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    mechs<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
	mechs$categoryOptionCombos<-unlist(mechs$categoryOptionCombos)
	#Convert the dates to booleans
	mechs$isValid<-( as.Date(mechs$endDate) >= as.Date( endDate ) )
    return( mechs ) } else {
      print(paste("Could not retreive mechanisms",httr::content(r,"text")))
      stop()
    }
  
  
}