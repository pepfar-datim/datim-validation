#' @title getMechanismsMap(base.url,username,passowrd)
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest)
#' 
getMechanismsMap<-function(base.url,username,password) {
  
  r<- r<-GET(URLencode(paste0(base.url,"/api/categoryOptions?filter=organisationUnits.id:eq:",organisationUnit,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
             authenticate(username,password),timeout(60))
  if (r$status == 200L ){
    r<- content(r, "text")
    des<-jsonlite::fromJSON(r,flatten=TRUE)[[1]] 
    return( des ) } else {
      print(paste("Could not retreive mechanisms",content(r,"text")))
      stop()
    }
  
  
}