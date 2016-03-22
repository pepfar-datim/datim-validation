#' @export
#' @title getCategoryOptionCombosMap(base.url,username,passowrd)
#' 
#' @description Utility function to produce a map of category option combos
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame  of code,name,id and shortName of all categoryOptionCombos

getCategoryOptionCombosMap<-function(base.url,username,password) {
  
  r<-httr::GET(URLencode(paste0(base.url,"api/categoryOptionCombos?fields=id,name,shortName,code&paging=false")),
             httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    cocs<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
    return( cocs ) } else {
      print(paste("Could not retreive category option combos map",httr::content(r,"text")))
      stop()
    }
  
  
}
