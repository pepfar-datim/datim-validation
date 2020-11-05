#' @export
#' @title getCategoryOptionCombosMap()
#' 
#' @description Utility function to produce a map of category option combos
#' @param HTTP handle returned from DHISLogin.
#' 
#' @return Returns a data frame  of code,name,id and shortName of all categoryOptionCombos
#' @examples \dontrun{
#' coc_map<-getCategoryOptionCombosMap()
#' }

getCategoryOptionCombosMap<-function( creds ) {
  
  url<-URLencode(paste0(creds$baseurl,"api/",api_version(),"/categoryOptionCombos?fields=id,name,shortName,code&paging=false"))
  r<-httr::GET(url,
               httr::timeout(300), 
               handle = creds$handle)
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    cocs<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
    return( cocs ) } else {
      print(paste("Could not retreive category option combos map",httr::content(r,"text")))
      stop()
    }
  
  
}
