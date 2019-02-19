#' @export
#' @title getCategoryOptionCombosMap()
#' 
#' @description Utility function to produce a map of category option combos
#'
#' @return Returns a data frame  of code,name,id and shortName of all categoryOptionCombos
#' @examples \dontrun{
#' coc_map<-getCategoryOptionCombosMap()
#' }

getCategoryOptionCombosMap<-function() {
  
  r<-httr::GET(URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/categoryOptionCombos?fields=id,name,shortName,code&paging=false")),
               httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    cocs<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
    return( cocs ) } else {
      print(paste("Could not retreive category option combos map",httr::content(r,"text")))
      stop()
    }
  
  
}
