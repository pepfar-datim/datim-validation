#' @export
#' @title getAvailableDataSets()
#' 
#' @description Utility function to produce a data frame of valid data elements based on current
#' DATIM form specification
#'
#' @return Returns a data frame  of 
#'
getDataSets<-function() {
  
  url<-URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/dataSets?fields=name,id,formType&paging=false"))
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  ds<-getCachedObject(sig)
  if (is.null(ds)) {
    r<-httr::GET(url ,httr::timeout(60))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      r<- jsonlite::fromJSON(r)
      ds<-as.data.frame(r$dataSets)
      ds<-ds[with(ds, order(name)), ]
      saveCachedObject(ds,sig)}
    
    else {print("Could not get a list of datasets"); stop()}
  }
  
  return(ds) }