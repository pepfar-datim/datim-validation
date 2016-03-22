#' @export
#' @title getAvailableDataSets(base.url,username,password)
#' 
#' @description Utility function to produce a data frame of valid data elements based on current
#' DATIM form specification
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame  of 
#'
getDataSets<-function(base.url,username,password) {
  url<-URLencode(paste0(base.url,"api/dataSets?fields=name,id,formType"))
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  ds<-getCachedObject(sig)
  if (is.null(ds)) {
    r<-httr::GET(url , httr::authenticate(username,password),httr::timeout(60))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      r<- jsonlite::fromJSON(r)
      ds<-as.data.frame(r$dataSets)
      ds<-ds[with(ds, order(name)), ]
      saveCachedObject(ds,sig)}
    
    else {print("Could not get valid data elements"); stop()}
  }
  
  return(ds) }