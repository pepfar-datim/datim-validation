#' @export
#' @title getMechanismsMap()
#' 
#' @description Utility function to produce a map of valid mechanisms
#'
#' @param organisationUnit Operating unit in question specified as a UID. If not specified, assume to be Global.
#' @return Returns a data frame  of name,code,id, and categoryOptionCombo (which is the UID of interest) along with start and endDates of the mechanisms
#' 
getMechanismsMap<-function(organisationUnit=NA) {
  if ( is.na(organisationUnit) ) { 
    organisationUnit<-getOption("organisationUnit")
    warning( paste("No organisation unit specified. Using orgunit",organisationUnit) )
  }
  #Determine if we should filter by OU
  ou_filter= !( organisationUnit== "ybg3MO3hcf4"  )
  #Special cases when global OU is used, particularly for SIMS Import
  url<-paste0(getOption("baseurl"),"api/categoryOptionCombos?filter=categoryCombo.id:eq:wUpfppgjEza&fields=code,name,id,categoryOptions[startDate,endDate,organisationUnits[id]]&paging=false&filter=categoryOptions.organisationUnits.id:!null")
  if  ( ou_filter ){
    url<-paste0(url,paste0("&filter=categoryOptions.organisationUnits.id:eq:",organisationUnit)) 
  }
  url<-URLencode(url)
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  mechs<-getCachedObject(sig)
  if (!is.null(mechs))  { return(mechs) } else
  if(is.null(mechs)) {
  
  r<-httr::GET(url,httr::timeout(60))
  if (r$status == 200L ){
  r<- httr::content(r, "text")
  mechs<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
  
  foo<-mechs$categoryOptions
  n<-length(foo)
  mechs_details<-data.frame(matrix(as.character(""), nrow = n, ncol = 3),stringsAsFactors = FALSE)
  names(mechs_details)<-c("startDate","endDate","organisationUnits")
  for (i in 1:length(foo)) {
    bar<-foo[i]
    mechs_details$startDate[i]<- bar[[1]]$startDate[1]
    mechs_details$endDate[i]<- bar[[1]]$endDate[1]
    mechs_details$organisationUnits[i]<-bar[[1]]$organisationUnits[[1]]$id
  }
  
  #Need to unwind the dates
  mechs_details$startDate<-as.Date(sapply(mechs_details$startDate, function(x) ifelse(is.null(x),"1900-01-01",x)),"%Y-%m-%d")
  mechs_details$endDate<-as.Date(sapply(mechs_details$endDate, function(x) ifelse(is.null(x),"1900-01-01",x)),"%Y-%m-%d")
  mechs<-cbind(mechs[,c("code","name","id")],mechs_details)
  saveCachedObject(mechs,sig) }
  return( mechs ) } 
  
  else {
      print(paste("Could not retreive mechanisms",httr::content(r,"text")))
      stop()
    }
  
}