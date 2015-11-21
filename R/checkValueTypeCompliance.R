#' @title checkValueTypeCompliancep(d,base.url,username,passowrd)
#' 
#' @description Utility function of extraction of data element ids, codes, shortName and names
#'
#' @param d Parsed data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a parsed data frame plus the valueType
#' 
checkValueTypeCompliance<-function(d,base.url,username,password) {
  
  
  #There are differences in the API version, so first, we need to know which version we are dealing with
  url<-URLencode(paste0(base.url,"api/system/info"))
  r<-httr::GET(url,httr::authenticate(username,password),httr::timeout(60))
  r<- httr::content(r, "text")
  sysInfo<-jsonlite::fromJSON(r,flatten=TRUE)
  version<-as.numeric(strsplit(sysInfo$version,"\\.")[[1]][2])
  if ( version < 21 ) { print("API version not supported. Aborting."); stop() }

  patterns<-
    list(NUMBER="^(-?[0-9]+)(\\.[0-9]+)?$",
         INTEGER="^(0|-?[1-9]\\d*)$",
         INTEGER_POSITIVE="^[1-9]\\d*$",
         INTEGER_ZERO_OR_POSITIVE="(^0$)|(^[1-9]\\d*$)",
         INTEGER_NEGATIVE = "^-[1-9]\\d*$",
         ZERO_PATTERN = "^0(\\.0*)?$",
         BOOLEAN="^(true|false)$",
         TRUE_ONLY="^true$",
         PERCENTAGE="^([0-9]|[1-9][0-9]|100)(\\.[0-9]+)?$",
         UNIT_INTERVAL="^(0(\\.[0-9]+)?)$|^1$",
         DATE="^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
         DATETIME="^(19|20)\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01]) (0[0-9]|1[0-9]|2[0-4]):([0-5][0-9]):([0-9][0-9])(\\.\\d{2,3})?$")
  
  patterns<-reshape2::melt(patterns)
  names(patterns)<-c("regex","valueType")    
  url<-URLencode(paste0(base.url,"api/dataElements?fields=id,valueType,optionSet[id],zeroIsSignificant&paging=false"))  
  sig<-digest::digest(url,algo='md5', serialize = FALSE)
  des<-getCachedObject(sig)
  
  if (is.null(des)){
    r<-httr::GET(url, httr::authenticate(username,password),httr::timeout(60))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      des<-jsonlite::fromJSON(r,flatten=TRUE)[[1]]
      des<-merge(des,patterns,by="valueType",all.x=T)
      saveCachedObject(des,sig)
    } else {
      print(paste("Could not retreive data elements",httr::content(r,"text")))
      stop()
      des<-NULL
    } 
  }
  
  d<-merge(d,des,by.x="dataElement",by.y="id")
  
  #Support only valueTypes with a regex
  foo<-d[!is.na(d$regex) & is.na(d$optionSet.id),]
  foo$isValidRegex<-mapply(grepl,foo$regex,as.character(foo$value))
  
  #ToDo: Validation of option sets
  #TODO: Validation of zero significance
  #TODO: Remap data elements and so forth
  return(foo[foo$isValidRegex==FALSE,c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value","valueType")])

}
