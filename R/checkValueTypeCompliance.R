#' @export
#' @title checkValueTypeCompliancep(d)
#' 
#' @description Utility function of extraction of data element ids, codes, shortName and names
#'
#' @param d A D2Parsed data frame
#' @return Returns a data frame of invalid data only
#' 
checkValueTypeCompliance<-function(d) {
  
  #There are differences in the API version, so first, we need to know which version we are dealing with
  url<-URLencode(paste0(getOption("baseurl"),"api/system/info"))
  r<-httr::GET(url,httr::timeout(60))
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

  des<-getDataElementMap()
  des<-merge(des,patterns,by="valueType",all.x=T)
  d<-merge(d,des,by.x="dataElement",by.y="id")
  
  #Support only valueTypes with a regex
  foo<-d[!is.na(d$regex),]
  foo$isValidRegex<-mapply(grepl,foo$regex,as.character(foo$value))
  
  #ToDo: Stricter validation of options
  
  #Validation of zero significance
  foo$zeroSignificance<-!foo$zeroIsSignificant & foo$value == "0"
  #TODO: Remap data elements and so forth
  return(foo[( foo$isValidRegex==FALSE | !foo$zeroSignificance == FALSE ),])

}
