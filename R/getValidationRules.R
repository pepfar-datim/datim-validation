#' @title Utility function for getting validation rules dynamically from DHIS2 and parsing them into a data frame
#' 
#' @description d2Parser will parse a compliant DHIS2 XML,JSON or CSV file and transform it into a standard data
#' frame which can be used in subsequent DATIM validation routines
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame of validation rules consisting of name, left and right side operators and strategies

getValidationRules<-function(base.url,username,password) {
  
url<-paste0(base.url,"api/validationRules.xml?fields=id,name,description,leftSide[expression,missingValueStrategy],rightSide[expression,missingValueStrategy],operator,periodType&paging=false")
sig<-digest::digest(url,algo='md5', serialize = FALSE)
vr<-getCachedObject(sig)

if (is.null(vr)) {
expression.pattern<-"[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
#Get a copy of the metadata from the server
r<-httr::GET(url, httr::authenticate(username,password),httr::timeout(60))
r<- httr::content(r, "text")
vr.xml<-XML::xmlRoot(XML::xmlTreeParse(r))
vr.names<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule","o"),XML::xmlGetAttr,"name")
vr.op<-sapply(XML::getNodeSet(vr.xml,"//o:operator","o"),XML::xmlValue)
vr.ls<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule/o:leftSide/o:expression","o"),XML::xmlValue)
vr.rs<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule/o:rightSide/o:expression","o"),XML::xmlValue)
vr.ls.strategy<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule/o:leftSide/o:missingValueStrategy","o"),XML::xmlValue)
vr.rs.strategy<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule/o:rightSide/o:missingValueStrategy","o"),XML::xmlValue)
vr.periodType<-sapply(XML::getNodeSet(vr.xml,"//o:validationRule/o:periodType","o"),XML::xmlValue)
vr<-data.frame(name=vr.names,ls=vr.ls,op=vr.op,rs=vr.rs,ls.strategy=vr.ls.strategy,rs.strategy=vr.rs.strategy,periodType=vr.periodType)
#Static predefined map of operators
op.map<-data.frame(x=c("greater_than_or_equal_to","greater_than","equal_to","not_equal_to","less_than_or_equal_to","less_than"),
                   y=c(">=",">","==","!=","<=","<"),stringsAsFactors=F)
#Strategies
strat.map<-data.frame(x=c("SKIP_IF_ANY_VALUE_MISSING","SKIP_IF_ALL_VALUES_MISSING","NEVER_SKIP"))
#Remap the operators
vr$op<-plyr::mapvalues(vr$op,op.map$x,op.map$y,warn_missing=FALSE)
#Remove decorations
vr$ls<-gsub("[#{}]","",vr$ls)
vr$rs<-gsub("[#{}]","",vr$rs)
#Count the left and right side operators
vr$rs.ops<-stringr::str_count(vr$rs,expression.pattern)
vr$ls.ops<-stringr::str_count(vr$ls,expression.pattern)
#vr$rs.ops<-ifelse(vr$rs.ops==0,1,vr$rs.ops)
#vr$ls.ops<-ifelse(vr$ls.ops==0,1,vr$ls.ops)
saveCachedObject(vr,sig)
  }

return(vr) 
}