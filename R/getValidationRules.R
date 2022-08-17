#' @export
#' @title Utility function for getting validation rules dynamically from DHIS2 and parsing them into a data frame
#'
#' @description d2Parser will parse a compliant DHIS2 XML,JSON or CSV file and transform it into a standard data
#' frame which can be used in subsequent DATIM validation routines
#' @param remove_decoration Remove decoration of curly braces present in the raw validation metadata.
#' @param d2session datimutils d2session object
#' @return Returns a data frame of validation rules consisting of name, left and right side operators and strategies

getValidationRules<-function(remove_decoration = FALSE, d2session = d2_default_session) {

url<-paste0(d2session$base_url,"api/",api_version(),"/validationRules.json?fields=id,name,description,leftSide[expression,missingValueStrategy],rightSide[expression,missingValueStrategy],operator,periodType&paging=false")
sig<-digest::digest(url,algo='md5', serialize = FALSE)
vr<-getCachedObject(sig)

if (is.null(vr)) {
expression.pattern<-"[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
#Get a copy of the metadata from the server
r<-httr::GET(url,httr::timeout(300), handle = d2session$handle)
r<- httr::content(r, "text")
vr<-jsonlite::fromJSON(r,flatten=TRUE)$validationRules
#Static predefined map of operators
op.map<-data.frame(x=c("greater_than_or_equal_to","greater_than","equal_to","not_equal_to","less_than_or_equal_to","less_than","exclusive_pair","compulsory_pair"),
                   y=c(">=",">","==","!=","<=","<","|","&"),stringsAsFactors=F)
#Strategies
strat.map<-data.frame(x=c("SKIP_IF_ANY_VALUE_MISSING","SKIP_IF_ALL_VALUES_MISSING","NEVER_SKIP"))
#Remap the operators
vr$operator<-plyr::mapvalues(vr$operator,op.map$x,op.map$y,warn_missing=FALSE)



#Count the left and right side operators
vr$rightSide.ops<-stringr::str_count(vr$rightSide.expression,expression.pattern)
vr$leftSide.ops<-stringr::str_count(vr$leftSide.expression,expression.pattern)

#Remove any line breaks

vr$leftSide.expression<-stringr::str_replace(vr$leftSide.expression,pattern = "\n","")
vr$rightSide.expression<-stringr::str_replace(vr$rightSide.expression,pattern = "\n","")

if (remove_decoration) {

  #Remove decorations
  vr$leftSide.expression <- gsub("[#{}]", "", vr$leftSide.expression)
  vr$rightSide.expression <- gsub("[#{}]", "", vr$rightSide.expression)

}

saveCachedObject(vr,sig)

}

return(vr)
}
