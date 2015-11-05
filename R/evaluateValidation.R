#' @title Utility function for evaluating validation rules 
#' 
#' @description evaluationValidation will accept a vector of combis (data elements.category optioncombinations) and values
#' and evaluate it against the supplied validation rules
#' 
#'
#' @param combis Data elements and category option combinations
#' @param values Values
#' @param vr Validation rule object
#' @param return_violations_only Only return validation rule violations
#' @return Returns a data frame of validation rule evaluations
evaluateValidation<-function(combis,values,vr,return_violations_only=TRUE) {
  expression.pattern<-"[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?"
  this.des<-vapply(combis,function(x){unlist(strsplit(x,"[.]"))[[1]]},FUN.VALUE=character(1))
  #Get the matching rules to apply
  matches <-vr[ grepl(paste(this.des,collapse="|"), vr$ls) | grepl(paste(this.des,collapse="|"), vr$rs),]
  
  #Get the matching rules
  matches$ls<-plyr::mapvalues(matches$ls,combis,values,warn_missing=FALSE)
  matches$ls.count<-stringr::str_count(matches$ls,expression.pattern)
  
  matches$rs<-plyr::mapvalues(matches$rs,combis,values,warn_missing=FALSE)
  matches$rs.count<-stringr::str_count(matches$rs,expression.pattern)
  #Remove rules which should not be evaluated
  foo<-!(matches$ls.strategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$ls.ops != matches$ls.count)) | 
    !(matches$rs.strategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$rs.ops != matches$rs.count)) | 
    matches$rs.strategy == "NEVER_SKIP" |
    matches$ls.strategy == "NEVER_SKIP"
  matches<-matches[foo,]
  
  matches$ls<-gsub(expression.pattern,"0",matches$ls) 
  matches$rs<-gsub(expression.pattern,"0",matches$rs)
  matches$ls<-sapply(matches$ls,function(x) {eval(parse(text=x))})
  matches$rs<-sapply(matches$rs,function(x) {eval(parse(text=x))})
  matches$formula<-paste(matches$ls,matches$op,matches$rs) 
  matches$result<-vapply(matches$formula,function(x) {eval(parse(text=x))},FUN.VALUE=logical(1)) 
  if (return_violations_only == TRUE) {matches<-matches[!matches$result,]}
  return(matches)
}