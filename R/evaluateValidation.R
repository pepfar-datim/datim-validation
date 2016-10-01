#' @export
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
  matches$leftSide.expression<-plyr::mapvalues(matches$leftSide.expression,combis,values,warn_missing=FALSE)
  matches$ls.count<-stringr::str_count(matches$leftSide.expression,expression.pattern)
  
  matches$rightSide.expression<-plyr::mapvalues(matches$rightSide.expression,combis,values,warn_missing=FALSE)
  matches$rs.count<-stringr::str_count(matches$rightSide.expression,expression.pattern)
  #Remove rules which should not be evaluated
  foo<-!(matches$leftSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$ls.ops != matches$ls.count)) | 
    !(matches$rightSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$rs.ops != matches$rs.count)) | 
    matches$rightSide.missingValueStrategy== "NEVER_SKIP" |
    matches$leftSide.missingValueStrategy == "NEVER_SKIP"
  matches<-matches[foo,]
  
  matches$leftSide.expression<-gsub(expression.pattern,"0",matches$leftSide.expression) 
  matches$rightSide.expression<-gsub(expression.pattern,"0",matches$rightSide.expression)
  matches$leftSide.expression<-sapply(matches$leftSide.expression,function(x) {eval(parse(text=x))})
  matches$rightSide.expression<-sapply(matches$rightSide.expression,function(x) {eval(parse(text=x))})
  matches$formula<-paste(matches$leftSide.expression,matches$operator,matches$rightSide.expression) 
  matches$result<-vapply(matches$formula,function(x) {eval(parse(text=x))},FUN.VALUE=logical(1)) 
  if (return_violations_only == TRUE) {matches<-matches[!matches$result,]}
  return(matches)
}
