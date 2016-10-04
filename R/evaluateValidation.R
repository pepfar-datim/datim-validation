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
  matches <-vr[ grepl(paste(this.des,collapse="|"), vr$leftSide.expression) | grepl(paste(this.des,collapse="|"), vr$rightSide.expression),]
  
  #Get the matching rules
  matches$leftSide.expression<-plyr::mapvalues(matches$leftSide.expression,combis,values,warn_missing=FALSE)
  matches$ls.count<-stringr::str_count(matches$leftSide.expression,expression.pattern)
  matches$rightSide.expression<-plyr::mapvalues(matches$rightSide.expression,combis,values,warn_missing=FALSE)
  matches$rs.count<-stringr::str_count(matches$rightSide.expression,expression.pattern)

  
  #Remove rules which should not be evaluated
  skip_these_rules<-(matches$leftSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$ls.ops != matches$ls.count)) | 
    (matches$rightSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & (matches$rs.ops != matches$rs.count)) | 
    !(matches$rightSide.missingValueStrategy== "NEVER_SKIP") |
    !(matches$leftSide.missingValueStrategy == "NEVER_SKIP") |
    ( ( matches$rs.count == matches$rs.ops )  & (matches$rightSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING" )) |
    ( ( matches$ls.count == matches$ls.ops )  & (matches$leftSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING" ))
  
  matches<-matches[!skip_these_rules,]
  
  if (nrow(matches) > 0 ) {
   #Special handling for logical operators
   matches_ex<-matches[matches$operator == "|",]
   #Do the sides have data?
   if ( nrow(matches_ex) > 0 )  {
   matches_ex$leftSide.expression<-matches_ex$ls.ops != matches_ex$ls.count
   matches_ex$rightSide.expression<-matches_ex$rs.ops != matches_ex$rs.count
   matches_ex$formula <- paste0("xor(",matches_ex$leftSide.expression,",",matches_ex$rightSide.expression,")") }
   
   matches_comp<-matches[matches$operator == "&",]
   #Do the sides have data?
   if (nrow(matches_comp) > 0 ) {
   matches_comp$leftSide.expression<-matches_comp$ls.ops != matches_comp$ls.count
   matches_comp$rightSide.expression<-matches_comp$rs.ops != matches_comp$rs.count
   matches_comp$formula == paste0(matches_comp$leftSide.expression,"&",matches_comp$rightSide.expression) }
  
  #Normal operators
  matches<-matches[!(matches$operator %in% c("|","&")),]
  if (nrow(matches > 0)) {
  matches$leftSide.expression<-gsub(expression.pattern,"0",matches$leftSide.expression) 
  matches$rightSide.expression<-gsub(expression.pattern,"0",matches$rightSide.expression)
  matches$leftSide.expression<-sapply(matches$leftSide.expression,function(x) {eval(parse(text=x))})
  matches$rightSide.expression<-sapply(matches$rightSide.expression,function(x) {eval(parse(text=x))})
  matches$formula<-paste(matches$leftSide.expression,matches$operator,matches$rightSide.expression) }
  matches<-rbind(matches_comp,matches_ex,matches)
  matches$result<-vapply(matches$formula,function(x) {eval(parse(text=x))},FUN.VALUE=logical(1)) 
  if (return_violations_only == TRUE) { matches<-matches[!matches$result,] } 
  return(matches) } else
  {return(NULL)}
}
