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
  
  validation.results_empty<-data.frame(name=character(),id=character(),
                                 periodType=character(),description=character(),
                                 operator=character(),leftSide.expression=numeric(),
                                 leftSide.missingValueStrategy=character(),rightSide.expression=numeric(),
                                 rightSide.ops=integer(),leftSide.ops=integer(),leftSide.count=integer(),
                                 rightSide.count=integer(),formula=character(),result=logical())
  
  this.des <-
    unique(vapply(combis, function(x) {
      unlist(strsplit(x, "[\\.]"))[[1]]
    }, FUN.VALUE = character(1)))
  
  matches_vr_rule <- function(x) {
    agrepl(x, vr$leftSide.expression) |
      agrepl(x, vr$rightSide.expression)
  }
  
  matches_v <- lapply(this.des,matches_vr_rule) %>% Reduce("|",.)
  matches <- vr[matches_v,]
  
  #Empty data frame
  if (nrow(matches) == 0) {return(validation.results_empty)}
  
  values<-as.character(values)

    matches$leftSide.expression<-stringi::stri_replace_all_fixed(matches$leftSide.expression,
                           combis, values, vectorize_all=FALSE)

    matches$rightSide.expression<-stringi::stri_replace_all_fixed(matches$rightSide.expression,
                                                                 combis, values, vectorize_all=FALSE)

  matches$leftSide.count<-stringr::str_count(matches$leftSide.expression,expression.pattern)
  matches$leftSide.count<-matches$leftSide.ops-matches$leftSide.count

  matches$rightSide.count<-stringr::str_count(matches$rightSide.expression,expression.pattern)
  matches$rightSide.count<-matches$rightSide.ops-matches$rightSide.count
  #Keep rules which should  be evaluated
  keep_these_rules <-
    (
      matches$leftSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" &
        (matches$leftSide.ops != matches$leftSide.count)
    ) |
    (
      matches$rightSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" &
        (matches$rightSide.ops != matches$rightSide.count)
    ) |
    (matches$rightSide.missingValueStrategy == "NEVER_SKIP") |
    (matches$leftSide.missingValueStrategy == "NEVER_SKIP") |
    (
      (matches$rightSide.count == matches$rightSide.ops)  &
        (
          matches$rightSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING"
        )
    ) |
    (
      (matches$leftSide.count == matches$leftSide.ops)  &
        (
          matches$leftSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING"
        )
    )
  
  matches<-matches[keep_these_rules,]
  
  if (nrow(matches) > 0 ) {
   #Special handling for logical operators
   matches_ex<-matches[matches$operator %in% c("|","&"),]
   #Do the sides have data?
   if (nrow(matches_ex) > 0)  {
     matches_ex$leftSide.expression<-matches_ex$leftSide.count
     matches_ex$rightSide.expression<-matches_ex$rightSide.count
     matches_ex$formula <-
       paste0(
         as.character(matches_ex$leftSide.count == 0),
         matches_ex$operator,
         as.character(matches_ex$rightSide.count == 0)
       )
   }  else {
     matches_ex <- validation.results_empty
   }
   
  #Normal operators
  matches_normal<-matches[!(matches$operator %in% c("&","|")),]
  
  if ( nrow(matches_normal) > 0 ) {
    matches_normal$leftSide.expression <-
      gsub(expression.pattern, "0", matches_normal$leftSide.expression)
    matches_normal$rightSide.expression <-
      gsub(expression.pattern, "0", matches_normal$rightSide.expression)
    matches_normal$leftSide.expression <-
      sapply(matches_normal$leftSide.expression, function(x) {
        eval(parse(text = x))
      })
    matches_normal$rightSide.expression <-
      sapply(matches_normal$rightSide.expression, function(x) {
        eval(parse(text = x))
      })
    matches_normal$formula <-
      paste(
        matches_normal$leftSide.expression,
        matches_normal$operator,
        matches_normal$rightSide.expression
      ) } else {
        matches_normal <- validation.results_empty
      }
  
  matches <- rbind(matches_normal, matches_ex)
  
  
  matches$result<-vapply(matches$formula,function(x) {eval(parse(text=x))},FUN.VALUE=logical(1)) 
  if (return_violations_only == TRUE) { matches<-matches[!matches$result,] } 
  return( matches) } else
  { return(validation.results_empty) }
}
