#' @export
#' @title Utility function for evaluating validation rules 
#' 
#' @description evaluationValidation will accept a vector of combis (data elements.category optioncombinations) and values
#' and evaluate it against the supplied validation rules
#'
#' @param combis Data elements and category option combinations
#' @param values Values
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of validation rule evaluations
evaluateValidation<-function(combis,values,vr,return_violations_only=TRUE) {
  
  
  validation.results_empty<-data.frame(name=character(),id=character(),
                                 periodType=character(),description=character(),
                                 operator=character(),leftSide.expression=numeric(),
                                 leftSide.missingValueStrategy=character(),rightSide.expression=numeric(),
                                 rightSide.ops=integer(),leftSide.ops=integer(),leftSide.count=integer(),
                                 rightSide.count=integer(),formula=character(),result=logical())
  
  this.des <-
    vapply(combis, function(x) {
      unlist(strsplit(x, "\\."))[[1]]
    }, FUN.VALUE = character(1))
  
  #Calculate the totals for later use
  
  totals_df<-data.frame(exp = this.des,values=values,stringsAsFactors = FALSE) %>% 
    dplyr::group_by(exp) %>% 
    dplyr::summarise(values = sum(values)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(exp=paste0(exp,"}"))
  
  matches_vr_rule <- function(x) {
    
    agrepl(x, vr$leftSide.expression) |
      agrepl(x, vr$rightSide.expression)
  }
  
  this.des<-unique(this.des)  
  matches_v <- lapply(this.des,matches_vr_rule) %>% Reduce("|",.)
  matches <- vr[matches_v,]
  
  #Empty data frame
  if (nrow(matches) == 0) {return(validation.results_empty)}
  
  #TODO: Functionalize this for both sides to remove repetitive code

    values<-as.character(values)
  
    matches$leftSide.expression<-stringi::stri_replace_all_fixed(matches$leftSide.expression,
                           combis, values, vectorize_all=FALSE)

    matches$rightSide.expression<-stringi::stri_replace_all_fixed(matches$rightSide.expression,
                                                                  combis, values, 
                                                                  vectorize_all=FALSE)
    
    #Substitute totals
    matches$leftSide.expression<-stringi::stri_replace_all_fixed(matches$leftSide.expression,
                                                                 totals_df$exp, totals_df$values, vectorize_all=FALSE)
    
    matches$rightSide.expression<-stringi::stri_replace_all_fixed(matches$rightSide.expression,
                                                                  totals_df$exp, totals_df$values,
                                                                  vectorize_all=FALSE)
    
    #We should have exact matches now
    expression.pattern<-"#\\{[a-zA-Z][a-zA-Z0-9]{10}(\\.[a-zA-Z][a-zA-Z0-9]{10})?\\}"
    left_side_misses<-stringr::str_count(matches$leftSide.expression,expression.pattern)
    right_side_misses<-stringr::str_count(matches$rightSide.expression,expression.pattern)
    
    matches$leftSide.count<-matches$leftSide.ops - left_side_misses
    matches$rightSide.count<-matches$rightSide.ops- right_side_misses
    
    #We need to zero out anything with a category option combo at this point,
    #as there is no exact match
    matches$leftSide.expression <-
      gsub(expression.pattern, "0", matches$leftSide.expression)
    matches$rightSide.expression <-
      gsub(expression.pattern, "0", matches$rightSide.expression)
  

  #Keep rules which should  be evaluated
    keep_these_rules <- (
      (
        matches$leftSide.missingValueStrategy == "NEVER_SKIP" |
          (matches$leftSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & matches$leftSide.ops != matches$leftSide.count) |
          (matches$leftSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING" & matches$leftSide.count != 0)
      )
      &
        (
          matches$rightSide.missingValueStrategy == "NEVER_SKIP" |
            (matches$rightSide.missingValueStrategy == "SKIP_IF_ANY_VALUE_MISSING" & matches$rightSide.ops != matches$rightSide.count) |
            (matches$rightSide.missingValueStrategy == "SKIP_IF_ALL_VALUES_MISSING" & matches$rightSide.count != 0)
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
