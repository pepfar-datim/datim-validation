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
                                 rightSide.count=integer(),formula=character(),result=logical(),stringsAsFactors = FALSE)
  
  des_list <- vapply(combis, function(x) {
      unlist(strsplit(x, "[.]"))[[1]]
    }, FUN.VALUE = character(1)) %>% 
    unique(.)
   
  vr_in <- function(x,table ) {
    foo <- pmatch(x,table)
    ifelse(is.na(foo),FALSE,TRUE)
  }
  
  matches <- vr %>% dplyr::mutate(left_in = vr_in(leftSide.expression,des_list),
                right_in = vr_in(rightSide.expression,des_list) ) %>%
    dplyr::filter(left_in | right_in) %>%
    dplyr::select(-left_in,-right_in)

  
  #Empty data frame
  if (nrow(matches) == 0 ) {return(validation.results_empty)}
  
  #Get the matching rules
  matches$leftSide.expression<-plyr::mapvalues(matches$leftSide.expression,combis,values,warn_missing=FALSE)
  matches$leftSide.count<-stringr::str_count(matches$leftSide.expression,expression.pattern)
  matches$leftSide.count<-matches$leftSide.ops-matches$leftSide.count
  matches$rightSide.expression<-plyr::mapvalues(matches$rightSide.expression,combis,values,warn_missing=FALSE)
  matches$rightSide.count<-stringr::str_count(matches$rightSide.expression,expression.pattern)
  matches$rightSide.count<-matches$rightSide.ops-matches$rightSide.count
  #Skip rules which should  be evaluated
  
  skip_when<-function(strategy,count,ops) {
    should_skip <- FALSE
    
    if (strategy == "NEVER_SKIP") {should_skip <- FALSE}
    
    if (strategy == "SKIP_IF_ANY_VALUE_MISSING") {
      should_skip <- ops == count
    }
    
    if (strategy == "SKIP_IF_ALL_VALUES_MISSING") {
      should_skip <- count == 0
    }
    
    should_skip
    
  }
  
  matches <- matches %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      skip_these_rules_right =  Map(
        f = skip_when,
        strategy = rightSide.missingValueStrategy,
        count = rightSide.count,
        ops = rightSide.ops
      ),
      skip_these_rules_left =  Map(
        f = skip_when,
        strategy = leftSide.missingValueStrategy,
        count = leftSide.count,
        ops = leftSide.ops
      )) %>%
    dplyr::mutate(skip_these_rules = skip_these_rules_right | skip_these_rules_left) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!skip_these_rules) %>%
    dplyr::select(-skip_these_rules,-skip_these_rules_right,-skip_these_rules_left)
      
      
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
  
  if (nrow(matches_normal > 0)) {
    
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
  
  
  
  if ( nrow(matches_normal) > 0 | nrow(matches_normal) > 0 ) {
    matches <- rbind(matches_normal, matches_ex)
    matches$result<-vapply(matches$formula,function(x) {eval(parse(text=x))},FUN.VALUE=logical(1)) }
    if (return_violations_only ) { matches<-matches[!matches$result,] } 
    matches
   } else {
    validation.results_empty
  }

}
