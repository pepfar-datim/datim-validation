#' @export
#' @title checkNegativeValues(data)
#'
#' @description In general, we should never accept data values
#' for numeric data elements which are negative, unless they are 
#' deduplication data, whose import is currently not supported. 
#'
#' @param data D2 compliant data frame object
#' @return A filtered D2 compliant data frame of invalid values, otherwise, returns NA. 
#'
checkNegativeValues<-function(data){
  
  numeric_types<-c("INTEGER","INTEGER_ZERO_OR_POSITIVE","PERCENTAGE","NUMBER","INTEGER_POSITIVE")
  dedupe_mechs<-c("X8hrDf6bLDC","YGT1o7UxfFu")
  
  
  des_numeric<-getDataElementMap() %>%
    dplyr::filter( valueType %in% numeric_types ) %>%
    dplyr::pull(id)
  
  value_is_negative<-grepl("^-",stringr::str_trim(data$value))
  mech_is_dedupe<-data$attributeOptionCombo %in% dedupe_mechs
  is_invalid<-value_is_negative & !mech_is_dedupe
  
  d_bad<-data[is_invalid,]
  
  if (NROW(d_bad) > 0) {
    
    warning("Negative values found in dedupe mechanisms!")
    d_bad
    
  } else {
    return(TRUE)
  }
  
}