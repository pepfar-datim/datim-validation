#' @export
#' @title checkNegativeValues(data)
#'
#' @description In general, data values which are negative will not be imported. 
#' 
#' @param data D2 compliant data frame object
#' @param creds DHISLogin object
#' @return A filtered D2 compliant data frame of invalid values, otherwise, 
#' returns TRUE if there are no violations. A warning will also be issued if 
#' there are any negative values found in non-dedupe mechanisms. 
#'
#' @examples \dontrun{
#'  d<-d2Parser("myfile.csv",type="csv")
#'  checkNegativeValues(data=d, creds = my_creds)
#' }
#'
checkNegativeValues<-function(data,creds){
  
  numeric_types<-c("INTEGER","INTEGER_ZERO_OR_POSITIVE","PERCENTAGE","NUMBER","INTEGER_POSITIVE")
  dedupe_mechs<-c("X8hrDf6bLDC","YGT1o7UxfFu")
  
  
  des_numeric<-getDataElementMap(creds = creds) %>%
    dplyr::filter( valueType %in% numeric_types ) %>%
    dplyr::pull(id)
  
  value_is_negative<-grepl("^-",stringr::str_trim(data$value))
  mech_is_dedupe<-data$attributeOptionCombo %in% dedupe_mechs
  is_invalid<-value_is_negative & !mech_is_dedupe
  
  d_bad<-data[is_invalid,]
  
  if (NROW(d_bad) > 0) {
    
    warning("Negative values found in non-dedupe mechanisms!")
    d_bad
    
  } else {
    return(TRUE)
  }
  
}