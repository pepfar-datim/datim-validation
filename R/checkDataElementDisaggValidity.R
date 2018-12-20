#' @export
#' @title checkDataElementDisaggValidity(data,dataset)
#' 
#' @description Utility function to produce a data frame of invalid data elements based on current
#' DATIM form specification
#'
#' @param data D2 compliant data frame object 
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#'
checkDataElementDisaggValidity<-function(data,datasets=NA, return_violations=TRUE){
  
  des<-getValidDataElements(datasets) %>%
    dplyr::select(dataElement= dataelementuid,categoryOptionCombo=categoryoptioncombouid)

  data_des_cocs_bad<-dplyr::anti_join(data,des,by=c("dataElement","categoryOptionCombo"))
  
  if (NROW(data_des_cocs_bad) > 0) {
    
    warning("Invalid data element / category option combos found!")
    if (return_violations) {return(data_des_cocs_bad)}
    
  } else {
    return(TRUE)
  }
  
}