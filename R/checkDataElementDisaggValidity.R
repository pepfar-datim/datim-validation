#' @export
#' @title checkDataElementDisaggValidity(data,dataset)
#'
#' @description Utility function to produce a data frame of
#' invalid data elements based on current
#' DATIM form specification. 
#'
#' @param data D2 compliant data frame object
#' @param datasets Should be a character vector of data set UIDs. 
#' Alternatively, if left missing, user will be promted to choose from a list.
#' @param return_violations Boolean to return violations only. 
#' @param d2session datimutils d2session object
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName",
#' "dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data, if any. 
#' If there are no violations, a boolean TRUE is returned. 
#' @examples \dontrun{
#'   d<-d2Parser("myfile.csv",type="csv")
#'   ds<-getCurrentMERDataSets(type="RESULTS")
#'   checkDataElementDisaggValidity(d,ds)
#' }
checkDataElementDisaggValidity<-function(data,datasets=NA, return_violations=TRUE, d2session = d2_default_session ){
  
  des<-getValidDataElements(datasets, d2session = d2session) %>%
    dplyr::select(dataElement = dataelementuid,categoryOptionCombo=categoryoptioncombouid)

  data_des_cocs_bad<-dplyr::anti_join(data,des,by=c("dataElement","categoryOptionCombo"))
  
  if (NROW(data_des_cocs_bad) > 0) {
    
    warning("Invalid data element / category option combos found!")
    if (return_violations) {return(data_des_cocs_bad)}
    
  } else {
    return(TRUE)
  }
  
}