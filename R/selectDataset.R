#' @export
#' @title selectDataSet()
#' 
#' @description Utility function to produce a vector of data set UIDs from user input.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName",
#' "dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#' @examples \dontrun{
#'   ds<-selectDataset()
#' }
selectDataset<-function(d2session = d2_default_session) {
  ds<-getDataSets(d2session = d2session)
  promptText<-paste0("Please select the dataset [1-",nrow(ds),"]:")
  print(promptText)
  acr<-select.list(ds$name,multiple=TRUE)
   ds<-ds[ds$name %in% acr,"id"] 
  return(ds)
}