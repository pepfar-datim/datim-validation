#' @export
#' @title selectDataSet()
#' 
#' @description Utility function to produce a vector of data set UIDs from user input. 
#' @param creds DHISLogin object
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName",
#' "dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#' @examples \dontrun{
#'   ds<-selectDataset()
#' }
selectDataset<-function(creds) {
  ds<-getDataSets(creds = creds)
  promptText<-paste0("Please select the dataset [1-",nrow(ds),"]:")
  print(promptText)
  acr<-select.list(ds$name,multiple=TRUE)
   ds<-ds[ds$name %in% acr,"id"] 
  return(ds)
}