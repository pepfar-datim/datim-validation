#' @export
#' @title selectDataSet()
#' 
#' @description Utility function to produce a vector of data set UIDs from user input
#'
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
selectDataset<-function() {
  ds<-getDataSets()
  promptText<-paste0("Please select the dataset [1-",nrow(ds),"]:")
  print(promptText)
  acr<-select.list(ds$name,multiple=TRUE)
   ds<-ds[ds$name %in% acr,"id"] 
  return(ds)
}