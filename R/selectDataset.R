#' @export
#' @title selectDataSet(base.url,username,password)
#' 
#' @description Utility function to produce a vector of data set UIDs from user input
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
selectDataset<-function(base.url,username,password) {
  ds<-getDataSets(base.url,username,password)
  promptText<-paste0("Please select the dataset [1-",nrow(ds),"]:")
  print(promptText)
  acr<-select.list(ds$name,multiple=TRUE)
   ds<-ds[ds$name %in% acr,"id"] 
  return(ds)
}