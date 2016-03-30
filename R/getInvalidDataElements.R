#' @export
#' @title getInvalidDataElements(data,base.url,username,password,dataset)
#' 
#' @description Utility function to produce a data frame of invalid data elements based on current
#' DATIM form specification
#'
#' @param data D2 compliant data frame object 
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#'
getInvalidDataElements<-function(data,base.url,username,password,datasets=NA){
  
  des<-getValidDataElements(base.url,username,password,datasets)
  des$combi<-paste0(des$dataelementuid,".",des$categoryoptioncombouid)
  des<-plyr::colwise(as.character)(des)
  
  foo<-unique(data[,c("dataElement","categoryOptionCombo")])
  foo$combi<-paste0(foo$dataElement,".",foo$categoryOptionCombo)
  foo<-foo[!(foo$combi %in% des$combi ),]
  foo<-foo[complete.cases(foo),]
  #Get all data element names and uids
  foo$dataElementName<-remapDEs(foo$dataElement,base.url,username,password,mode_in="id",mode_out="shortName")
  foo$categoryOptionComboName<-remapCategoryOptionCombos(foo$categoryOptionCombo,base.url,username,password,mode_in="id",mode_out="shortName")
  return(foo[,c("dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo")])

}