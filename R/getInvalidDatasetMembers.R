#' @export
#' @title getDataElementsOrgunits(data,base.url,username,password,organisationUnit,datasets)
#' 
#' @description Returns a map of lists consisting of data elements and orgunits for a dataset (or datasets) for a given organisationUnit
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @param organisationUnit Should be the UID of the organisation unit ancestor.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the data is considered to be invalid.

getDataElementsOrgunits<-function(base.url,username,password,organisationUnit,datasets=NA) {
  if ( length(datasets) == 0 || is.na(datasets) ){
  allDataSets<-getDataSets(base.url,username,password) }
  
  dataSetValid<-Reduce("&",datasets %in% allDataSets$id)
  
  while(!dataSetValid || is.na(dataSetValid) ) {
    datasets<-selectDataset(base.url,username,password)
    if (length(datasets) == 0) {break;}
    dataSetValid <- Reduce("&",datasets %in% allDataSets$id) }
  
  if (length(datasets) == 0 || is.na(datasets)) { stop("Invalid dataset"); }
  
  sig<-digest::digest(paste(datasets,organisationUnit,sep="",collapse=""),algo='md5', serialize = FALSE)
  
  des_ous.all<-getCachedObject(sig)
  
  if (is.null(des_ous.all)){
    
      for (i in 1:length(datasets)) {
      if (i==1) { des_ous.all<-list() }
      url<-paste0(base.url,"api/organisationUnits?fields=id&paging=false&filter=path:like:",organisationUnit,"&filter=dataSets.id:eq:",datasets[i])
      r<-httr::GET(url, httr::authenticate(username,password),httr::timeout(60))
      r<- httr::content(r, "text")
      ous<-unique(jsonlite::fromJSON(r,flatten=TRUE)$organisationUnits$id)
      #OUs
      des<-unique(getValidDataElements(base.url,username,password,datasets[i])$dataelementuid)
      des_ous<-list(dataset=datasets[i],list(ous=ous,des=des)) 
      des_ous.all<-list.append(des_ous.all,des_ous)
      } 
  }

  return(des_ous.all) }


#' @export
#' @title getInvalidOrgunitsFromDatasets(data,base.url,username,password,organisationUnit,datasets)
#' 
#' @description Returns a data frame of invalid data element /organisation unit combinations
#' based on the current data set assignments
#' 
#' @param data D2 compliant data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @param organisationUnit Should be the UID of the organisation unit ancestor.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the data is considered to be invalid.
getInvalidOrgunitsFromDatasets<-function(data,base.url,username,password,organisationUnit,datasets) {
  des_ous<-getDataElementsOrgunits(base.url,username,password,organisationUnit,datasets)
  return(subset(data,!(orgUnit %in% des_ous$ous)))
  }

#' @export
#' @title getInvalidOrgunitsFromDatasets(data,base.url,username,password,organisationUnit,datasets)
#' 
#' @description Returns a data frame of invalid data element /organisation unit combinations
#' based on the current data set assignments
#'
#' @param data D2 compliant data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @param organisationUnit Should be the UID of the organisation unit ancestor.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the data is considered to be invalid.
getInvalidDataElementFromDatasets<-function(data,base.url,username,password,organisationUnit,datasets) {
  des_ous<-getDataElementsOrgunits(base.url,username,password,organisationUnit,datasets)
  subset(data,!(dataElement %in% des_ous$des))
}

getInvalidDatasetMembers<-function(data,base.url,username,password,organisationUnit,datasets=NA){
  allDataSets<-getDataSets(base.url,username,password)
  dataSetValid<-Reduce("&",datasets %in% allDataSets$id)
  while(!dataSetValid || is.na(dataSetValid) ) {
    datasets<-selectDataset(base.url,username,password)
    if (length(datasets) == 0) {break;}
    dataSetValid <- Reduce("&",datasets %in% allDataSets$id) }
  if (length(datasets) == 0 || is.na(datasets)) { stop("Invalid dataset"); }
  foo=data.frame(dataElement=character(),period=character(),orgUnit=character(),categoryOptionCombo=character(),
                 attributeOptionCombo=character(),value=character(),type=character())
  ous<-getInvalidOrgunitsFromDatasets(data,base.url,username,password,organisationUnit,datasets)
  if (!is.null(ous) & nrow(ous) > 0) {
    ous$type="ORGUNIT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,ous)
  }
  
  des<-getInvalidDataElementFromDatasets(data,base.url,username,password,organisationUnit,datasets)
  if (!is.null(des) & nrow(des) > 0) {
    des$type="DATAELEMENT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,des)
  }
  
  return(foo)
}
  
