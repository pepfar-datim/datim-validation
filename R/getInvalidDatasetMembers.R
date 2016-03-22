#' @export
#' @title getInvalidDatasetMembers(data,base.url,username,password,organisationUnit,dataset)
#' 
#' @description Returns a data frame of invalid data element /organisation unit combinations
#' based on the current data set assignments
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param dataset Should be a description of the dataset group, such as "MER Results", "MER Targets" or "SIMS"
#' @param organisationUnit Should be the UID of the organisation unit ancestor.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the data is considered to be invalid.

getDataElementsOrgunits<-function(base.url,username,password,organisationUnit,dataset) {
  #Only support MER Results and targets for now. 
  valid.datasets<-c("MER Targets","MER Results","SIMS")
  if (!(dataset %in% valid.datasets)) {print("Not a valid dataset group"); stop()}
  #Datasets
  url<-URLencode(paste0(base.url,"api/dataSets?filter=name:like:",dataset,"&fields=id,name"))
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  ds<-getCachedObject(sig)
  if(is.null(ds)){
  r<-httr::GET(url , httr::authenticate(username,password),httr::timeout(60))
  r<- httr::content(r, "text")
  ds<-jsonlite::fromJSON(r,flatten=TRUE)$dataSets$id
  saveCachedObject(ds,sig)
  }
  url1<-paste0(base.url,"api/organisationUnits?fields=id&paging=false&filter=path:like:",organisationUnit,"&filter=dataSets.id:in:[",paste(ds,sep="",collapse=","),"]")
  url2<-paste0(base.url,"api/dataElements?fields=id&paging=false&filter=dataSets.id:in:[",paste(ds,sep="",collapse=","),"]")
  sig<-digest::digest(paste(url1,url2,sep="",collapse=""),algo='md5', serialize = FALSE)
  des_ous<-getCachedObject(sig)
  if (is.null(des_ous)){
      #DES
      r<-httr::GET(url1 , httr::authenticate(username,password),httr::timeout(60))
      r<- httr::content(r, "text")
      ous<-jsonlite::fromJSON(r,flatten=TRUE)$organisationUnits$id
      #OUs
      r<-httr::GET(url2, httr::authenticate(username,password),httr::timeout(60))
      r<- httr::content(r, "text")
      des<-jsonlite::fromJSON(r,flatten=TRUE)$dataElements$id
      des_ous<-list(ous=ous,des=des)
      saveCachedObject(des_ous,sig)
    }
  
  return(des_ous) }


getInvalidOrgunitsFromDatasets<-function(data,base.url,username,password,organisationUnit,dataset) {
  des_ous<-getDataElementsOrgunits(base.url,username,password,organisationUnit,dataset)
  return(subset(data,!(orgUnit %in% des_ous$ous)))
  }

getInvalidDataElementFromDatasets<-function(data,base.url,username,password,organisationUnit,dataset) {
  des_ous<-getDataElementsOrgunits(base.url,username,password,organisationUnit,dataset)
  subset(data,!(dataElement %in% des_ous$des))
}

getInvalidDatasetMembers<-function(data,base.url,username,password,organisationUnit,dataset){
  foo=data.frame(dataElement=character(),period=character(),orgUnit=character(),categoryOptionCombo=character(),
                 attributeOptionCombo=character(),value=character(),type=character())
  ous<-getInvalidOrgunitsFromDatasets(data,base.url,username,password,organisationUnit,dataset)
  if (!is.null(ous) & nrow(ous) > 0) {
    ous$type="ORGUNIT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,ous)
  }
  
  des<-getInvalidDataElementFromDatasets(data,base.url,username,password,organisationUnit,dataset)
  if (!is.null(des) & nrow(des) > 0) {
    des$type="DATAELEMENT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,des)
  }
  
  return(foo)
}
  