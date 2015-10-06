#' @title getInvalidDataElements(data,base.url,username,password,dataset)
#' 
#' @description Utility function to produce a data frame of invalid data elements based on current
#' DATIM form specification
#'
#' @param data D2 compliant data frame object 
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param dataset Should be a description of the dataset group, such as "MER Results"
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#'
getInvalidDataElements<-function(data,base.url,username,password,dataset){
  #Valid data set assignments against the dataset
  r<-httr::GET(URLencode(paste0(base.url,"api/sqlViews/bkJ3PteNu7A/data.json?paging=false")),
         httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
  r<- httr::content(r, "text")
  r<- jsonlite::fromJSON(r)
  des<-as.data.frame(r$rows)
  foo<-r$header
  names(des)<-as.character(foo$name) } else {print("Could not get valid data elements"); stop()}
  #Datasets
  
  r<-httr::GET(URLencode(paste0(base.url,"api/dataSets?filter=name:like:",dataset,"&fields=id,name")),
                    httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
  r<- httr::content(r, "text")
  ds<-jsonlite::fromJSON(r)$dataSets

  des.this<-des[des$dataset %in% ds$name,]
  des.this$combi<-paste0(des.this$dataelementuid,".",des.this$categoryoptioncombouid)
  des.this<-plyr::colwise(as.character)(des.this)
  
  foo<-unique(data[,c("dataElement","categoryOptionCombo")])
  foo$combi<-paste0(foo$dataElement,".",foo$categoryOptionCombo)
  foo<-foo[!(foo$combi %in% des.this$combi ),]
  foo<-foo[complete.cases(foo),]
  #Get all data element names and uids
  foo$dataElementName<-plyr::mapvalues(as.character(foo$dataElement),as.character(des.this$dataelementuid),as.character(des.this$shortname),warn_missing=FALSE)
  foo$categoryOptionComboName<-plyr::mapvalues(foo$categoryOptionCombo,des.this$categoryoptioncombouid,as.character(des.this$categoryoptioncombo),warn_missing=FALSE)
  return(foo[,c("dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo")]) } else {print("Could not get datasets");break()}
}