#' @export
#' @title getInvalidDataElements(data,base.url,username,password,dataset)
#' 
#' @description Utility function to produce a data frame of valid data elements based on current
#' DATIM form specification
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param dataset Should be the UID of the dataset which you are validating. 
#' @return Returns a data frame  of "dataSet","dataElementName","shortname","code","dataelementuid","categoryOptionComboName"
#' 
#'
getValidDataElements<-function(base.url,username,password,dataset=NA) {
allDataSets<-getDataSets(base.url,username,password)
dataSetValid<-dataset %in% allDataSets$id 
while(!dataSetValid ) {
  dataset<-selectDataset(base.url,username,password)
  if (dataset == "") {break;}
  dataSetValid<- dataset %in% allDataSets$id }
if (dataset == "" || is.na(dataset)) { stop("Invalid dataset"); }
#Valid data set assignments against the dataset
#Custom forms
if ( allDataSets[allDataSets$id==dataset,"formType"] == "CUSTOM" ) {

  url<-URLencode(paste0(base.url,"api/sqlViews/DotdxKrNZxG/data.json?var=dataSets:",dataset,"&paging=false")) 

} else { url<-URLencode(paste0(base.url,"api/sqlViews/ZC8oyMiZVQD/data.json?var=dataSets:",dataset,"&paging=false")) }

sig<-digest::digest(paste0(url,dataset),algo='md5', serialize = FALSE)
des<-getCachedObject(sig)

if (is.null(des)) {
  r<-httr::GET(url , httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    r<- jsonlite::fromJSON(r)
    des<-as.data.frame(r$rows)
    foo<-r$header
    names(des)<-as.character(foo$name)
    saveCachedObject(des,sig)}
  
  else {print("Could not get valid data elements"); stop()}
}


 return(des)
}