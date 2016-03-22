#' @export
#' @title getInvalidDataElements(data,base.url,username,password,dataset)
#' 
#' @description Utility function to produce a data frame of valid data elements based on current
#' DATIM form specification
#'
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param dataset Should be a description of the dataset group, such as "MER Results", "MER Targets" or "SIMS"
#' @return Returns a data frame  of "dataElementName","categoryOptionComboName","dataElement","categoryOptionCombo"
#' of invalid data elements which are present the the data 
#'
getValidDataElements<-function(base.url,username,password,dataset) {
#Only support MER Results and targets for now. 
valid.datasets<-c("MER Targets","MER Results","SIMS")
if (!(dataset %in% valid.datasets)) {print("Not a valid dataset group"); stop()}
#Valid data set assignments against the dataset
url<-URLencode(paste0(base.url,"api/sqlViews/bkJ3PteNu7A/data.json?paging=false"))
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

if (!is.null(des)) {
  url<-URLencode(paste0(base.url,"api/dataSets?fields=name,id&filter=name:like:",dataset))
r<-httr::GET(url,httr::authenticate(username,password))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      r<- jsonlite::fromJSON(r)
      ds<-as.data.frame(r$dataSets)
    names(ds)<-c("dataset","datasetuid")}
 else {print("Could not get a data set listing"); stop()}  
 des<-merge(des,ds,by="dataset") }

 return(des)
}