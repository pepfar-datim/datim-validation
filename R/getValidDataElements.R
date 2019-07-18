#' @export
#' @title getInvalidDataElements(datasets)
#' 
#' @description Utility function to produce a data frame of valid data elements based on current
#' DATIM form specification
#'
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame  of "dataSet","dataElementName","shortname","code","dataelementuid","categoryOptionComboName"
#' 
#'
getValidDataElements<-function(datasets=NA) {
  allDataSets<-getDataSets()
  dataSetValid<-Reduce("&",datasets %in% allDataSets$id)
  while( !dataSetValid || is.na(dataSetValid) ) {
    datasets<-selectDataset()
    if (length(datasets) == 0) {break;}
    dataSetValid <- Reduce("&",datasets %in% allDataSets$id) }
  if (length(datasets) == 0 || any(is.na(datasets))) { stop("Invalid dataset"); }
  #Valid data set assignments against the dataset
  #Custom forms
  des.all <-
    data.frame(
      dataset = character(),
      dataelement = character(),
      shortname = character(),
      code = character(),
      dataelementuid = character(),
      categoryoptioncombo = character(),
      categoryoptioncombouid = character()
    )
  
  
  for (i in seq_along(datasets)) {
    
    if ( allDataSets[allDataSets$id==datasets[i],"formType"] == "CUSTOM" ) {
      
      url<-URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/sqlViews/DotdxKrNZxG/data.json?var=dataSets:",datasets[i],"&paging=false"))
      
    } else { url<-URLencode(paste0(getOption("baseurl"),"api/", api_version(),"/sqlViews/ZC8oyMiZVQD/data.json?var=dataSets:",datasets[i],"&paging=false")) }
    
    sig<-digest::digest(paste0(url,datasets[i]),algo='md5', serialize = FALSE)
    des<-getCachedObject(sig)
    
    if (is.null(des)) {
      r<-httr::GET(url ,httr::timeout(60))
      if (r$status == 200L ){
        r<- httr::content(r, "text")
        r<- jsonlite::fromJSON(r)
        des<-as.data.frame(r$listGrid$rows,stringsAsFactors=FALSE)
        foo<-r$listGrid$header
        names(des)<-as.character(foo$name)
        #Select only the columns we are interested in
        des<-des[,names(des.all)]
        saveCachedObject(des,sig)
        if (nrow(des) > 0) { des.all<-rbind(des.all,des) }
      }
      
      else {print("Could not get valid data elements"); stop()}
      
    } else {  if (nrow(des) > 0) { des.all<-rbind(des.all,des) } }
    
  }
  
  return( plyr::colwise(as.character)(des.all) )
}
