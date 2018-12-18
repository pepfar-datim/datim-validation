#' @export
#' @title getExactDuplicates(data)
#' 
#' @description Returns a listing of exact duplicates by dataElement,period,orgunit,categoryOptionCombo and attributeOptionCombo
#' 
#' @param d A d2 parsed data frame
#' @return Returns a data frame of d2 data, with exact duplicates.
getExactDuplicates<-function(d){
  dups<-d %>% 
    dplyr::group_by(.,dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo) %>% 
    dplyr::summarise(count=n()) %>% dplyr::filter(count>1)
  if (NROW(d) > 0 ) { warning("Your data contains exact duplicates!"); }
  return(dups)
}

#' @export
#' @title getPureDuplicates(data)
#' 
#' @description Returns a listing of pure duplicates by dataElement,period,orgunit,categoryOptionCombo. 
#' For the purposes of de-duplication, these are considered to be pure duplicates (only the attributeOptionCombo differ)
#' 
#' @param d A d2 parsed data frame
#' @return Returns a data frame of d2 data, with pure duplicates.
getPureDuplicates<-function(d){
  foo<-duplicated(d[,c("dataElement","period","orgUnit","categoryOptionCombo")])
  foo<-unique(d[foo,c("dataElement","period","orgUnit","categoryOptionCombo")])
  foo<-merge(d,foo,by=c("dataElement","period","orgUnit","categoryOptionCombo"))
  return(foo)
}

#' @export
#' @title getCrosswalkMap()
#' 
#' @description Utility function which returns a map of DSD / TA data elements UIDs
#'
#' @return Returns a map of DSD/TA data element UIDs.
#' 
getCrosswalkMap<-function(){
  
  r<-httr::GET(URLencode(paste0(getOption("baseurl"),"api/sqlViews/UTlIicJBZFg/data.json&paging=false")),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    r<-jsonlite::fromJSON(r,flatten=TRUE)
    cw<-as.data.frame(r$rows,stringsAsFactors=FALSE)
    names(cw)<-r$header$name
    return( cw[,c("dsd_de_uid","ta_de_uid")] ) } else {
      print(paste("Could not retrieve crosswalk map",httr::content(r,"text")))
      stop()
    }
}

#' @export
#' @title getCrosswalkMechanism()
#' 
#' @description Utility function which returns the crosswalk mechanism UID
#' 
#' @return Returns a UID of the crosswalk de-duplication mechanism
#' 
getCrosswalkMechanism<-function(){
  r<-httr::GET(URLencode(paste0(getOption("baseurl"),"api/categoryOptionCombos?filter=name:like:00001")),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    r<-jsonlite::fromJSON(r,flatten=TRUE)
    return(r$categoryOptionCombos$id) } else {
      print(paste("Could not retreive crosswalk mechanism ID",httr::content(r,"text")))
      stop() }
  
}

#' @export
#' @title getCrossWalkDuplicates(d)
#' 
#' @description Utility function which returns the crosswalk mechanism UID
#' 
#' @param d A d2 parsed data frame of data.
#' @return Returns a data frame of all crosswalk mechanism values, along with default de-deudplication adjustments.
#'
getCrossWalkDuplicates<-function(d){
  cw<-getCrosswalkMap()
  #TA values
  ta<-merge(d,cw,by.x="dataElement",by.y="ta_de_uid")
  ta<-ta[ , -which(names(ta) %in% c("dsd_de_uid"))]
  names(ta)[which(names(ta) %in% c("value"))]<-"value_ta"
  names(ta)[which(names(ta) %in% c("attributeOptionCombo"))]<-"attributeOptionCombo_ta"
  #DSD Values
  dsd<-merge(d,cw,by.x="dataElement",by.y="dsd_de_uid")
  #Swtich the DSD data element
  names(dsd)[which(names(dsd) %in% c("value"))]<-"value_dsd"
  names(dsd)[which(names(dsd) %in% c("attributeOptionCombo"))]<-"attributeOptionCombo_dsd"
  names(dsd)[which(names(dsd) %in% c("dataElement"))]<-"dataElement_dsd"
  names(dsd)[which(names(dsd) %in% c("ta_de_uid"))]<-"dataElement"
  
  dupes<-merge(ta,dsd,by=c("dataElement","period","orgUnit","categoryOptionCombo"))
  
  dsd.ta.agg<-function(x,y) { ifelse(y >= x,-x,-y) }
  dupes$crosswalk_mechanism<-getCrosswalkMechanism()
  dupes$auto_resolve<-mapply(dsd.ta.agg,as.numeric(dupes$value_dsd),as.numeric(dupes$value_ta))
  return(dupes)
}