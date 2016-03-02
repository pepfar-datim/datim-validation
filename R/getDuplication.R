#' @title getExactDuplicates(data)
#' 
#' @description Returns a listing of exact duplicates by dataElement,period,orgunit,categoryOptionCombo and attributeOptionCombo
#' 
#' @param d A d2 parsed data frame
#' @return Returns a data frame of d2 data, with exact duplicates.
getExactDuplicates<-function(d){
  foo<-duplicated(d[,c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo")])
  foo<-unique(d[foo,c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo")])
  foo<-merge(d,foo,by=c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo"))
  return(foo)
}


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


#' @title getCrosswalkMap(base.url,username,password)
#' 
#' @description Utility function which returns a map of DSD / TA data elements UIDs
#' 
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a map of DSD/TA data element UIDs.
#' 
getCrosswalkMap<-function(base.url,username,password){
  
  r<-httr::GET(URLencode(paste0(base.url,"api/sqlViews/UTlIicJBZFg/data.json&paging=false")), httr::authenticate(username,password),httr::timeout(60))
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

#' @title getCrosswalkMechanism(base.url,username,password)
#' 
#' @description Utility function which returns the crosswalk mechanism UID
#' 
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a UID of the crosswalk de-duplication mechanism
#' 
getCrosswalkMechanism<-function(base.url,username,password){
  r<-httr::GET(URLencode(paste0(base.url,"api/categoryOptionCombos?filter=name:like:00001")), httr::authenticate(username,password),httr::timeout(60))
  if (r$status == 200L ){
    r<- httr::content(r, "text")
    r<-jsonlite::fromJSON(r,flatten=TRUE)
    return(r$categoryOptionCombos$id) } else {
      print(paste("Could not retreive crosswalk mechanism ID",httr::content(r,"text")))
      stop() }
  
}

#' @title getCrossWalkDuplicates(d,base.url,username,password)
#' 
#' @description Utility function which returns the crosswalk mechanism UID
#' 
#' @param d A d2 parsed data frame of data.
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @return Returns a data frame of all crosswalk mechanism values, along with default de-deudplication adjustments.
#'
getCrossWalkDuplicates<-function(d,base.url,username,password){
  cw<-getCrosswalkMap(base.url,username,password)
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
  dupes$crosswalk_mechanism<-getCrosswalkMechanism(base.url,username,password)
  dupes$auto_resolve<-mapply(dsd.ta.agg,as.numeric(dupes$value_dsd),as.numeric(dupes$value_ta))
  return(dupes)
}