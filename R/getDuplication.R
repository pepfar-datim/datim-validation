#' @export
#' @title getExactDuplicates(data)
#' 
#' @description Returns a listing of exact duplicates by dataElement,
#' period,orgunit,categoryOptionCombo and attributeOptionCombo
#' 
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of d2 data, with exact duplicates.
#'   A warning will be issued if duplicates are detected. 
#' 
#' @examples \dontrun{
#'  d<-d2Parser("myfile.csv",type="csv",header=TRUE)
#'  dups<-getExactDuplicates(d)
#'  dups
#'  }
getExactDuplicates <- function(d) {
  dups <- d %>%
    dplyr::group_by(.,
                    dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo,
                    attributeOptionCombo) %>%
    dplyr::summarise(count = dplyr::n()) %>% dplyr::filter(count > 1)
  if (NROW(dups) > 0) {
    warning("Your data contains exact duplicates!")
    
  }
  return(dups)
}

#' @export
#' @title getPureDuplicates(data)
#' 
#' @description Returns a listing of pure duplicates by dataElement,period,orgunit,
#' categoryOptionCombo. 
#' For the purposes of de-duplication, these are considered to be pure 
#' duplicates (only the attributeOptionCombo differ). 
#' 
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of d2 data, with pure duplicates used for de-duplication purposes.
#' @examples \dontrun{
#'     d<-d2Parser("myfile.csv",type="csv",header=TRUE)
#'     dups<-getPureDuplicates(d)
#'     dups
#'  }
#'  
getPureDuplicates<-function(d){

  d %>%
    dplyr::group_by(dataElement,
                    period,
                    orgUnit,
                    categoryOptionCombo) %>%
    dplyr::summarise(count = dplyr::n()) %>% dplyr::filter(count > 1)
}

#' @export
#' @title getCrosswalkMap()
#' 
#' @description Utility function which returns a map of DSD / TA data elements UIDs
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a map of DSD/TA data element UIDs.
#' @examples \dontrun{
#'     cw_map<-getCrosswalkMap()
#'  }
#' 
getCrosswalkMap<-function(d2session = d2_default_session){
  
  r<-httr::GET(URLencode(paste0(d2session$base_url,"api/sqlViews/UTlIicJBZFg/data.json&paging=false")),httr::timeout(300), handle = d2session$handle)
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
#' @title getCrosswalkMechanism(d2session = d2_default_session)
#' 
#' @description Utility function which returns the crosswalk mechanism UID
#'
#' @inheritParams datim_validation_params
#' 
#' @return Returns a UID of the crosswalk de-duplication mechanism
#' @examples \dontrun{
#'     cw_uid<-getCrosswalkMechanism()
#'  }
#' 
#' 
getCrosswalkMechanism<-function(d2session = d2_default_session){
  r<-httr::GET(URLencode(paste0(d2session$base_url,"api/categoryOptionCombos?filter=name:like:00001")),httr::timeout(300),handle = d2session$handle)
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
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of all crosswalk mechanism values,
#'  along with default de-deudplication adjustments.
#' @examples \dontrun{
#'     cw_dups<-getCrossWalkDuplicates(d)
#'  }
#' 
#'
getCrossWalkDuplicates<-function(d,d2session = d2_default_session){
  cw<-getCrosswalkMap(d2session = d2session)
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
  dupes$crosswalk_mechanism<-getCrosswalkMechanism(d2session = d2session)
  dupes$auto_resolve<-mapply(dsd.ta.agg,as.numeric(dupes$value_dsd),as.numeric(dupes$value_ta))
  return(dupes)
}