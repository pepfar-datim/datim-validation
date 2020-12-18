#' @export
#' @title checkDataElementCadence(data)
#'
#' @description Utility function to produce a data frame of
#' invalid data value based on the so-called data element cadence. 
#' Quarterly data elements can be submitted each quarter. 
#' SAPR data elements should only be submiteted in FYQ2 and FYQ4. 
#' APR data elements should only be submitted in FYQ4. 
#' 
#'
#' @param data D2 compliant data frame object
#' @param d2session datimutils d2session object
#' @return Returns a data frame  of "dataElement","period",
#' of invalid data elements which are present the the data, if any. 
#' If there are no violations, a boolean TRUE is returned. 
#' @examples \dontrun{
#'   d<-d2Parser("myfile.csv",type="csv")
#'   checkDataElementCadence(d)
#' }
checkDataElementCadence<-function(data,d2session = d2_default_session){
  
  
  #Get a listing of periods which are present in the data
  des_periods<-unique(data[,c("period","dataElement")])
  cadence_maps<-purrr::map_dfr(unique(des_periods$period),getDataElementCadenceMapForPeriod(.,d2session = d2session)) %>% 
    dplyr::select(period,dataElement=uid)
  
  data_des_periods_bad<-dplyr::anti_join(des_periods,cadence_maps,by=c("period","dataElement"))
  
  if (NROW(data_des_periods_bad) > 0) {
    
    warning("Invalid data element / period combinations found!")
    return(data_des_periods_bad)
    
  } else {
    return(TRUE)
  }
  
}


getDataElementCadenceMapForPeriod <- function(period,d2session = d2_default_session) {
  url <-
    URLencode(
      paste0(
        d2session$baseurl,
        "api/",
        api_version(),
        "/dataStore/dataElementCadence/",
        period
      )
    )
  sig <- digest::digest(url, algo = 'md5', serialize = FALSE)
  cadence_map <- getCachedObject(sig)
  if (is.null(cadence_map)) {
    r <- httr::GET(url , httr::timeout(300), handle = d2session$handle)
    if (r$status == 200L) {
      r <- httr::content(r, "text")
      cadence_map <- jsonlite::fromJSON(r)
      cadence_map_df<-cadence_map$dataElements
      cadence_map_df$period<-cadence_map$period
      saveCachedObject(cadence_map_df, sig)
    } else {
      stop("Could not retreive data element cadence map for period ",
           period)
    }
  }
  return(cadence_map_df)
}
  