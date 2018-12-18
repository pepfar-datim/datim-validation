#' @export
#' @importFrom dplyr %>%
#' @title checkValueTypeCompliance(d)
#' 
#' @description Utility function of extraction of data element ids, codes, shortName and names
#'
#' @param d A D2Parsed data frame
#' @return Returns a data frame of invalid data only
#' 
checkValueTypeCompliance<-function(d) {
  
  #There are differences in the API version, so first, we need to know which version we are dealing with
  url<-URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/system/info"))
  r<-httr::GET(url,httr::timeout(60))
  r<- httr::content(r, "text")
  sysInfo<-jsonlite::fromJSON(r,flatten=TRUE)
  version<-as.numeric(strsplit(sysInfo$version,"\\.")[[1]][2])
  if ( version < 21 ) { print("API version not supported. Aborting."); stop() }

  patterns<-
    list(NUMBER="^(-?[0-9]+)(\\.[0-9]+)?$",
         INTEGER="^(0|-?[1-9]\\d*)$",
         INTEGER_POSITIVE="^[1-9]\\d*$",
         INTEGER_ZERO_OR_POSITIVE="(^0$)|(^[1-9]\\d*$)",
         INTEGER_NEGATIVE = "^-[1-9]\\d*$",
         ZERO_PATTERN = "^0(\\.0*)?$",
         BOOLEAN="^(true|false)$",
         TRUE_ONLY="^true$",
         PERCENTAGE="^([0-9]|[1-9][0-9]|100)(\\.[0-9]+)?$",
         UNIT_INTERVAL="^(0(\\.[0-9]+)?)$|^1$",
         DATE="^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
         DATETIME="^(19|20)\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01]) (0[0-9]|1[0-9]|2[0-4]):([0-5][0-9]):([0-9][0-9])(\\.\\d{2,3})?$")
  
  patterns<-reshape2::melt(patterns)
  names(patterns)<-c("regex","valueType")    

  des<-getDataElementMap()
  des<-merge(des,patterns,by="valueType",all.x=T)
  d<-merge(d,des,by.x="dataElement",by.y="id")
  
  #Support only valueTypes with a regex
  d_regex_validation<-d[!is.na(d$regex),]
  
  if (NROW(d_regex_validation) > 0)  {
  
  d_regex_validation$is_valid_pattern <-
    mapply(grepl,
           d_regex_validation$regex,
           as.character(d_regex_validation$value))
  
  #Validation of zero significance. If the value is zero and the the 
  #data elemement is not zero signficant, then flag it. 
  #TRUE on Non-zeros and valid zeros. False on invalid zeros
  d_regex_validation$is_valid_zero<- !(d_regex_validation$value == "0" & !d_regex_validation$zeroIsSignificant)
  d_regex_validation$is_valid_value <- d_regex_validation$is_valid_pattern  &  d_regex_validation$is_valid_zero
  d_regex_validation<- d_regex_validation %>%
    dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value,is_valid_value) %>%
    dplyr::filter(!is_valid_value) } else {
      d_regex_validation<-data.frame(dataElement=character(),
                                     period=character(),
                                     categoryOptionCombo=character(),
                                     attributeOptionCombo=character(),
                                     value=character(),
                                     is_valid_value=logical(),
                                     stringsAsFactors = FALSE)
    }
  #Deal with data of type option sets
  d_option_sets<-checkOptionSetCompliance(d)
  #Return anything which is not valid
  dplyr::bind_rows(d_regex_validation,d_option_sets)
}



#' @export
#' @title getOptionSetMap()
#' 
#' @description Utility function to fetch a list of optionsets and possible values within them
#'
#' @return Returns a list of option sets values per option set
#' 
getOptionSetMap<-function() {

  
  url<-URLencode(paste0(getOption("baseurl"),"api/",api_version(),"/optionSets?fields=id,name,options[code]&paging=false"))
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  option_sets<-getCachedObject(sig)
  if (is.null(option_sets)) {
    r<-httr::GET(url ,httr::timeout(60))
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      r<- jsonlite::fromJSON(r,flatten = TRUE)
      option_sets<-as.data.frame(r$optionSets)
      saveCachedObject(ds,sig)}
    
    else {print("Could not get a list of option sets"); stop()}
  }
  
  return(option_sets)
}


#' @export
#' @title checkOptionSetCompliance()
#' 
#' @param d D2Parsed data frame
#' @description Internal function for validation of data which have option sets
#'
#' @return Returns a data frame of invalid values validated against their option set.
#' 
#' 
checkOptionSetCompliance<-function(d) {
  
  option_sets_des<-getDataElementMap() %>% 
    dplyr::filter(!is.na(optionSet.id)) %>%
    dplyr::select(dataElement=id,optionSetID=optionSet.id) %>%
    dplyr::distinct()
  
  d <-  dplyr::inner_join(d,option_sets_des,by="dataElement") 
  
  if (NROW(d) == 0) {
    
    foo<-data.frame(dataElement=character(),
               period=character(),
               categoryOptionCombo=character(),
               attributeOptionCombo=character(),
               value=character(),
               is_valid_value=logical(),
               stringsAsFactors = FALSE)
    return(foo)
  }
  
  option_set_map<-getOptionSetMap()
  
  getOptionSetValues<-function(x) {
    list_index<-which(option_set_map$id == x)
    option_set_map[list_index,"options"][[1]] %>% dplyr::pull("code")
  }
  
  d$option_set_values_list<-lapply(d$optionSetID, getOptionSetValues)
  d$is_valid_value<-mapply(function(x,y) x %in% y, d$value,d$option_set_values_list)
  d %>% dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value,is_valid_value) %>%
    dplyr::filter(!is_valid_value)
    
}
