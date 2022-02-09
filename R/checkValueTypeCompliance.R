#' @export
#' @importFrom dplyr %>%
#' @title checkValueTypeCompliance(d)
#' 
#' @description This function will check data to ensure that its value matches
#' a given pattern based on its value type. As an example, if the data element
#' which is being validated should only accept positive integers, any values
#' which are not positive integers will be flagged. For values which are composed
#' of option sets, any values which do not correspond so one of the option set 
#' value codes, will also be flagged. Additionally ,any values which are zero
#' for which the data element is not zero significant will also be flagged. 
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of invalid data only
#' @examples 
#' \dontrun{
#' d<-d2Parser("myfile.csv",type="csv")
#' checkValueTypeCompliance(d)
#' }
#' 
checkValueTypeCompliance<-function(d, d2session = d2_default_session) {
  
  #There are differences in the API version, so first, we need to know which version we are dealing with
  url<-URLencode(paste0(d2session$base_url,"api/",api_version(),"/system/info"))
  r<-httr::GET(url,httr::timeout(300),handle = d2session$handle)
  r<- httr::content(r, "text")


  patterns<-
    list(NUMBER="^(-?[0-9]+)(\\.[0-9]+)?$",
         INTEGER="^(0|-?[1-9]\\d*)$",
         INTEGER_POSITIVE="^[1-9]\\d*$",
         INTEGER_ZERO_OR_POSITIVE="(^0$)|(^[1-9]\\d*$)",
         INTEGER_NEGATIVE = "^-[1-9]\\d*$",
         ZERO_PATTERN = "^0(\\.0*)?$",
         BOOLEAN="^(true|false|True|False|0|1)$",
         TRUE_ONLY="^(true|True|1)$",
         PERCENTAGE="^([0-9]|[1-9][0-9]|100)(\\.[0-9]+)?$",
         UNIT_INTERVAL="^(0(\\.[0-9]+)?)$|^1$",
         DATE="^(19|20)\\d\\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$",
         DATETIME="^(19|20)\\d\\d-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01]) (0[0-9]|1[0-9]|2[0-4]):([0-5][0-9]):([0-9][0-9])(\\.\\d{2,3})?$")
  
  patterns<-reshape2::melt(patterns)
  names(patterns)<-c("regex","valueType")    

  des<-getDataElementMap(d2session = d2session)
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
  #data element is not zero significant, then flag it. 
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
  d_option_sets<-checkOptionSetCompliance(d, d2session = d2session)
  #Return anything which is not valid
  d<-dplyr::bind_rows(d_regex_validation,d_option_sets)
  if ( NROW(d) > 0 ) {
    d
  } else {
    TRUE
  }
  }



#' @export
#' @title getOptionSetMap()
#' 
#' @description Utility function to fetch a list of optionsets and possible values within them
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame structure composed of option set names, UIDs, and a data frame for 
#' each of these with individual options present in the option set.
#' @examples 
#'  \dontrun{
#'   op_set_map<-getOptionSetMap()
#' }
getOptionSetMap<-function(d2session = d2_default_session) {

  
  url<-URLencode(paste0(d2session$base_url,"api/",api_version(),"/optionSets?fields=id,name,options[code]&paging=false"))
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  option_sets<-getCachedObject(sig)
  if (is.null(option_sets)) {
    r<-httr::GET(url ,httr::timeout(300), handle = d2session$handle)
    if (r$status == 200L ){
      r<- httr::content(r, "text")
      r<- jsonlite::fromJSON(r,flatten = TRUE)
      option_sets<-as.data.frame(r$optionSets)
      saveCachedObject(option_sets,sig)}
    
    else {stop("Could not get a list of option sets")}
  }
  
  return(option_sets)
}


#' @export
#' @title checkOptionSetCompliance()
#'
#' @inheritParams datim_validation_params
#'
#' @description Internal function for validation of data which have option sets
#'
#' @return Returns a data frame of invalid values validated against their option set.
#' 
#' @examples 
#'  \dontrun{
#'   d<-d2Parser("myfile.csv",type="csv")
#'   checkOptionSetCompliance(d)
#' }
checkOptionSetCompliance<-function(d, d2session = d2_default_session) {
  
  option_sets_des<-getDataElementMap(d2session = d2session) %>% 
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
  
  option_set_map<-getOptionSetMap(d2session = d2session)
  
  getOptionSetValues<-function(x) {
    list_index<-which(option_set_map$id == x)
    option_set_map[list_index,"options"][[1]] %>% dplyr::pull("code")
  }
  
  d$option_set_values_list<-lapply(d$optionSetID, getOptionSetValues)
  d$is_valid_value<-mapply(function(x,y) x %in% y, d$value,d$option_set_values_list)
  d %>% dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value,is_valid_value) %>%
    dplyr::filter(!is_valid_value)
    
}
