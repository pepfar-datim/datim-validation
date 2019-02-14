#' @export
#' @title checkMechanismPeriodValidity(data,organisationUnit)
#' 
#' @description This function will return an object of invalid mechanisms and periods. 
#' All data which is reported 
#' must have a period within the valid start and end dates of the
#' attribute option combination to which it is assigned. 
#' The mechanism must also be associated with the operating unit. 
#' If either of these two conditions 
#' are not met, the data will be flagged as being invalid. 
#' 
#' @param data A data frame which has been parsed by either d2Parser or sims2Parser
#' @param organisationUnit UID of the operating unit.
#' @param return_violations Should the function return a list of violations only. 
#' @return Returns a data frame of invalid period-mechanism combinations. A warning will 
#' also be issued. 
#' Returns TRUE if there are no violations. 
#' 
#' @examples 
#'  \dontrun{
#'  d<-d2Parser("myfile.csv",type="csv")
#'  ds<-getCurrentMERDataSets(type="RESULTS")
#'  checkMechanismValidity(data=d, organisationUnit = "f5RoebaDLMx")
#' }
#' 
checkMechanismValidity <- function(data, organisationUnit=NA, return_violations=TRUE) {
  
  if (is.na(organisationUnit)) {
    organisationUnit = getOption("organisationUnit")
  }
  
  data_mechs_periods <- data %>%
      dplyr::select(attributeOptionCombo,period) %>%
    dplyr::distinct()
  
  period_info <-
    do.call(rbind.data.frame, lapply(unique(data_mechs_periods$period), getPeriodFromISO))
  
  data_mechs_periods <- merge(data_mechs_periods, period_info, by.x = "period", by.y = "iso")
  
  mechanism_map <-
    getMechanismsMap(organisationUnit = organisationUnit)%>%
    dplyr::select(id,code,startDate,endDate)
  
  names(mechanism_map) <-
    c("attributeOptionCombo",
      "code",
      "startDate_mech",
      "endDate_mech")
  
  data_mechs_periods <-
    dplyr::left_join(data_mechs_periods, mechanism_map, by = "attributeOptionCombo")
  
  data_mechs_periods <-data_mechs_periods %>%
    dplyr::mutate(is_valid = (startDate >= startDate_mech & endDate <= endDate_mech)) %>%
    dplyr::mutate(is_valid = ifelse(is.na(is_valid),FALSE,is_valid)) %>%
    dplyr::filter(!is_valid) 
    

  if (NROW(data_mechs_periods) > 0) {
    
    warning("Invalid mechanisms found!")
    
    if (return_violations) {return(data_mechs_periods)}
    
    } else {
    return(TRUE)
  }
  
}