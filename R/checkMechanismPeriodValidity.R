#' @export
#' @title (checkMechanismPeriodValidity(data)
#' 
#' @description This function will return an object of invalid mechanisms and periods. All data which is reported 
#' must have a period within the valid start and end dates of the attribute option combination to which it is assigned.
#'
#' @param data A data frame which has been parsed by either d2Parser or sims2Parser
#' @return Returns a data frame of invalid period-mechanism combinations. 
#' 
#' 
checkMechanismPeriodValidity<-function(data) {
  mechs_periods<-unique(data[,c("attributeOptionCombo","period")])
  bar<-do.call(rbind.data.frame, lapply(unique(mechs_periods$period),getPeriodFromISO))
  mechs_periods<-merge(mechs_periods,bar,by.x="period",by.y="iso")
  baz<-getMechanismsMap()[,c("id","code","startDate","endDate")]
  names(baz)<-c("attributeOptionCombo","code","startDate_mech","endDate_mech")
  mechs_periods<-merge(mechs_periods,baz,by="attributeOptionCombo",all.x=T)
  mechs_periods$isValid<-mechs_periods$startDate >= mechs_periods$startDate_mech & mechs_periods$endDate <= mechs_periods$endDate_mech 
  return( mechs_periods[!mechs_periods$isValid,] ) }