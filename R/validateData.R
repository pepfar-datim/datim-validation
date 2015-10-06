#' @title Function which checks the of a DATIM data payload agaist define validation rules
#' 
#' @description validateData should be supplied a d2Parser compliant data frame.
#'The data frame is checked dynamically against validation rules defined in the DATIM server.
#' @param data d2Parser data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param return_violations_only Paramater to return only violations or all validation rule evalualtions.
#' @param parallel Should the rules be evaluated in parallel. 
#' @return Returns a data frame with validation rule results.
validateData<-function(data,base.url,username,password,return_violations_only=TRUE,parallel=TRUE) {

#Calculate the totals  and append to the data frame
data$combi<-paste0(data$dataElement,".",data$categoryOptionCombo)
data.totals<-aggregate(value ~ dataElement + period + orgUnit + attributeOptionCombo, data = data,FUN=sum)
data.totals$combi<-data.totals$dataElement
data.totals$categoryOptionCombo<-NA
data.totals<-data.totals[,names(data)]
data<-rbind(data,data.totals)

#Check the data against the validation rules
vr<-getValidationRules(base.url,username,password)
if (Sys.info()[['sysname']] == "Windows" & parallel == TRUE ) {warning("Parallel execution may not be supported on Windows")}

validation.results<-plyr::ddply(data,plyr::.(period,attributeOptionCombo,orgUnit), function(x) evaluateValidation(x$combi,x$value,vr),.parallel=parallel)

#Remap the OUs
validation.results$orgUnit<-remapOUs(validation.results$orgUnit,base.url,username,password,organisationUnit,mode_in="id",mode_out="code")
#Remap the mechanisms
validation.results$attributeOptionCombo<-remapMechs(validation.results$attributeOptionCombo,base.url,username,password,organisationUnit,mode_in="id",mode_out="code")

if ( return_violations_only == TRUE & !is.na(return_violations_only) ) {
  
return( validation.results[!validation.results$result,] )

} else { 
  
return  (validation.results )

}
}
