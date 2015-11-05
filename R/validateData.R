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
#' @param return_violations_only Only return violations
#' @param outputToFile Do not return an object, but rather output to a file directly
#' @return Returns a data frame with validation rule results.
validateData<-function(data,base.url,username,password,return_violations_only=TRUE,parallel=TRUE,outputToFile) {

data$value<-as.numeric(data$value) #This may throw a warning 
foo<-apply(apply(data,2,is.na),1,sum) == 0 #Filter out anything which is not complete.
data<-data[foo,]
#Calculate the totals  and append to the data frame
data$combi<-paste0(data$dataElement,".",data$categoryOptionCombo)
data.totals<-aggregate(value ~ dataElement + period + orgUnit + attributeOptionCombo, data = data,FUN=sum)
data.totals$combi<-data.totals$dataElement
data.totals$categoryOptionCombo<-NA
data.totals<-data.totals[,names(data)]
data<-rbind(data,data.totals)

#Check the data against the validation rules
vr<-getValidationRules(base.url,username,password)
if (Sys.info()[['sysname']] == "Windows" & parallel == TRUE ) { warning("Parallel execution may not be supported on Windows") }

validation.results<-plyr::ddply(data,plyr::.(period,attributeOptionCombo,orgUnit), function(x) evaluateValidation(x$combi,x$value,vr,return_violations_only,outputToFile),.parallel=parallel)

return(validation.results)

}
