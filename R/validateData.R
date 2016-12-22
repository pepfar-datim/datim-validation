#' @export
#' @title Function which checks the of a DATIM data payload agaist define validation rules
#' 
#' @description validateData should be supplied a d2Parser compliant data frame.
#'The data frame is checked dynamically against validation rules defined in the DATIM server.
#' @param data d2Parser data frame
#' @param organisationUnit Organisation unit. Defaults to the user organisation unit if not supplied.
#' @param return_violations_only Paramater to return only violations or all validation rule evalualtions.
#' @param parallel Should the rules be evaluated in parallel. 
#' @return Returns a data frame with validation rule results.
validateData<-function(data,organisationUnit=NA,return_violations_only=TRUE,parallel=TRUE) {
if ( is.na(organisationUnit) ) {organisationUnit = getOption("organisationUnit")}
if (nrow(data) == 0 || is.null(data) ) {stop("Data values cannot be empty!")}

  header <-
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value")
  
data <- data[, header[header %in% names(data)]]
invalid<-function(x) { sapply(x, function(x) {is.na(x) || missing(x) || x=="" })}  
data$value<-as.numeric(data$value) #This may throw a warning 
invalid.rows<-apply(apply(data,2,invalid),1,sum) == 0 #Filter out anything which is not complete.
if (sum(invalid.rows) != sum(invalid.rows)) {
  foo<-nrow(data)-sum(invalid.rows)
  msg<-paste(foo, " rows are invalid. Please check your data.")
  warning(msg)}
data<-data[invalid.rows,]
#Calculate the totals  and append to the data frame
data$combi<-paste0(data$dataElement,".",data$categoryOptionCombo)
data.totals<-aggregate(value ~ dataElement + period + orgUnit + attributeOptionCombo, data = data,FUN=sum)
data.totals$combi<-data.totals$dataElement
data.totals$categoryOptionCombo<-NA
data.totals<-data.totals[,names(data)]
data<-rbind(data,data.totals)
#Empty data frame
validation.results_empty<-data.frame(name=character(),id=character(),
                                     periodType=character(),description=character(),
                                     operator=character(),leftSide.expression=numeric(),
                                     leftSide.missingValueStrategy=character(),rightSide.expression=numeric(),
                                     rightSide.ops=integer(),leftSide.ops=integer(),leftSide.count=integer(),
                                     rightSide.count=integer(),formula=character(),result=logical())
validation.results<-validation.results_empty

#Check the data against the validation rules
vr<-getValidationRules()
if (Sys.info()[['sysname']] == "Windows"  ) { 
    if  ( parallel == TRUE )  {warning("Parallel execution is not supported on Windows" ); parallel = FALSE }
}

validation.results<-plyr::ddply(data,plyr::.(period,attributeOptionCombo,orgUnit),
                                    function(x) evaluateValidation(x$combi,x$value,vr,return_violations_only),
                                    .parallel=parallel,
                                    .inform=TRUE)

if ( nrow(validation.results) > 0 ) {
validation.results$orgUnit<-remapOUs(validation.results$orgUnit,
                                     organisationUnit,
                                     mode_in="id",mode_out="code")
validation.results$attributeOptionCombo<-remapMechs(validation.results$attributeOptionCombo,
                                                    organisationUnit,
                                                    mode_in="id",mode_out="code") 
validation.results<-plyr::colwise(as.character)(validation.results) 
return  ( validation.results ) 
} else
{
  return( validation.results_empty )
}

}