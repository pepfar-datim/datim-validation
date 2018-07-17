#' @export
#' @importFrom stats aggregate
#' @importFrom stats complete.cases
#' @title Function which checks the of a DATIM data payload agaist define validation rules
#' 
#' @description validateData should be supplied a d2Parser compliant data frame.
#'The data frame is checked dynamically against validation rules defined in the DATIM server.
#' @param data d2Parser data frame
#' @param organisationUnit Organisation unit. Defaults to the user organisation unit if not supplied.
#' @param return_violations_only Paramater to return only violations or all validation rule evalualtions.
#' @param parallel Should the rules be evaluated in parallel. 
#' @return Returns a data frame with validation rule results.
validateData<-function(data,organisationUnit=NA,return_violations_only=TRUE,parallel=TRUE,datasets=NA) {

    allDataSets<-getDataSets()
    dataSetValid<-Reduce("&",datasets %in% allDataSets$id)
    while(!dataSetValid || is.na(dataSetValid) ) {
      datasets<-selectDataset()
      if (length(datasets) == 0) {break;}
      dataSetValid <- Reduce("&",datasets %in% allDataSets$id) 
    }
    if (length(datasets) == 0 || is.na(datasets)) { stop("Invalid dataset"); }

    print("Processing validation")

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
validation.results<-plyr::colwise(as.character)(validation.results) 
mechs<-getMechanismsMap()
ous<-getOrganisationUnitMap()

validation.results$mech_code<-plyr::mapvalues(validation.results$attributeOptionCombo,mechs$id,mechs$code,warn_missing = FALSE)
validation.results$ou_name<-plyr::mapvalues(validation.results$orgUnit,ous$id,ous$shortName,warn_missing = FALSE)

# filter by data sets
vr_rules<-getValidationRules()
validDataElements<-getValidDataElements(datasets=ds)
match<-paste(unique(validDataElements$dataelementuid),sep="",collapse="|")
vr_filter<-vr_rules[grepl(match,vr_rules$leftSide.expression) & grepl(match,vr_rules$rightSide.expression),"id"]
vr_violations<-validation.results[ vr_violations$id %in% vr_filter,]


return  ( vr_violations ) 
} else
{
  return( validation.results_empty )
}

}
