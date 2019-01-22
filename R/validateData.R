
#' @export
#' @title Internal function which prepares a parsed data frame for validation
#' 
#' @param d Data frame parsed by d2Parser
#'
#' @return Modifed data object with combis and totals appended


prepDataForValidation <- function(d) {
  header <-
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value")
  
  d <- d[, header[header %in% names(d)]]
  invalid<-function(x) { sapply(x, function(x) {is.na(x) || missing(x) || x=="" })}  
  d$value<-as.numeric(d$value) #This may throw a warning 
  notMissing <-
    function(x) {
      sapply(x, function(x) {
        !is.na(x) || !missing(x) || x != ""
      })
      
    }
  
  valid_rows<- d %>% 
    dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value) %>%
    dplyr::mutate_all(notMissing) %>% 
    dplyr::mutate(sum = rowSums(.[1:6])) %>% 
    dplyr::mutate(is_valid = (sum==6L) ) %>%
    dplyr::pull(is_valid) 
  
  if (sum(valid_rows) != NROW(d)) {
    foo <- nrow(d) - sum(!valid_rows)
    msg <-
      paste(foo,
            " rows are incomplete. Please check your file to ensure its correct.")
    warning(msg)
  }

  d <- d[valid_rows, ]

  d$combi<-paste0(d$dataElement,".",d$categoryOptionCombo)
  data.totals<-aggregate(value ~ dataElement + period + orgUnit + attributeOptionCombo, data = d,FUN=sum)
  data.totals$combi<-data.totals$dataElement
  data.totals$categoryOptionCombo<-NA
  data.totals<-data.totals[,names(d)]
  data.totals$value<-as.character(data.totals$value)
  rbind(d,data.totals)
}

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
#' @param datasets Vector of dataset UIDs which can  be used to restrict 
#' the validation rules which will be applied. 
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
if ( is.na(organisationUnit) ) {organisationUnit = getOption("organisationUnit")}
if (nrow(data) == 0 || is.null(data) ) {stop("Data values cannot be empty!")}


#Calculate the totals  and append to the data frame
data<-prepDataForValidation(data)

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
  validation.results <- plyr::colwise(as.character)(validation.results)
  mechs <- getMechanismsMap(organisationUnit = organisationUnit)
  ous <- getOrganisationUnitMap(organisationUnit = organisationUnit)
  
  validation.results$mech_code <-
    plyr::mapvalues(validation.results$attributeOptionCombo,
                    mechs$id,
                    mechs$code,
                    warn_missing = FALSE)
  
  validation.results$ou_name <-
    plyr::mapvalues(validation.results$orgUnit,
                    ous$id,
                    ous$shortName,
                    warn_missing = FALSE)
  
  # filter by data sets
  vr_rules <- getValidationRules()
  
  validDataElements <- getValidDataElements(datasets = datasets)
  
  match <-
    paste(unique(validDataElements$dataelementuid),
          sep = "",
          collapse = "|")
  
  vr_filter <-
    vr_rules[grepl(match, vr_rules$leftSide.expression) &
               grepl(match, vr_rules$rightSide.expression), "id"]
  
    validation.results[validation.results$id %in% vr_filter, ]
  
 
} else
{
  validation.results_empty
}

}
