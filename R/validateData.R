#' @export
#' @title Internal function which prepares a parsed data frame for validation
#'
#' @param d Data frame parsed by d2Parser
#'
#' @return Modifed data object with combis and totals appended.
#

prepDataForValidation <- function(d) {
  header <-
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value"
    )
  
  d <- d[, header[header %in% names(d)]]
  invalid <-
    function(x) {
      sapply(x, function(x) {
        is.na(x) || missing(x) || x == ""
      })
    }
  d$value <- as.numeric(d$value) #This may throw a warning
  
  total_rows <- NROW(d)
  d <- d %>%
    dplyr::filter(purrr::reduce(purrr::map(., is.na), `+`) == 0)
  
  if (total_rows != NROW(d)) {
    foo <- total_rows - NROW(d)
    msg <-
      paste(foo,
            " rows are incomplete. Please check your file to ensure its correct.")
    warning(msg)
  }
  
  #Calculate the totals
  d$combi <- paste0("#{",d$dataElement,".", d$categoryOptionCombo,"}")
  # data.totals <-
  #   aggregate(
  #     value ~ dataElement + period + orgUnit + attributeOptionCombo,
  #     data = d,
  #     FUN = sum
  #   )
  # data.totals$combi <- data.totals$dataElement
  # data.totals$categoryOptionCombo <- NA
  # data.totals <- data.totals[, names(d)]
  # dplyr::bind_rows(d, data.totals) %>%
  #   dplyr::filter(value != 0)
  
  d
}

#' @export
#' @importFrom stats aggregate
#' @importFrom stats complete.cases
#' @title Function which checks the of a DATIM data payload agaist define validation rules
#'
#' @description validateData should be supplied a d2Parser compliant data frame.
#'The data frame is checked dynamically against validation rules defined in the DATIM server.
#' @param data d2Parser data frame
#' @param organisationUnit Organization unit. Defaults to the user organization unit if not supplied.
#' @param return_violations_only Parameter to return only violations or all validation rule evaluations.
#' @param parallel Should the rules be evaluated in parallel. Default is to not evaluate in parallel.
#' @param datasets Vector of dataset UIDs which can  be used to restrict
#' the validation rules which will be applied.
#' @param vr Data frame of validation rules from getValidationRules, or a custom filtered data frame. 
#' @param d2session datimutils d2session object
#' @return Returns a data frame with validation rule results.
#' @examples \dontrun{
#'   d<-d2Parser("myfile.csv",type="csv")
#'   vr_rules<-validateData(d)
#'   doMC::registerDoMC(cores=4)
#'   vr_rules<-validateData(d,parallel=TRUE)
#'   ds<-getCurrentMERDataSets(type="RESULTS")
#'   vr_rules<-validateData(d,parallel=TRUE,datasets=ds)
#' }
validateData <-function(data,
           organisationUnit = NA,
           return_violations_only = TRUE,
           parallel = FALSE,
           datasets = NA, 
           vr = NULL,
           d2session = d2_default_session) {
    
  if (nrow(data) == 0 ||
      is.null(data)) {
    stop("Data values cannot be empty!")
  }
  
  if (is.na(organisationUnit)) {
    organisationUnit = d2session$user_orgunit
  }
  
  allDataSets <- getDataSets(d2session = d2session)
  dataSetValid <- Reduce("&", datasets %in% allDataSets$id)
  while (!dataSetValid || is.na(dataSetValid)) {
    datasets <- selectDataset(d2session = d2session)
    if (length(datasets) == 0) {
      break
    }
    dataSetValid <- Reduce("&", datasets %in% allDataSets$id)
  }
  
  if (!dataSetValid) {
    stop("Invalid dataset")
    
  }
  
#Calculate the totals  and append to the data frame
data<-prepDataForValidation(data)

#Empty data frame
validation.results_empty <- data.frame(
  name = character(),
  id = character(),
  periodType = character(),
  description = character(),
  operator = character(),
  leftSide.expression = numeric(),
  leftSide.missingValueStrategy = character(),
  rightSide.expression = numeric(),
  rightSide.ops = integer(),
  leftSide.ops = integer(),
  leftSide.count = integer(),
  rightSide.count = integer(),
  formula = character(),
  result = logical()
)
validation.results <- validation.results_empty

#Load validation rules from the server if none are supplied.
if (is.null(vr)) {
  vr <- getValidationRules(d2session = d2session)
}

if (Sys.info()[['sysname']] == "Windows") {
  if (parallel == TRUE)  {
    warning("Parallel execution is not supported on Windows")
    parallel = FALSE
  }
}

validation.results <-
  plyr::ddply(data, plyr::.(period, attributeOptionCombo, orgUnit),
              function(x)
                evaluateValidation(x$combi, x$value, vr, return_violations_only),
              .parallel = parallel,
              .inform = TRUE)

if ( nrow(validation.results) > 0 ) {
  validation.results <- plyr::colwise(as.character)(validation.results)
  mechs <- getMechanismsMap(organisationUnit = organisationUnit, d2session = d2session)
  ous <- getOrganisationUnitMap(organisationUnit = organisationUnit, d2session = d2session)
  
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
  vr_rules <- getValidationRules( d2session = d2session )
  
  validDataElements <- getValidDataElements(datasets = datasets, d2session = d2session)
  
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
