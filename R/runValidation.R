#' Title
#'
#' @param d datimvalidation object
#' @param datasets list of dataset UIDs
#' @param organisationUnit users organisation unit or one which they wish to assume
#' @param datastream one of RESULTS, TARGETS, NARRATIVES, SIMS.
#' @param d2session datimutils session object
#'
#' @return datimvalidation object with all validations run.
#' @export
#'
runValidation <- function(d,
                          d2session = dynGet("d2_default_session", inherits = TRUE)) {

  datastream <- d$info$datastream
  datasets <- d$info$datasets
  #Exact duplicates
  d <- getExactDuplicates(d)
  #Check orgunits are within users hierarchy
  d <- checkOrgunitsInHierarchy(d, userOrgUnit = d$info$organisationUnit, d2session = d2session)

  #Check data element cadence
  d <- checkDataElementCadence(d, d2session)

  # Check Data element disagg validity

  d <- checkDataElementDisaggValidity(d, datasets = datasets, d2session = d2session)

  #Check Data element orgunit validity

  d <- checkDataElementOrgunitValidity(d, datasets = datasets, d2session = d2session)

  #Check mechanism validity

  d <-
    checkMechanismValidity(d, organisationUnit = d$info$organisationUnit, d2session =  d2session)

  if (datastream %in% c("RESULTS","TARGETS")) {
    #Negative values
    d <- checkNegativeValues(d, d2session = d2session)
    #Value type compliance
    d <- checkValueTypeCompliance(d, d2session = d2session)
    #Validation rules 
    d <- checkValidationRules(d, d2session = d2session)
  }

  if (datastream == "SIMS") {
    #Value type compliance
    d <- checkValueTypeCompliance(d, d2session = d2session)
  }

  if (datastream == "NARRATIVES") {
    d <- checkNarrativeLength(d)
}

  d
}
