#' Title
#'
#' @param d
#' @param datasets
#' @param organisationUnit
#' @param datastream
#' @param d2session
#'
#' @return
#' @export
#'
runValidation <- function(d,
                          datasets = NA,
                          organisationUnit = NA,
                          datastream = NA,
                          d2session = dynGet("d2_default_session", inherits = TRUE)) {
  #Exact duplicates
  d <- getExactDuplicates(d)
  #Check orgunits are within users hierarchy
  d <- checkOrgunitsInHierarchy(d, userOrgUnit = organisationUnit, d2session = d2session)

  #Check data element cadence
  d <- checkDataElementCadence(d, d2session)

  # Check Data element disagg validity

  d <- checkDataElementDisaggValidity(d, datasets = datasets, d2session = d2session)

  #Check Data element orgunit validity

  d <- checkDataElementOrgunitValidity(d, datasets = datasets, d2session = d2session)

  #Check mechanism validity

  d <-
    checkMechanismValidity(d, organisationUnit = organisationUnit, d2session =  d2session)

  if (datastream == "MER") {
    #Negative values
    d <- checkNegativeValues(d, d2session = d2session)
    #Value type compliance
    d <- checkValueTypeCompliance(d, d2session = d2session)

    can_parallel <-
      "parallel" %in% rownames(utils::installed.packages()) == TRUE &
      .Platform$OS.type != "windows"

    d$tests$validation_rules <- validateData(
      d$data$import,
      organisationUnit = organisationUnit,
      return_violations_only = TRUE,
      parallel = can_parallel,
      d2session = d2session
    )
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
