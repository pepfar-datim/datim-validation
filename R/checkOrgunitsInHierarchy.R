#' Title
#'
#' @param d
#' @param userOrgUnit
#' @param d2session
#'
#' @return
#' @export
#'
checkOrgunitsInHierarchy <-
  function(d,
           userOrgUnit,
           d2session = dynGet("d2_default_session", inherits = TRUE) ) {
    if (is.null(d)) {
      stop("Data cannot be null")
    }

    if (is.null(userOrgUnit)) {
      stop("You must specify and organisation unit")
    }

    data_orgunits <- unique(d$orgUnit)

    user_orgunits <- getOrganisationUnitMap(organisationUnit = userOrgUnit, d2session = d2session)

    invalid_orgunits <- data_orgunits[!(data_orgunits %in% user_orgunits$id)]

    if (length(invalid_orgunits) > 0) {

      warning("Organisation units detected which are not in the provided operating unit.")
      return(invalid_orgunits)

    } else {
      return(TRUE)
    }

    }
