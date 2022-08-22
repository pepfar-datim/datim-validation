#' Title
#'
#' @param datasets Vector of dataset UIDs
#' @param d2session
#'
#' @return A list of data element IDs and all organisation units which are valid.
#'
#' @examples
getDataElementOrgunitMap <- function(dataset, d2session = dynGet("d2_default_session",
                                                                  inherits = TRUE)) {
  #TODO: Rewrite using datimutils
  #Note that we are not doing any additional filtering here
  #However, the orgunits should be filtered by the server
  #based on the users orgunit profile.
  #We will check seperately whether all of the orgunits present
  # in the file actually belong to the users hierarchy.
  if (length(dataset) != 1L) {
    stop("You must specify a single dataset UID")
  }

  url <-
    paste0(
      d2session$base_url,
      "api/", api_version(), "/dataSets/", dataset, "?fields=organisationUnits[id],dataSetElements[dataElement[id]]")

  #This API call should only be a function of the users actual orgunit, not the one which
  #The may be using for validation. Global users will have all DEs/OrgUnits anyway.
  sig <- digest::digest(paste0(url, d2session$user_orgunit), algo = "md5", serialize = FALSE)
  ous_des <- getCachedObject(sig)
  if (is.null(ous_des)) {
    r <- httr::GET(url, httr::timeout(300), handle = d2session$handle)
    if (r$status == 200L) {
        r <- httr::content(r, "text")
        ous_des <- jsonlite::fromJSON(r, flatten = TRUE)
        saveCachedObject(ous_des, sig)
    } else  {
    stop("Could not retreive a list of data elements and orgunits from the server")
      }
    }
  ous_des
  }


#' Title
#'
#' @param orgunits_data_elements
#' @param de_map
#'
#' @return
#'
#' @examples
validateOrgunitDataElements <- function(orgunit_data_elements, de_map) {

  org_unit <- unique(orgunit_data_elements$orgUnit)
  if (length(org_unit) > 1) {
    stop("Data elements can only be validated for a single orgunit.")
  }

  #Which datasets does this orgunit belong to?
  has_dataset <-
    unlist(lapply(de_map, \(x) any(
      x$organisationUnits$id %in% org_unit
    )))

  #This orgunit does not have any data sets
  #Thus all data elements are invalid.
  if (!any(has_dataset)) {
    return(orgunit_data_elements)
  }
  #Filter the complete map for this orgunit
  de_map <- de_map[has_dataset]

  #Filter the datasets and get the possible data elements
  possible_des <-
    lapply(lapply(de_map, \(.) .$dataSetElement), \(.) .$dataElement.id) |> unlist() |> unique()

  #Return all data elements which are not part of any of the datasets.
  orgunit_data_elements[!(orgunit_data_elements$dataElement %in% possible_des), ]
}


#' @export
#' @title checkDataElementOrgunitValidity(data,organisationUnit,datasets, d2session)
#'
#' @description Returns a data frame invalid data elements which exist in the data
#' but which do not have a valid organistion unit / dataset association.
#'
#' @param data D2 compliant data frame
#' @param datasets Should be a character vector of data set UIDs.
#' @param return_violations Return the invalid data if TRUE
#' @param d2session datimutils d2session object
#' @return Returns subset of data which contains
#'   invalid data element / organisation unit associations. If no violations are found, a boolean
#'   TRUE value is returned.
#' @examples \dontrun{
#'      d<-d2Parser("myfile.csv",type="csv")
#'      ds<-getCurrentMERDataSets(type="RESULTS")
#'      checkDataElementOrgunitValidity(data=d,datasets=ds)
#' }
#'
checkDataElementOrgunitValidity <-
  function(d,
           datasets,
           return_violations  = TRUE,
           d2session = d2_default_session) {

  #Get a list of all data elements and orgunits
  #Present in the data and split into a list.
  des_ous <- unique(d[, c("orgUnit", "dataElement")])
  des_ous <- split(des_ous, des_ous$orgUnit)
  #Get a list of datasets, and the organisationunits and
  #data elements which they contain
  #TODO: This likely needs to be cached. Skip for now.
  de_map <- lapply(datasets, \(x) getDataElementOrgunitMap(x, d2session = d2session))

  des_ous_test <- lapply(des_ous, function(x) validateOrgunitDataElements(x, de_map))
  #Filter the list for any orgunits which have bogus data elements
  bad_data_des_ous <- des_ous_test[unlist(lapply(des_ous_test, \(.) NROW(.) > 0))]


  if (length(bad_data_des_ous) > 0) {
    warning("Invalid data element/orgunit associations were detected!")
    if (return_violations) {
      return(do.call("rbind", bad_data_des_ous))
    } else {
      return(FALSE)
    }
  } else {

    if (return_violations) {
      return(data.frame(dataElement = character(),
                        orgUnit = character()))
    } else {
      return(TRUE)
    }
  }

}

#TODO: Provide checks for orgunit closure dates. Be sure that the orgunit is open for the period for which the
#data is being entered
