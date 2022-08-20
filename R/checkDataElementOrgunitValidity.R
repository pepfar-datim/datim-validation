getDataElementOrgunitMap <- function(dataset, d2session = d2_default_session) {
  #TODO: Rewrite using datimutils
  #Note that we are not doing any additional filtering here
  #However, the orgunits should be filtered by the server
  #based on the users orgunit profile.
  url <-
    paste0(
      d2session$base_url,
      "api/", api_version(), "/dataSets/", dataset, "?fields=organisationUnits[id],dataSetElements[dataElement[id]]")

  r <- httr::GET(url, httr::timeout(300), handle = d2session$handle)
  r <- httr::content(r, "text")
  ous_des <- jsonlite::fromJSON(r, flatten = TRUE)

  ous_des

}

validateOrgunitDataElements <- function(orgUnitDataElements, de_map) {

  orgUnit <- unique(orgUnitDataElements$orgUnit)
  #Which datasets does this orgunit belong to?
  has_dataset <-
    unlist(lapply(de_map, \(x) any(
      x$organisationUnits$id %in% orgUnit
    )))

  #Filter the complete map for this orgunit
  de_map <- de_map[has_dataset]

  #Need to deal with the zero case here.
  #This means that the orgunit has data,
  #but no valid data elemens
  if (length(de_map) == 0L) {
    return(orgUnitDataElements)
  }

  #Filter the datasets and get the possible data elements
  possible_des <-
    lapply(lapply(de_map, \(.) .$dataSetElement), \(.) .$dataElement.id) |> unlist() |> unique()

  #Are all the data elements for this orgunit in the list of possible des?
  orgUnitDataElements[!(orgUnitDataElements$dataElement %in% possible_des),]
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
checkDataElementOrgunitValidity <- function(d, datasets, return_violations =TRUE, d2session = d2_default_session) {
  #Get a list of all data elements and orgunits
  #Present in the data and split into a list.
  des_ous <- unique(d[,c("orgUnit","dataElement")])
  des_ous <- split(des_ous, des_ous$orgUnit)
  #Get a list of datasets, and the organisationunits and
  #data elements which they contain
  #TODO: This likely needs to be cached. Skip for now.
  de_map <- lapply(datasets, \(x) getDataElementOrgunitMap(x, d2session = d2session))

  des_ous_test <- lapply(des_ous, function(x) validateOrgunitDataElements(x,de_map))
  #Filter the list for any orgunits which have bogus data elements
  bad_data_des_ous <- des_ous_test[unlist(lapply(des_ous_test,\(.) NROW(.) > 0))]


  if (length(bad_data_des_ous) > 0) {
    warning("Invalid data element/orgunit associations were detected!")
    if (return_violations) {
      return(do.call("rbind", bad_data_des_ous))
    } else {
      return(FALSE)
    }
  }

  TRUE

}

#TODO: Provide checks for orgunit closure dates. Be sure that the orgunit is open for the period for which the
#data is being entered
