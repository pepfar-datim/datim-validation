#' Title
#'
#' @param datasets Vector of dataset UIDs
#' @param d2session
#'
#' @return A list of data element IDs and all organisation units which are valid.
#'
getDataElementDetailsMap <- function(dataset, d2session = dynGet("d2_default_session",
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

  path <-
    paste0(
      "/dataSets/",
      dataset,
      "?fields=organisationUnits[id],categoryCombo[categoryOptionCombos[id]]",
      ",dataSetElements[dataElement[id]]"
    )

  #This API call should only be a function of the users actual orgunit, not the one which
  #The may be using for validation. Global users will have all DEs/OrgUnits anyway.


    r <- d2_api_get(path, d2session = d2session)

    #Reshape this list a bit to make it a bit easier to deal with
    #Also deal with NULLs

    des_details <- list(acocs = character(),
                        des = character(),
                        ous = character())



    if (NROW(r$categoryCombo$categoryOptionCombos) > 0) {
      des_details$acocs <- r$categoryCombo$categoryOptionCombos$id
    }

    if (length(r$dataSetElements) > 0) {
      des_details$des <- r$dataSetElements$dataElement.id
    }

    if (NROW(r$organisationUnits) > 0) {
      des_details$ous <- r$organisationUnits$id
    }


    des_details
  }


#' Title
#'
#' @param orgunits_data_elements A data frame consisting of organisation units and data elements
#' obtained from the validation payload.
#' @param de_map A nested list of possible data element /organiation unit combinations.
#'
#' @return A data frame of organisation unit/ data element combinations which are invalid.
validateOrgunitDataElements <- function(orgunit_data_elements, de_map) {

  org_unit <- unique(orgunit_data_elements$orgUnit)
  if (length(org_unit) > 1) {
    stop("Data elements can only be validated for a single orgunit.")
  }

  #Which datasets does this orgunit belong to?
  has_dataset <-
    unlist(lapply(de_map, \(x) any(
      x$ous %in% org_unit
    )))

  #This orgunit does not have any data sets
  #Thus all data elements are invalid.
  if (!any(has_dataset)) {
    return(orgunit_data_elements)
  }
  #Filter the complete map for this orgunit
  de_map <- de_map[has_dataset]

  #Filter the datasets and get the possible data elements
  possible_des <- lapply(de_map, \(.) .$des) |> unlist() |> unique()

  #Return all data elements which are not part of any of the datasets.
  orgunit_data_elements[!(orgunit_data_elements$dataElement %in% possible_des), ]
}


#' @export
#' @title checkDataElementOrgunitValidity(data,organisationUnit,datasets, d2session)
#'
#' @description Returns a data frame invalid data elements which exist in the data
#' but which do not have a valid organistion unit / dataset association.
#'
#' @param d datimvalidation object obtained from d2Parser.
#' @param datasets Should be a character vector of data set UIDs.
#' @param d2session datimutils d2session object
#' @return datimvalidation object obtained from d2Parser which have
#' been checked for invalid data element/ organisation unit combinations
#' @examples \dontrun{
#'      d<-d2Parser("myfile.csv",type="csv")
#'      ds<-getCurrentDataSets(type="RESULTS")
#'      d <- checkDataElementOrgunitValidity(d,datasets=ds)
#' }
#'
checkDataElementOrgunitValidity <-
  function(d,
           datasets,
           d2session = d2_default_session) {

  data <- d$data$import
  #Get a list of all data elements and orgunits
  #Present in the data and split into a list.
  des_ous <- unique(data[, c("orgUnit", "dataElement")])
  des_ous <- split(des_ous, des_ous$orgUnit)
  #Get a list of datasets, and the organisationunits and
  #data elements which they contain
  de_map <- lapply(datasets, \(x) getDataElementDetailsMap(x, d2session = d2session))

  des_ous_test <- lapply(des_ous, function(x) validateOrgunitDataElements(x, de_map))
  #Filter the list for any orgunits which have bogus data elements
  invalid_des_ous <- des_ous_test[unlist(lapply(des_ous_test, \(.) NROW(.) > 0))]


  if (length(invalid_des_ous) > 0L) {
    msg <- paste("ERROR! Invalid data element/orgunit associations were detected!")
    d$tests$invalid_des_ous <- do.call("rbind", invalid_des_ous)
    d$info$messages <- appendMessage(d$info$messages, msg, "ERROR")
    }

  d

}

#TODO: Provide checks for orgunit closure dates. Be sure that the orgunit is open for the period for which the
#data is being entered
