#' Title Get a list of data elemement and attribute option combos
#' which they are theoretically associated with from the server.
#'
#' @param datasets Vector of dataset UIDs
#' @param d2session
#'
#' @return A list of all data elements and valid attribute option combos
#' based on a given dataset.
#'
getDE_ACOC_Map <- function(dataset, d2session = dynGet("d2_default_session",
                                                       inherits = TRUE)) {
  #TODO: Rewrite using datimutils
  if (length(dataset) != 1L) {
    stop("You must specify a single dataset UID")
  }

  #A map of data elements and attribute option combos which are theoretically
  #allowable. T
  url <-
    paste0(
      d2session$base_url,
      "api/",
      api_version(),
      "/dataSets/",
      dataset,
      "?fields=categoryCombo[categoryOptionCombos[id]],dataSetElements[dataElement[id]]"
    )

  r <- httpcache::GET(url, httr::timeout(300), handle = d2session$handle)
  if (r$status == 200L) {
    r <- httr::content(r, "text")
    des_acocs <- jsonlite::fromJSON(r, flatten = TRUE)
  } else  {
    stop("Could not retreive a list of data elements and attribute option combos from the server")
  }

  des_acocs
}



#' Title Given a data frame of data elements and attribute option combinations
#' identify the invalid combinations.
#'
#' @param des_acocs A data frame consisting of unique data element
#' attribute option combinations.
#' @param des_acocs_map A list of all possible valid Data elements
#' and attribute option combinations.
#'
#' @return
#'
#' @examples
validateDEs_ACOCs <- function(des_acocs, des_acocs_map) {

  acoc <- unique(des_acocs$attributeOptionCombo)
  if (length(acoc) > 1) {
    stop("Data elements can only be validated for a attribute option combo")
  }

  #Which datasets does this orgunit belong to?
  has_acoc <- unlist(lapply(des_acocs_map, \(x) any(
    x$categoryCombo$categoryOptionCombos$id %in% acoc
  )))

  #This orgunit does not have any data sets
  #Thus all data elements are invalid.
  if (!any(has_acoc)) {
    return(des_acocs)
  }
  #Filter the complete map for this orgunit
  des_acocs_map_f <- des_acocs_map[has_acoc]

  #Filter the datasets and get the possible data elements
  possible_des <-
    lapply(lapply(des_acocs_map_f, \(.) .$dataSetElement), \(.) .$dataElement.id) |> unlist() |> unique()

  #Return all data elements which do not have an association with a mechanism
  des_acocs[!(des_acocs$dataElement %in% possible_des), ]
}


#' @export
#' @title checkDataElementMechValidity(data,organisationUnit,datasets, d2session)
#'
#' @description Returns a data frame invalid data elements which exist in the data
#' but which do not have a valid association with an attribute option combo (mechanism.)
#'
#' @param data D2 compliant data frame
#' @param datasets Should be a character vector of data set UIDs.
#' @param return_violations Return the invalid data if TRUE
#' @param d2session datimutils d2session object
#' @return Returns subset of data which contains
#'   invalid data element / attribute option combo associations. As an example, MER results
#'   data should always be submitted against mechanism, while other data may be
#'   reported against the "default" mechanism.
#'   If no violations are found, a boolean
#'   TRUE value is returned.
#' @examples \dontrun{
#'      d <- d2Parser("myfile.csv", type = "csv")
#'      ds <- getCurrentMERDataSets(type = "RESULTS")
#'      checkDataElementOrgunitValidity(data = d,datasets = ds)
#' }
#'
checkDataElementMechValidity <-
  function(d,
           datasets,
           return_violations  = TRUE,
           d2session = d2_default_session) {

    #Get a list of all data elements and orgunits
    #Present in the data and split into a list.
    des_acocs <- unique(d[, c("dataElement", "attributeOptionCombo")])
    des_acocs <- split(des_acocs, des_acocs$attributeOptionCombo)
    #Get a list of datasets, and the organisationunits and
    #data elements which they contain
    des_acocs_map <- lapply(datasets, \(x) getDE_ACOC_Map(x, d2session = d2session))

    des_acocs_test <- lapply(des_acocs, function(x) validateDEs_ACOCs(x, des_acocs_map))
    #Filter the list for any orgunits which have bogus data elements
    bad_data_des_acocs <- des_acocs_test[unlist(lapply(des_acocs_test, \(.) NROW(.) > 0))]


    if (length(bad_data_des_acocs) > 0L) {
      warning("Invalid data element/mechanism associations were detected!")
      if (return_violations) {
        return(do.call("rbind", bad_data_des_acocs))
      } else {
        return(FALSE)
      }
    } else {

      if (return_violations) {
        return(data.frame(dataElement = character(),
                          attributeOptionCombo = character()))
      } else {
        return(TRUE)
      }
    }

  }
