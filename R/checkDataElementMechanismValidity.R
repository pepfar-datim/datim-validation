
#' Title Given a data frame of data elements and attribute option combinations
#' identify the invalid combinations.
#'
#' @param des_acocs A data frame consisting of unique data element
#' attribute option combinations.
#' @param des_acocs_map A list of all possible valid Data elements
#' and attribute option combinations.
#'
#' @return A data frame of invalid data element/disagg combinations.
#'
validateDEs_ACOCs <- function(des_acocs, des_acocs_map) {

  acoc <- unique(des_acocs$attributeOptionCombo)
  if (length(acoc) > 1) {
    stop("Data elements can only be validated for a attribute option combo")
  }

  #Which datasets does this orgunit belong to?
  has_acoc <- unlist(lapply(des_acocs_map, \(x) any(
    x$acocs %in% acoc
  )))

  #This orgunit does not have any data sets
  #Thus all data elements are invalid.
  if (!any(has_acoc)) {
    return(des_acocs)
  }
  #Filter the complete map for this orgunit
  des_acocs_map_f <- des_acocs_map[has_acoc]

  #Filter the datasets and get the possible data elements
  #Filter the datasets and get the possible data elements
  possible_des <- lapply(des_acocs_map_f, \(.) .$des) |> unlist() |> unique()

  #Return all data elements which do not have an association with a mechanism
  des_acocs[!(des_acocs$dataElement %in% possible_des), ]
}


#' @export
#' @title checkDataElementMechValidity(data,organisationUnit,datasets, d2session)
#'
#' @description Returns a data frame invalid data elements which exist in the data
#' but which do not have a valid association with an attribute option combo (mechanism.)
#'
#' @param d datimvalidation object
#' @param datasets Should be a character vector of data set UIDs.
#' @param d2session datimutils d2session object
#' @return datimvalidation object
#' @examples \dontrun{
#'      d <- d2Parser("myfile.csv", type = "csv")
#'      ds <- getCurrentDataSets(type = "RESULTS")
#'      d <- checkDataElementOrgunitValidity(data = d,datasets = ds)
#' }
#'
checkDataElementMechValidity <-
  function(d,
           datasets,
           d2session = d2_default_session) {
    data <- d$data$import
    #Get a list of all data elements and orgunits
    #Present in the data and split into a list.
    des_acocs <-
      unique(data[, c("dataElement", "attributeOptionCombo")])
    des_acocs <- split(des_acocs, des_acocs$attributeOptionCombo)
    #Get a list of datasets, and the organisationunits and
    #data elements which they contain
    des_acocs_map <-
      lapply(datasets,
             \(x) getDataElementDetailsMap(x, d2session = d2session))

    des_acocs_test <-
      lapply(des_acocs, \(x) validateDEs_ACOCs(x, des_acocs_map))
    #Filter the list for any orgunits which have bogus data elements
    bad_data_des_acocs <-
      des_acocs_test[unlist(lapply(des_acocs_test, \(.) NROW(.) > 0))]


    if (length(bad_data_des_acocs) > 0L) {
      msg <-
        paste("ERROR! Invalid data element/mechanism associations were detected!")
      d$info$messages <-
        appendMessage(d$info$messages, msg, "ERROR")
      d$tests$bad_data_des_acocs <- do.call("rbind", bad_data_des_acocs)

    } else {
      msg <- paste("No invalid data element/mechanism associations were detected.")
      d$info$messages <- appendMessage(d$info$messages, msg, "INFO")
    }

    d

}
