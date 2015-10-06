#' @title Function which checks the validity of mechanisms
#' 
#' @description checkValidMechanisms should be supplied a d2Parser compliant data frame along with the operating unit UID
#' along with the other required paramaters. It will return a vector of non-valid mechanism UIDs which were part of the data
#'
#' @param data d2Parser data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param organisationUnit UID of the operating unit
#' @param validityDate Indicates the validity date, as a valid R-parsable date string
#' @return Returns a vector of non-valid mechanisms
#' @note
#' getValidMechanisms(foo,"https://www.datim.org","admin","district","Ab12345678")
#' will remap organisation units specified as codes to UIDs
getInvalidMechanisms <-
  function(data,base.url,username,password,organisationUnit,validityDate='2016-09-29') {
    if ( class(data) != "data.frame" ) {print("Data must be a valid data frame"); stop() }
    mechs<-getMechanismsMap(base.url,username,password,organisationUnit)
    invalidMechs<-mechs[!mechs$isValid & mechs$categoryOptionCombo %in% unique(data$attributeOptionCombo),]
    return ( invalidMechs )
  }

