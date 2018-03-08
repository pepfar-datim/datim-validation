#' @export
#' @title Function which checks the validity of mechanisms
#' 
#' @description checkValidMechanisms should be supplied a d2Parser compliant data frame along with the operating unit UID
#' along with the other required paramaters. It will return a vector of non-valid mechanism UIDs which were part of the data
#'
#' @param data d2Parser data frame
#' @param organisationUnit UID of the operating unit.Defaults to the user organisation unit if left blank
#' @return Returns a vector of non-valid mechanisms
#' @note
#' getValidMechanisms(foo,"https://www.datim.org","admin","district","Ab12345678")
#' will remap organisation units specified as codes to UIDs
getInvalidMechanisms <-
  function(data,organisationUnit=NA) {
    if ( class(data) != "data.frame" ) {print("Data must be a valid data frame"); stop() }
    if (is.na(organisationUnit)) {organisationUnit = getOption("organisationUnit")}
    mechs<-getMechanismsMap(organisationUnit)
    foo<-unique(data$attributeOptionCombo)
    invalidMechs<-foo[!(foo %in% mechs$id)]
    return ( invalidMechs )
  }

