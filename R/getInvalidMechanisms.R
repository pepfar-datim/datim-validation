#' @export
#' @title Function which checks the validity of mechanisms
#' 
#' @description checkValidMechanisms should be supplied a d2Parser compliant data frame along with the operating unit UID
#' along with the other required paramaters. It will return a vector of non-valid mechanism UIDs which were part of the data
#'
#' @param data d2Parser data frame
#' @param organisationUnit UID of the operating unit.Defaults to the user organisation unit if left blank
#' @param ISO String which identifies the period, such as 2016Q1 or 2016Q2
#' @return Returns a vector of non-valid mechanisms
#' @note
#' getValidMechanisms(foo,"https://www.datim.org","admin","district","Ab12345678")
#' will remap organisation units specified as codes to UIDs
getInvalidMechanisms <-
  function(data,organisationUnit=NA,ISO=NA) {
    if ( class(data) != "data.frame" ) {print("Data must be a valid data frame"); stop() }
    if ( is.na(ISO) ) {print("A valid ISO period identifier must be supplied"); stop() }
    if (is.na(organisationUnit)) {organisationUnit = getOption("organisationUnit")}
    mechs<-getMechanismsMap(organisationUnit,ISO)
    foo<-unique(data$attributeOptionCombo)
    invalidMechs<-foo[!(foo %in% mechs$id) | (foo %in% mechs[!mechs$isValid,c("categoryOptionCombos")])]
    return ( invalidMechs )
  }

