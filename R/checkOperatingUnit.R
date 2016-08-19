#' @export
#' @title Function which checks that the organisation unit provided is valid
#' 
#' @description Validates that the supplied UID is a valid operating unit UID
#' 
#' @param organisationUnit Should be a UID of an operating unit. Defaults to the 
#' user organisation unit if not supplied
#' @return Returns a vector of organisation unit UIDs
#' @note
#' remapOUs(foo,"https://www.datim.org","admin","district","code","id","Ab12345678")
#' will remap organisation units specified as codes to UIDs
checkOperatingUnit<-function(organisationUnit=NA) {
  if (is.na(organisationUnit)) {organisationUnit = getOption("organisationunit")}
      sites<-getValidOperatingUnits()
      isValid<-organisationUnit %in% sites$id
      return(isValid)       
}