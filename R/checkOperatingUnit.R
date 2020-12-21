#' @export
#' @title Function which checks that the organisation unit provided is valid
#' 
#' @description Validates that the supplied UID is a valid operating unit UID
#' 
#' @param organisationUnit Should be a UID of an operating unit. Defaults to the 
#' user organisation unit if not supplied
#' @param d2session datimutils login session object  
#' @return Returns a vector of organisation unit UIDs
#' @note
#' remapOUs(foo,"https://www.datim.org","admin","district","code","id","Ab12345678")
#' will remap organisation units specified as codes to UIDs
checkOperatingUnit<-function(organisationUnit=NA, d2session = d2_default_session ) {
  if (is.na(organisationUnit)) {organisationUnit = d2session$user_orgunit}
      sites<-getValidOperatingUnits(d2session = d2session)
      organisationUnit %in% sites$id
}