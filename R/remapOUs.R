#' @export
#' @title Function which converts mechanism codes, names or shortnames to to another class of identifiers
#'
#' @description remapOUs should be supplied a vector of organisation Units (names,codes or shortnames)
#' along with the other required paramaters. It will return a vector of to another class of identifiers.
#'
#' @param ous_in A vector of organisation unit identifiers (codes, names or shortNames)
#' @param organisationUnit UID of the Operating Unit/Country
#' @param mode_in Should be one of code, name, shortName or id. This is the class we are mapping from.
#' @param mode_out Should be on of code,name,shortName or id. This is the class we are mapping to.
#' @return Returns a vector of organisation unit UIDs will remap organisation units specified as codes to UIDs
#' @examples \dontrun{
#' d<-d2Parser("myfile.csv",type="csv")
#' #Add a new column with mechanism codes.
#' d$ou_names<-remapOUs(d$organisationUnits,mode_in="id",mode_out="shortName")
#' }
#' 
remapOUs <-
  function(ous_in,
           organisationUnit,
           mode_in = "code",
           mode_out = "id") {
    is_valid_mode <-
      (mode_in %in% c("code", "name", "shortName", "id"))  &
      (mode_out %in% c("code", "name", "shortName", "id"))
    if (is_valid_mode == FALSE)  {
      print("Not a valid mode. Must be one of code,name,shortName or id")
      stop()
    } else {
      sites <-
        getOrganisationUnitMap(organisationUnit)
      #TODO Get rid of this method and use stringi instead. 
      cmd <-
        paste0(
          "plyr::mapvalues(ous_in,sites$",
          mode_in,
          ",sites$",
          mode_out,
          ",warn_missing = FALSE)"
        )
      as.character(eval(parse(text = cmd)))
    }
  }