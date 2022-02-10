#' @export
#' @title Remap Operating Units
#'
#' @description Function which converts mechanism codes, names, or shortnames to
#' another class of identifiers. \code{remapOUs} should be supplied a vector of
#' organisation Units (names, codes or shortnames) along with the other required
#' paramaters. It will return a vector of to another class of identifiers.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a vector of organisation unit UIDs will remap organisation
#' units specified as codes to UIDs
#'
#' @examples \dontrun{
#' d <- d2Parser("myfile.csv",type="csv")
#' d$ou_names <- remapOUs(d$organisationUnits,mode_in="id",mode_out="shortName")
#' }
#'
remapOUs <-
  function(ous_in,
           organisationUnit,
           mode_in = "code",
           mode_out = "id",
           d2session = dynGet("d2_default_session", inherits = TRUE)) {
    is_valid_mode <-
      (mode_in %in% c("code", "name", "shortName", "id"))  &
      (mode_out %in% c("code", "name", "shortName", "id"))
    if (is_valid_mode == FALSE)  {
      print("Not a valid mode. Must be one of code,name,shortName or id")
      stop()
    } else {
      sites <-
        getOrganisationUnitMap(organisationUnit, d2session = d2session)
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
