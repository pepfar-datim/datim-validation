#' @export
#' @title Function which converts mechanism codes, names or shortnames to UIDs
#' 
#' @description remapMechs should be supplied a vector of data mechanisms (names,codes or shortnames)
#' along with the other required paramaters. It will return a vector of UIDs.
#' 
#'
#' @param mechs_in A vector of data element identifiers (codes, names or shortNames)
#' @param mode_in Should be one of code, name or id. This is the type we are mapping from.
#' @param mode_out Should be one of code,name or id. This is the type we are mapping to.
#' @param organisationUnit The UID of the operating unit.
#' @param d2session datimutils d2session object
#' @return Returns a vector of mechanism UIDs
#' @examples \dontrun{
#' d<-d2Parser("myfile.csv",type="csv")
#' #Add a new column with organisation unit short names. 
#' d$mech_names<-remapMechs(d$attributeOptionCombo,mode_in="id",mode_out="code")
#' }
remapMechs <-
  function(mechs_in,
           organisationUnit,
           mode_in = "code",
           mode_out = "id",
           d2session = d2_default_session) {
    is_valid_mode <-
      (mode_in %in% c("code", "name", "id")) &
      (mode_out %in% c("code", "name", "id"))
    if (is_valid_mode == FALSE)  {
      print("Not a valid mode. Must be one of code,name or id")
      stop()
    }
    mechs <- getMechanismsMap(organisationUnit, d2session = d2session)
    cmd <-
      paste(
        "plyr::mapvalues(mechs_in,mechs$",
        mode_in,
        ",mechs$",
        mode_out,
        ",warn_missing = FALSE)"
      )
    as.character(eval(parse(text = cmd)))
  }