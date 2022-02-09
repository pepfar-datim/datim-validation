#' @export
#' @title Function which converts data element codes, names or shortnames to another class of identifiers
#'
#' @description remapDes should be supplied a vector of data elements (names,codes or shortnames)
#' along with the other required paramaters. It will return a vector of identifiers of a different class.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a vector of DATIM uids for each data element which needs to be remapped.
#' @examples \dontrun{
#'     d <- d2Parser("myfile.csv",type="csv")
#'     d$de_codes <- remapOUs(d$dataElements,mode_in="id",mode_out="code")
#' }
remapDEs <- function(des_in,
                     mode_in = "code",
                     mode_out = "id",
                     d2session = dynGet("d2_default_session",
                                        inherits = TRUE)) {
  is_valid_mode <-
    (mode_in %in% c("code", "name", "shortName", "id"))  &
    (mode_out %in% c("code", "name", "shortName", "id"))
  if (is_valid_mode == FALSE)  {
    print("Not a valid mode. Must be one of code,name,shortName or id")
    stop()
  } else {
    des <- getDataElementMap(d2session = d2session)
    cmd <-
      paste0(
        "plyr::mapvalues(des_in,des$",
        mode_in,
        ",des$",
        mode_out,
        ",warn_missing = FALSE)"
      )
    eval(parse(text = cmd))
  }

}
