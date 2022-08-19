#' @export
#' @title Remap Category Option Combinations
#'
#' @description Function which converts category option combos from identifier
#' to another. \code{remapCategoryOptionCombos} should be supplied a vector of
#' category option combos (names, codes, uids, or shortnames). It will return a
#' vector of another class as specified with the \code{mode_out} paramater.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a vector of category option combos of the mode_out type.
#' @examples \dontrun{
#'     d <- d2Parser("myfile.csv", type = "csv")
#'     d$coc_codes <- remapOUs(d$categoryOptionCombos,
#'                             mode_in = "id",
#'                             mode_out = "code")
#' }
#'
remapCategoryOptionCombos <- function(cocs_in,
                                      mode_in,
                                      mode_out,
                                      d2session = dynGet("d2_default_session",
                                                         inherits = TRUE)) {

  valid_modes <- c("code", "name", "id", "shortName")
  is_valid_mode_in <- mode_in %in% valid_modes
  is_valid_mode_out <- mode_out %in% valid_modes
  is_ambiguous_mode <-
    (mode_in %in% c("name", "shortName")) & (mode_out %in% c("code", "id"))

  is_valid_mode <- is_valid_mode_in & is_valid_mode_out
  if (!is_valid_mode)  {
    stop("Not a valid mode. Must be one of code,name,shortName or id")
  }

  if (is_ambiguous_mode) {
    stop(paste("Ambiguous mapping mode detected.",
               "Names cannot be reliably mapped to unique codes/ids."))
  }

  cocs <- getCategoryOptionCombosMap(d2session = d2session)
  cmd <- paste("plyr::mapvalues(cocs_in,cocs$",
               mode_in, ",cocs$", mode_out,
               ",warn_missing = FALSE)")

  as.character(eval(parse(text = cmd)))

}
