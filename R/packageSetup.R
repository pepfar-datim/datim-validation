# TODO merge `iso`, `ISO`, `isoPeriod`
# TODO merge `csv_header` and `hasHeader`
# TODO merge `return_violations` and `return_violations_only`
# TODO merge `d2session` and `d2_session`

#' @title Standardized package function parameter definitions
#'
#' @param csv_header By default, CSV files are assumed to have a header,
#' otherwise FALSE will allow for files without a CSV header.
#' @param d D2 Parsed data frame
#' @param data D2 compliant data frame object
#' @param dataElementIdScheme Should be one of either code, name, shortName,
#' or id. If this paramater is "id", then the Data elements are assumed to be
#' already specififed as UIDs.
#' @param datasets Should be a character vector of data set UIDs.
#' Alternatively, if left missing, user will be promted to choose from a list.
#' @param filename Location of the payload to be imported. Should be a valid
#' import file.
#' @param hasHeader TRUE by default. Should be set to FALSE if the file does not
#' contain header information.
#' @param idScheme Remapping scheme for category option combos.
#' @param invalidData Exclude any (NA or missing) data from the parsed file?
#' @param iso String which identifies the period, such as 2016Q1 or 2016Q2
#' @param ISO Formatted string such as 2016Q1. Refer to DHIS2 documentation for
#' details.
#' @param isoPeriod period to be used for date shift boundaries. If not
#' provided, no boundaries are set.
#' @param cocs_in A vector of category option combinations.
#' @param des_in A vector of data element identifiers (codes, names, or
#' shortNames).
#' @param mechs_in A vector of data element identifiers (codes, names, or
#' shortNames).
#' @param ous_in A vector of organisation unit identifiers (codes, names, or
#' shortNames).
#' @param mode_in Should be one of code, name,shortName or id. This is the class
#' we are mapping from.
#' @param mode_out Should be one of code,name,shortName or id. This is the class
#' we are mapping to.
#' @param organisationUnit Organisation unit ancestor.
#' @param orgUnitIdScheme Should be one of either code, name, shortName or id.
#' If this paramater is "id", then the organisation units are assumed to be
#' already specififed as UIDs
#' @param return_violations Boolean to return violations only.
#' @param return_violations_only Parameter to return only violations or all
#' validation rule evaluations.
#' @param sig MD5 hash of the objects URL reference
#' @param vr Validation rule object
#' @param wd By default, the .R_Cache directory in the working directory.
#' @param d2session DHIS2 Session id
#' @param d2_session DHIS2 Session id
#'
#' @return list of all paramaters of this constructor function
#'
datim_validation_params <- function(csv_header,
                                    d,
                                    data,
                                    dataElementIdScheme,
                                    datasets,
                                    filename,
                                    hasHeader,
                                    idScheme,
                                    invalidData,
                                    iso,
                                    ISO,
                                    isoPeriod,
                                    cocs_in,
                                    des_in,
                                    mechs_in,
                                    ous_in,
                                    mode_in,
                                    mode_out,
                                    organisationUnit,
                                    orgUnitIdScheme,
                                    return_violations,
                                    return_violations_only,
                                    sig,
                                    vr,
                                    wd,
                                    d2session,
                                    d2_session) {

  # This function should return something
  #Return its own argument names
  #rlang::fn_fmls_names(fn = datim_validation_params)
  #or explicitly return
  #NULL
}
