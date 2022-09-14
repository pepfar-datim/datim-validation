#' @title Utility function to check that the supplied coding scheme is correct
#'
#' @description checkCodingScheme will ensure that all indentifiers
#' after parsing are valid UIDs,
#' or in the case of periods, valid periods.
#'
#' @inheritParams datim_validation_params
#'
#' @return Warnings are issued if the coding scheme is not
#' congruent with what has been supplied as a paramater.
#'
#'
checkCodingScheme <- function(data,
                              d2session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
  #This is a very superficial and quick check,
  #just to be sure that the coding scheme is correct.
  #Additional validation will be required to be sure data elements,
  #catcombos and orgunits are properly associated
  is_valid <- TRUE
  data_element_check_v <-
    unique(data$dataElement) %in% getDataElementMap(d2session = d2session)$id
  data_element_check <- unique(data$dataElement)[!data_element_check_v]
  if (length(data_element_check) > 0) {
    warning(
      "The following data element identifiers could not be found:",
      paste(data_element_check, sep = "", collapse = ",")
    )
    is_valid <- FALSE
  }
  #TODO: This is duplicative with checkOrgunitsInHierarchy
  orgunit_check <-
    unique(data$orgUnit)[!(
      unique(data$orgUnit)
      %in%
      getOrganisationUnitMap(d2session = d2session)$id
    )]
  if (length(orgunit_check) > 0) {
    warning(
      "The following org unit identifiers could not be found:",
      paste(orgunit_check, sep = "", collapse = ",")
    )
    is_valid <- FALSE
  }
  coc_check <-
    unique(data$categoryOptionCombo)[!(
      unique(data$categoryOptionCombo) %in%
        getCategoryOptionCombosMap(d2session = d2session)$id
    )]
  if (length(coc_check) > 0) {
    warning(
      "The following category option combo identifiers could not be found:",
      paste(coc_check, sep = "", collapse = ",")
    )
    is_valid <- FALSE
  }
  acoc_check <-
    unique(data$attributeOptionCombo)[!(
      unique(data$attributeOptionCombo) %in%
        getMechanismsMap(d2session = d2session)$id
    )]
  if (length(acoc_check) > 0) {
    warning(
      "The following attribute option combo identifiers could not be found:",
      paste(acoc_check, sep = "", collapse = ",")
    )
    is_valid <- FALSE
  }

    list("dataElement" = data_element_check,
       "orgUnit" = orgunit_check,
       "categoryOptionCombo" = coc_check,
       "attributeOptionCombo" = acoc_check,
       "is_valid" = is_valid)
}

#' @export
#' @importFrom utils read.csv
#' @importFrom utils select.list
#' @title General purpose parsing function for different formats of DHIS2 data
#'
#' @description d2Parser will parse a compliant DHIS2 XML, JSON, or CSV file
#' and transform it into a standard data frame which can be used in
#' subsequent DATIM validation routines.
#'
#' @param type Type of the file. Should be one of  xml, json or csv.
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of at least "dataElement", "period", "orgUnit",
#' "categoryOptionCombo", "attributeOptionCombo", "value"
#'
#' @note function(filename = "/home/me/foo.xml", type = "xml",
#' dataElementIdScheme = "code", orgUnitIdScheme = "code", idScheme = "id")
#' Note that all values will be returned as characters.
#' @examples \dontrun{
#'     #Be sure to login to DATIM first
#'     datimutils::loginToDATIM(config_path = "/home/me/datim-test.json")
#'     #Load a CSV file
#'     d <- d2Parser("myfile.csv",type="csv",header=TRUE)
#'     #Load a JSON file
#'     d <- d2Parser("myfile.json",type="json",dataElementIdScheme="code")
#'     #Load an XML file
#'     d <- d2Parser("myfile.xml",type="xml",dataElementIdScheme="name")
#' }
#'
d2Parser <-
  function(
           filename,
           type,
           datastream,
           organisationUnit = NA,
           dataElementIdScheme = "id",
           orgUnitIdScheme = "id",
           idScheme = "id",
           invalidData = FALSE,
           csv_header = TRUE,
           isoPeriod = NA,
           d2session = dynGet("d2_default_session", inherits = TRUE)) {

      if (is.na(organisationUnit)) {
        #Get the users organisation unit if not specified
        organisationUnit <- d2session$user_orgunit
      }
      valid_type <- type %in% c("xml", "json", "csv")
      if (!valid_type) {
        stop("ERROR:Not a valid file type")
      }

      if (datastream == "SIMS") {
        header <-
          c(
            "dataElement",
            "period",
            "orgUnit",
            "categoryOptionCombo",
            "attributeOptionCombo",
            "value",
            "assessmentid"
          )
      } else {
        header <-
          c(
            "dataElement",
            "period",
            "orgUnit",
            "categoryOptionCombo",
            "attributeOptionCombo",
            "value",
            "storedby",
            "lastUpdated",
            "comment"
          )

      }

      if (type == "xml") {

        data <- xml2::read_xml(filename) %>%
          xml2::xml_children() %>%
          purrr::map(., xml2::xml_attrs) %>%
          purrr::map_df(~ (as.list(.)))


        #Names in the XML must correspond exactly
        if (!Reduce("&", names(data) %in% header)) {
          stop("XML attributes must be one of the following:",
               paste(header, sep = "", collapse = ","))
        }
      }

      if (type == "csv") {
        data <- read.csv(filename, header = csv_header, stringsAsFactors = FALSE)
        data[] <- lapply(data, stringr::str_trim)
        #Get number of columns and assign the header
        names(data)[seq_len(ncol(data))] <- header[seq_len(ncol(data))]
        #Data element, period and orgunit must be specified
        missing_required <- !complete.cases(data[, 1:3])
        if (sum(missing_required) > 0) {
          msg <- paste0("File contains rows with missing ",
                        "required fields in rows ",
                        paste(which(missing_required == TRUE),
                              sep = "", collapse = ","),
                        ". These rows will be excluded.")
          warning(msg)
        }
        data <- data[!missing_required, ]
      }

      if (type == "json") {
        j <- jsonlite::fromJSON(txt = filename)

        data <- j$dataValues

        if (!is.null(j[["period"]])) {
          data$period <- j$period
        }
        if (!is.null(j[["orgUnit"]])) {
          data$orgUnit <- j$orgUnit
        }
        if (!is.null(j[["attributeOptionCombo"]])) {
          data$attributeOptionCombo <- j$attributeOptionComboid
        }

        #Names in the JSON must correspond exactly
        if (!Reduce("&", names(data) %in% header)) {
          stop("JSON attributes must be one of the following:",
               paste(header, sep = "", collapse = ","))
        }
      }

      data <- data[, header[header %in% names(data)]]

      if (orgUnitIdScheme != "id") {
        data$orgUnit <-
          remapOUs(
            data$orgUnit,
            organisationUnit,
            mode_in = orgUnitIdScheme,
            mode_out = "id",
            d2session = d2session
          )
      }
      if (dataElementIdScheme != "id") {
        data$dataElement <-
          remapDEs(
            data$dataElement,
            mode_in = dataElementIdScheme,
            mode_out = "id",
            d2session = d2session
          )
      }
      if (idScheme != "id") {
        data$attributeOptionCombo <- remapMechs(
          data$attributeOptionCombo,
          organisationUnit = organisationUnit,
          mode_in = idScheme,
          mode_out = "id",
          d2session = d2session
        )
      }

      #Data frame needs to be completely flattened to characters
      data <- plyr::colwise(as.character)(data)

      isMissing <- function(x) { x == "" | is.na(x) } #nolint

      if (NROW(data) == 1) {
        valid_rows <- sum(sapply(data, isMissing)) == 0L
      } else {

        valid_rows <- purrr::reduce(purrr::map(data, isMissing), `+`) == 0L
      }

      if (sum(valid_rows) != NROW(data)) {

        msg <-
          paste0(sum(!valid_rows), " rows are incomplete. ",
                 "Please check your file to ensure its correct.")
        warning(msg)
      }

      if (!invalidData) {
        data <- data[valid_rows, ]
      }

      code_scheme_check <- checkCodingScheme(data, d2session = d2session)

      if (!code_scheme_check$is_valid) {
        return(code_scheme_check)
      } else {
        data
      }

  }
