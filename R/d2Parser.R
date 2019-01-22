
#' @title Utility function to check that the supplied coding scheme is correct
#'
#' @description checkCodingScheme will ensure that all indentifiers after parsing are valid UIDs,
#' or in the case of periods, valid periods. 
#'
#' @param data A parse DHIS2 data payload
#' @return Only returns an error if the 
checkCodingScheme <- function(data) {
  #This is a very superficial and quick check, just to be sure that the coding scheme is correct.
  #Additional validation will be required to be sure data elements, catcombos and orgunits are properly
  #Associated.
  data_element_check <-
    unique(data$dataElement)[!(unique(data$dataElement) %in% getDataElementMap()$id)]
  if (length(data_element_check) > 0) {
    warning(
      "The following data element identifiers could not be found:",
      paste(data_element_check, sep = "", collapse = ",")
    )
  }
  orgunit_check <-
    unique(data$orgUnit)[!(
      unique(data$orgUnit) %in% getOrganisationUnitMap(organisationUnit = getOption("organisationUnit"))$id
    )]
  if (length(orgunit_check) > 0) {
    warning(
      "The following org unit identifiers could not be found:",
      paste(orgunit_check, sep = "", collapse = ",")
    )
  }
  coc_check <-
    unique(data$categoryOptionCombo)[!(unique(data$categoryOptionCombo) %in% getCategoryOptionCombosMap()$id)]
  if (length(coc_check) > 0) {
    warning(
      "The following category option combo identifiers could not be found:",
      paste(coc_check, sep = "", collapse = ",")
    )
  }
  acoc_check <-
    unique(data$attributeOptionCombo)[!(
      unique(data$attributeOptionCombo) %in% getMechanismsMap(organisationUnit = getOption("organisationUnit"))$id
    )]
  if (length(acoc_check) > 0) {
    warning(
      "The following attribute option combo identifiers could not be found:",
      paste(acoc_check, sep = "", collapse = ",")
    )
  }
  
    list("dataElement"=data_element_check,
       "orgUnit"=orgunit_check,
       "categoryOptionCombo"=coc_check,
       "attributeOptionCombo"=acoc_check
       
       )
}

#' @export
#' @importFrom utils read.csv
#' @importFrom utils select.list
#' @title General purpose parsing function for different formats of DHIS2 data
#'
#' @description d2Parser will parse a compliant DHIS2 XML,JSON or CSV file and transform it into a standard data
#' frame which can be used in subsequent DATIM validation routines
#'
#' @param filename Location of the payload to be imported. Should be a valid DHIS2 import file
#' @param type Type of the file. Should be either xml, json or csv
#' @param organisationUnit Organisation unit UID of the operating unit. If left blank, assumed to be global.
#' @param dataElementIdScheme Should be one of either code, name, shortName or id. If this paramater is "id",
#' then the Data elements are assumed to be already specififed as UIDs.
#' @param orgUnitIdScheme Should be one of either code, name, shortName or id. If this paramater is "id",
#' then the organisation units are assumed to be already specififed as UIDs
#' @param idScheme Remapping scheme for category option combos
#' @param invalidData Exclude any (NA or missing) data from the parsed file?
#' @param csv_header By default, CSV files are assumed to have a header, otherwise FALSE will allow for 
#' files without a CSV header. 
#'
#' @return Returns a data frame of at least "dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value"
#'
#' @note function(filename="/home/me/foo.xml",type="xml",dataElementIdScheme="code",orgUnitIdScheme="code",idScheme="id")
#' Note that all values will be returned as characters.
#'
d2Parser <-
  function(filename,
           type,
           organisationUnit = NA,
           dataElementIdScheme = "id",
           orgUnitIdScheme = "id",
           idScheme = "id",
           invalidData = FALSE,
           csv_header = TRUE) {
    
    
    if (is.na(organisationUnit)) {
      #Get the users organisation unit if not specified 
      organisationUnit <- getOption("organisationUnit")
    }
    
    valid_type <- type %in% c("xml", "json", "csv")
    if (!valid_type) {
      stop("ERROR:Not a valid file type")
    }
    
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
    
    if (type == "xml") {
      d <- XML::xmlTreeParse(filename, useInternalNode = TRUE)
      data <-
        data.frame(t(sapply(XML::xmlRoot(d) ["dataValue"], XML::xmlAttrs)),
                   row.names = seq(1, XML::xmlSize(XML::xmlRoot(d))))

      
      #Get all the attributes specified in the
      data.attrs <- XML::xmlAttrs(XML::xmlRoot(d))
      if ( !is.null(data.attrs) ) {
      if ( !is.na(data.attrs["period"]) ) {
        data$period <- data.attrs["period"]
      }
      if ( !is.na(data.attrs["orgUnit"]) ) {
        data$orgUnit <- data.attrs["orgUnit"]
      }
      if ( !is.na(data.attrs["attributeOptionCombo"]) ) {
        data$attributeOptionCombo <- data.attrs["attributeOptionCombo"]
      }
      }
      
      #Names in the XML must correspond exactly
      if (!Reduce("&",names(data) %in% header)) {
        stop("XML attributes must be one of the following:", 
             paste(header,sep="",collapse=",")) }
      
    }
    
    if (type == "csv") {
      data <- read.csv(filename,header = csv_header,stringsAsFactors = FALSE)
      data[] <- lapply(data, stringr::str_trim)
      #Get number of columns and assign the header
      names(data)[1:ncol(data)]<-header[1:ncol(data)] 
      #Data element, period and orgunit must be specified
      missing_required<-!complete.cases(data[,1:3])
      if (sum(missing_required) > 0) { 
        msg<-paste0("File contains rows with missing required fields in rows ", 
                    paste(which(missing_required == TRUE),sep="",collapse = ","),". These rows will be excluded.")
        warning(msg)
        }
      data<-data[!missing_required,] }
    
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
      if (!Reduce("&",names(data) %in% header)) {
        stop("JSON attributes must be one of the following:", 
             paste(header,sep="",collapse=",")) }
      
    }
    

    
    data <- data[, header[header %in% names(data)]]

    if (orgUnitIdScheme != "id") {
      data$orgUnit <-
        remapOUs(
          data$orgUnit,
          organisationUnit,
          mode_in = orgUnitIdScheme,
          mode_out = "id"
        )
    }
    if (dataElementIdScheme != "id") {
      data$dataElement <-
        remapDEs(
          data$dataElement,
          mode_in = dataElementIdScheme,
          mode_out = "id"
        )
    }
    if (idScheme != "id") {
      data$attributeOptionCombo <- remapMechs(
        data$attributeOptionCombo,
        organisationUnit = organisationUnit,
        mode_in = idScheme,
        mode_out = "id"
      )
    }
    
    #Data frame needs to be completely flattened to characters
    data <- plyr::colwise(as.character)(data)
    
    notMissing <-
      function(x) {
        sapply(x, function(x) {
          !is.na(x) || !missing(x) || x != ""
        })
        
      }
    
    valid_rows<- data %>% 
      dplyr::select(dataElement,period,orgUnit,categoryOptionCombo,attributeOptionCombo,value) %>%
      dplyr::mutate_all(notMissing) %>% 
      dplyr::mutate(sum = rowSums(.[1:6])) %>% 
      dplyr::mutate(is_valid = (sum==6L) ) %>%
      dplyr::pull(is_valid) 
    
    if (sum(valid_rows) != NROW(data)) {
      foo <- nrow(data) - sum(!valid_rows)
      msg <-
        paste(foo,
              " rows are incomplete. Please check your file to ensure its correct.")
      warning(msg)
    }
    if (!invalidData) {
      data <- data[valid_rows, ]
    }
    
    checkCodingScheme(data)
    
    return(data)
    
  }
