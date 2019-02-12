
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
  is_valid<-TRUE
  data_element_check <-
    unique(data$dataElement)[!(unique(data$dataElement) %in% getDataElementMap()$id)]
  if (length(data_element_check) > 0) {
    warning(
      "The following data element identifiers could not be found:",
      paste(data_element_check, sep = "", collapse = ",")
    )
    is_valid<-FALSE
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
    is_valid<-FALSE
  }
  coc_check <-
    unique(data$categoryOptionCombo)[!(unique(data$categoryOptionCombo) %in% getCategoryOptionCombosMap()$id)]
  if (length(coc_check) > 0) {
    warning(
      "The following category option combo identifiers could not be found:",
      paste(coc_check, sep = "", collapse = ",")
    )
    is_valid<-FALSE
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
    is_valid<-FALSE
  }
  
    list("dataElement"=data_element_check,
       "orgUnit"=orgunit_check,
       "categoryOptionCombo"=coc_check,
       "attributeOptionCombo"=acoc_check,
       "is_valid"=is_valid
       
       )
}


shiftSIMSData <- function(data,isoPeriod) {
  #The column afer the value column is used for storing the assessment
  data<-data[,1:7]
  
  names(data)<-
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value",
      "assessmentid"
    )
  
  #if period is provided, use it for boundaries
  if(!is.na(isoPeriod)){
    period<-getPeriodFromISO(isoPeriod);
  } else {
    period<-NA
  }
  
  #Start to shift the data
  data_shifted<-data[0,]
  assessments<-unique(data[,c("period","orgUnit","attributeOptionCombo","assessmentid")])
  #Are there any assessment ids which occur on different dates?
  #This should not be possible
  if ( sum(duplicated(assessments$assessmentid)) != 0 ) {stop("Duplicate assessment IDS were found.")}
  assessments_ou_acoc<-aggregate(. ~ orgUnit + attributeOptionCombo,data=assessments[,-4],length)
  #Possible collisions
  assessments_ou_acoc_dups<-assessments_ou_acoc[assessments_ou_acoc$period > 1,]
  asessments_collisions<-assessments[0,]
  if (nrow(assessments_ou_acoc_dups)> 0 ) {
    for (i in 1:nrow(assessments_ou_acoc_dups)) {
      foo<-assessments_ou_acoc_dups[i,]
      bar<-assessments[assessments$orgUnit==foo$orgUnit & assessments$attributeOptionCombo==foo$attributeOptionCombo,]
      #Are there any duplicated dates?
      
      if (sum(duplicated(bar$period)) > 0) {
        dates<-as.Date(unique(strptime(bar$period,"%Y%m%d",tz = "UTC")))
        start_date<-min(dates)
        end_date<-max(dates)
        #We need a minumum pool of dates
        if ( (start_date - end_date) < nrow(bar) ) {
          end_date<-start_date + nrow(bar)
        }
        #make sure end date does not go beyond boundaries
        if(!is.na(period)){
          if(end_date > period$endDate){
            start_date = start_date - (end_date - period$endDate)
          }
          if(start_date < period$startDate){
            warning("Shifting results in periods outside of the defined isoPeriod")
          }
        }
        
        possible_dates<-seq(start_date,end_date,by="day")
        #Remove any dates which are already used
        possible_dates<-possible_dates[!(possible_dates %in% dates)]
        
        duplicated_dates<-which(duplicated(bar$period))
        for (j in 1:length(duplicated_dates)) {
          this_date<-as.Date(bar$period[duplicated_dates[j]],"%Y%m%d")
          #Which date is closest?
          date_distance<-methods::as(abs(possible_dates-this_date),"integer")
          replacement_date_n<-which(date_distance == min(date_distance))
          
          bar$period[duplicated_dates[j]]<-format(possible_dates[replacement_date_n],"%Y%m%d")
          #Remove it from the pool
          possible_dates<-possible_dates[-replacement_date_n]
        }
      }
      asessments_collisions<-rbind(asessments_collisions,bar) }
    #Non-collisions
    
    data_clear <-
      merge(data,
            assessments_ou_acoc[assessments_ou_acoc$period == 1, c("orgUnit", "attributeOptionCombo")],
            by = c("orgUnit", "attributeOptionCombo"))
    data_not_clear <- merge(
      data,
      asessments_collisions,
      by = c("orgUnit", "attributeOptionCombo", "assessmentid")
    )
    data_not_clear <-
      data_not_clear[, c(
        "orgUnit",
        "attributeOptionCombo",
        "dataElement",
        "period.y",
        "categoryOptionCombo",
        "value",
        "assessmentid"
      )]
    names(data_not_clear) <- names(data_clear)
    data_shifted <- rbind(data_clear, data_not_clear)
    assertthat::assert_that(nrow(data) == nrow(data_shifted))
    data_shifted$comment <- data_shifted$assessmentid
    data_shifted$storedby <- NA
    data_shifted$timestamp <- NA
  } else {
    data_shifted <- data
    data_shifted$storedby <- NA
    data_shifted$timestamp <- NA
    data_shifted$comment <- data_shifted$assessmentid
  }
  
  header_final <-
    c(
      "dataElement",
      "period",
      "orgUnit",
      "categoryOptionCombo",
      "attributeOptionCombo",
      "value",
      "storedby",
      "timestamp",
      "comment"
    )
  data_shifted[, header_final[header_final %in% names(data_shifted)]]
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
#' @param mode be either "NORMAL" or "SIMS".
#' @param isoPeriod period to be used for date shift boundaries. If not provided,
#'  no boundaries are set. Only relevant for SIMS data
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
           csv_header = TRUE,
           mode="NORMAL",
           isoPeriod=NA) {
    
    
    if (is.na(organisationUnit)) {
      #Get the users organisation unit if not specified 
      organisationUnit <- getOption("organisationUnit")
    }
    
    valid_type <- type %in% c("xml", "json", "csv")
    if (!valid_type) {
      stop("ERROR:Not a valid file type")
    }
    
    valid_mode <- mode %in% c("NORMAL","SIMS") 
      if (!valid_mode ) {
        stop("ERROR:Not a valid parsing mode")
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
          x != "" & !is.na(x) & !missing(x)
        })
        
      }
    
    if (NROW(data) == 1 ) { 
      valid_rows <- sum(sapply(data,notMissing)) == 6L
       } else {
         valid_rows <- rowSums(apply(data[,1:6], 2, notMissing)) == 6L
      }
    
    if ( sum(valid_rows) != NROW(data) ) {
      foo <- NROW(data) - sum(!valid_rows)
      msg <-
        paste0(foo,
              " rows are incomplete. Please check your file to ensure its correct.")
      warning(msg)
    }
    
    if (!invalidData) {
      data <- data[valid_rows, ]
    }
    
    code_scheme_check<-checkCodingScheme(data)
    
    if (!code_scheme_check$is_valid) {
      return(code_scheme_check)
    } 
    
    if (mode=="SIMS") {
     data <- shiftSIMSData(data=data,isoPeriod=isoPeriod)
    }
    
    data
  
  }

#' Title
#'
#' @param ... Same paramaters as d2Parser, except the mode is set to "SIMS"
#'
#' @return Returns a data frame of at least "dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value"
#' @export
#' @seealso d2Parser

sims2Parser<-function(...) {
  
  d2Parser(...,mode="SIMS")
}
