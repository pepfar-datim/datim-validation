#' @export
#' @title Special purpose parsing function for SIMS2 aggregate data with duplicates
#'
#' @description sims2Parser will parse a semi-compliant DHIS2  CSV file and transform it into a standard data
#' frame which can be used in subsequent DATIM validation routines. The difference with d2Parser is that
#' an extra (non-standard) field will be introduced to record the SIMS visit. This will in turn be
#' used to deduplicate visits which occur at the same site + mechanism + date combination. This function
#' will automatically decollide these types of visits 
#'
#' @param filename Location of the payload to be imported. Should be a valid SIMS2 aggregate file. See the separate spec for details
#' @param dataElementIdScheme Should be one of either code, name, shortName or id. The default is "code". If this paramater is "id",
#' then the Data elements are assumed to be already specififed as UIDs.
#' @param orgUnitIdScheme Should be one of either code, name, shortName or id. If this paramater is "id",
#' then the organisation units are assumed to be already specififed as UIDs
#' @param idScheme Remapping scheme for category option combos
#' @param invalidData Exclude any (NA or missing) data from the parsed file?
#'
#' @return Returns a data frame of  "dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value"
#'
#' @note function(filename="/home/me/foo.csv",dataElementIdScheme="code",orgUnitIdScheme="code",idScheme="id",invalidData=FALSE)
#' Note that all values will be returned as characters.
#'
sims2Parser <-
  function(filename,
           dataElementIdScheme = "code",
           orgUnitIdScheme = "id",
           idScheme = "id",
           invalidData = FALSE) {
    
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
    
    #We need to be a global user.
    organisationUnit <- getOption("organisationUnit")
    assertthat::assert_that(organisationUnit =="ybg3MO3hcf4")
    data <- read.csv(filename,na="",stringsAsFactors = FALSE)
    #Ensure we have the correct number of columns
    data<-data[,1:length(header)]
    #Get number of columns and assign the header
    names(data)<-header
    #Data element, period and orgunit must be specified
    missing_required<-!complete.cases(data[,1:3])
    if (sum(missing_required) > 0) { 
      msg<-paste0("File contains rows with missing required fields in rows ", 
                  paste(which(missing_required == TRUE),sep="",collapse = ","),". These rows will be excluded.")
      warning(msg)
    }
    data<-data[!missing_required ,] 
    
    data <- data[, header[header %in% names(data)]]
    if (orgUnitIdScheme != "id") {
      data$orgUnit <-
        remapOUs(
          data$orgUnit,
          organisationUnit,
          mode_in = orgUnitIdScheme,
          mode_out = "id"
        )
      #Filter out any thing which does not correspond to the orgUnit mapping scheme
      ou_non_match<-unique(data$orgUnit)[!(unique(data$orgUnit) %in% getOrganisationUnitMap()$id)]
      if (length(ou_non_match > 0)) {
        msg<-paste0("The following orgunits are not valid and will be removed",paste(ou_non_match,sep="",collapse=","))
        warning(msg)
      }
      data<-data[!(data$orgUnit %in% ou_non_match),]
    }
    if (dataElementIdScheme != "id") {
      data$dataElement <-
        remapDEs(
          data$dataElement,
          mode_in = dataElementIdScheme,
          mode_out = "id"
        )
      
      de_non_match<-unique(data$dataElement)[!(unique(data$dataElement) %in% getDataElementMap()$id)]
      if (length(de_non_match > 0)) {
        msg<-paste0("The following data elements are not valid and will be removed: ",paste(de_non_match,sep="",collapse=" , "))
        warning(msg)
      }
      data<-data[!(data$dataElement %in% de_non_match),]
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
    
    invalid <-
      function(x) {
        sapply(x, function(x) {
          is.na(x) || missing(x) || x == "" || x=="NULL"
        })
      }
    invalid.rows <-
      apply(apply(data, 2, invalid), 1, sum) != 0 #Anything which is not complete.
    if ( sum(invalid.rows) ) {
      msg<-paste(sum(invalid.rows),
                 " rows are incomplete. Please check your file to ensure its correct.")
      warning(msg)
    }
    
    if (sum(invalid.rows)) {
      data <- data[!invalid.rows, ]
    }
    #Start to shift the data
    data_shifted<-data[0,]
    assessments<-unique(data[,c("period","orgUnit","attributeOptionCombo","assessmentid")])
    assessments_ou_acoc<-aggregate(. ~ orgUnit + attributeOptionCombo,data=assessments[,-4],length)
    #Possible collisions
    assessments_ou_acoc_dups<-assessments_ou_acoc[assessments_ou_acoc$period > 1,]
    asessments_collisions<-assessments[0,]
    if (nrow(asessments_collisions)> 0 ) {
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
        possible_dates<-seq(start_date,end_date,by="day")
        #Remove any dates which are already used
        possible_dates<-possible_dates[!(possible_dates %in% dates)]
        
        duplicated_dates<-which(duplicated(bar$period))
        for (j in 1:length(duplicated_dates)) {
          this_date<-as.Date(bar$period[duplicated_dates[j]],"%Y%m%d")
          #Which date is closest?
          date_distance<-as(abs(possible_dates-this_date),"integer")
          replacement_date_n<-which(date_distance == min(date_distance))
          
          bar$period[duplicated_dates[j]]<-format(possible_dates[replacement_date_n],"%Y%m%d")
          #Remove it from the pool
          possible_dates<-possible_dates[-replacement_date_n]
        }
      }
      asessments_collisions<-rbind(asessments_collisions,bar) }
    #Non-collisions
    
    data_clear<-merge(data,assessments_ou_acoc[assessments_ou_acoc$period == 1,c("orgUnit","attributeOptionCombo")],
                      by=c("orgUnit","attributeOptionCombo"))
    data_not_clear<-merge(data,asessments_collisions,
                          by=c("orgUnit","attributeOptionCombo","assessmentid"))
    data_not_clear<-data_not_clear[,c("orgUnit","attributeOptionCombo","dataElement","period.y","categoryOptionCombo","value","assessmentid")]
    names(data_not_clear)<-names(data_clear)
    data_shifted<-rbind(data_clear,data_not_clear)
    assertthat::assert_that(nrow(data) == nrow(data_shifted))
    data_shifted$comment<-data_shifted$assessmentid
    data_shifted$storedby<-NA
    data_shifted$timestamp<-NA } else { data_shifted<-data }
    
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
    data_shifted <- data_shifted[, header_final[header_final %in% names(data_shifted)]]
    return(data_shifted)
  }
