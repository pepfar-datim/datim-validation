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
    
      data <- read.csv(filename,stringsAsFactors = FALSE)
      #Get number of columns and assign the header
      names(data)[1:ncol(data)]<-header[1:ncol(data)] 
      #Data element, period and orgunit must be specified
      missing_required<-!complete.cases(data[,1:3])
      if (sum(missing_required) > 0) { 
        msg<-paste0("File contains rows with missing required fields in rows ", 
                    paste(which(missing_required == TRUE),sep="",collapse = ","),". These rows will be excluded.")
        warning(msg)
        }
      data<-data[!missing_required,] 
    
    
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
    
    invalid <-
      function(x) {
        sapply(x, function(x) {
          is.na(x) || missing(x) || x == ""
        })
      }
    invalid.rows <-
      apply(apply(data, 2, invalid), 1, sum) == 0 #Anything which is not complete.
    if (sum(invalid.rows) != nrow(data)) {
      foo <- nrow(data) - sum(invalid.rows)
      msg <-
        paste(foo,
              " rows are incomplete. Please check your file to ensure its correct.")
      warning(msg)
    }
    if (sum(invalid.rows)) {
      data <- data[invalid.rows, ]
    }
    data_shifted<-data[0,]
    assessments<-unique(data[,c("period","orgUnit","attributeOptionCombo","assessmentid")])
    assessments<-aggregate(. ~ period + orgUnit + attributeOptionCombo,data=assessments,length)
    names(assessments)<-c("period","orgUnit","attributeOptionCombo","assessments")

    for (i in 1:nrow(assessments)) {
      foo<-assessments[i,]
      bar<-merge(data,assessments[,c("period","orgUnit","attributeOptionCombo")],
                 by=c("period","orgUnit","attributeOptionCombo"))
      if (foo$assessments > 1) {
      assessments_dates<-seq(strptime(foo$period,"%Y%m%d"), by = "day", length.out = foo$assessments)
      assessment_ids<-sort(unique(bar$assessmentid))
      assessments_shifted<-data.frame(assessmentid=assessment_ids,assessments_dates=assessments_dates)
      bar<-merge(bar,assessments_shifted,by=c("assessmentid"))
      bar$period<-format(bar$assessments_dates,"%Y%m%d")
      bar<- bar[, c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value")] }
      data_shifted<-rbind(data_shifted,bar) 
      #TODO: How to handle date collisions after shifting?
    }
    
    return(data_shifted)
    
}
