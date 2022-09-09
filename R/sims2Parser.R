#' @export
#' @title SIMS2 Parser
#'
#' @description Special purpose parsing function for SIMS2 aggregate data with
#' duplicates. \code{sims2Parser} will parse a semi-compliant DHIS2 CSV file and
#' transform it into a standard data frame which can be used in subsequent DATIM
#' validation routines. The difference with d2Parser is that an extra
#' (non-standard) field will be introduced to record the SIMS visit. This will
#' in turn be used to deduplicate visits which occur at the same site +
#' mechanism + date combination. This function will automatically decollide
#' these types of visits
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame of "dataElement", "period", "orgUnit",
#' "categoryOptionCombo", "attributeOptionCombo", "value"
#'
#' @note function(filename = "/home/me/foo.csv", dataElementIdScheme = "code",
#' orgUnitIdScheme = "code", idScheme = "id", invalidData = FALSE)
#' Note that all values will be returned as characters.
#'
sims2Parser <-
  function(filename,
           dataElementIdScheme = "code",
           orgUnitIdScheme = "id",
           idScheme = "id",
           invalidData = FALSE,
           hasHeader = TRUE,
           isoPeriod = NA,
           d2session = dynGet("d2_default_session", inherits = TRUE)) {

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
    assertthat::assert_that(organisationUnit == "ybg3MO3hcf4")
    data <- read.csv(filename,
                     na = "",
                     stringsAsFactors = FALSE,
                     header = hasHeader,
                     quote = '"',
                     row.names = NULL,
                     sep = ",")
    #TODO: Centralize all of this between the SIMS parser and general parser
    #Number of lines in the file number equal the number of records
    if (nrow(data) != length(readLines(filename)) - as.numeric(hasHeader)) {
      warning(paste("Number of records does not equal the number of lines.",
                    "You may have empty lines or line breaks!"))
    }
    #Ensure we have the correct number of columns
    data <- data[, seq_len(length(header))]
    #Get number of columns and assign the header
    names(data) <- header
    #Data element, period and orgunit must be specified
    missing_required <- !complete.cases(data[, 1:3])
    if (sum(missing_required) > 0) {
      msg <- paste0(
        "File contains rows with missing required fields in rows ",
        paste(which(missing_required == TRUE), sep = "", collapse = ","),
        ". These rows will be excluded.")
      warning(msg)
    }
    data <- data[!missing_required, ]

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

    # Filter out any thing which does not correspond to
    # the orgUnit mapping scheme
    ou_non_match <-
      unique(data$orgUnit)[!(
        unique(data$orgUnit) %in%
          getOrganisationUnitMap(d2session = d2session)$id
      )]
    if (length(ou_non_match) > 0) {
      msg <- paste0("The following orgunits are not valid and will be removed",
                    paste(ou_non_match, sep = "", collapse = ","))
      warning(msg)
      data <- data[!(data$orgUnit %in% ou_non_match), ]
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

    de_non_match <-
      unique(data$dataElement)[!(
        unique(data$dataElement) %in%
          getDataElementMap(d2session = d2session)$id
      )]
    if (length(de_non_match) > 0) {
      msg <- paste0("The following data elements are not ",
                    "valid and will be removed: ",
                    paste(de_non_match, sep = "", collapse = " , "))
      warning(msg)
      data <- data[!(data$dataElement %in% de_non_match), ]
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

    mechs_non_match <-
      unique(data$attributeOptionCombo)[!(
        unique(data$attributeOptionCombo) %in%
          getMechanismsMap(d2session = d2session)$id
      )]
    if (length(mechs_non_match) > 0) {
      msg <- paste0("The following mechanisms are not ",
                    "valid and will be removed: ",
                    paste(mechs_non_match, sep = "", collapse = " , "))
      warning(msg)
      data <- data[!(data$attributeOptionCombo %in% mechs_non_match), ]
    }

    #Data frame needs to be completely flattened to characters
    data <- plyr::colwise(as.character)(data)

    invalid <-
      function(x) {
        sapply(x, function(x) {
          is.na(x) || missing(x) || x == "" || x == "NULL"
        })
      }

    # Anything which is not complete.
    invalid.rows <- apply(apply(data, 2, invalid), 1, sum) != 0

    if (!invalidData) {
      data <- data[!invalid.rows, ]
    }

    if (sum(invalid.rows) > 0) {
      msg <- paste(sum(invalid.rows),
                   "rows are incomplete.",
                   "Please check your file to ensure its correct.")
      warning(msg)
    }




    #TODO: End centralization here.
    #TODO: Functionalize this with dateShifter

    #if period is provided, use it for boundaries
    if (!is.na(isoPeriod)) {
      period <- getPeriodFromISO(isoPeriod)
    } else {
      period <- NA
    }

    #Start to shift the data
    data_shifted <- data[0, ]
    assessments <- unique(data[, c("period",
                                   "orgUnit",
                                   "attributeOptionCombo",
                                   "assessmentid")])
    #Are there any assessment ids which occur on different dates?
    #This should not be possible
    if (sum(duplicated(assessments$assessmentid)) != 0) {
      stop("Duplicate assessment IDS were found.")
    }
    assessments_ou_acoc <- aggregate(. ~ orgUnit + attributeOptionCombo,
                                     data = assessments[, -4],
                                     length)
    #Possible collisions
    assessments_ou_acoc_dups <-
      assessments_ou_acoc[assessments_ou_acoc$period > 1, ]
    asessments_collisions <- assessments[0, ]
    if (nrow(assessments_ou_acoc_dups) > 0) {
      for (i in seq_len(nrow(assessments_ou_acoc_dups))) {
        foo <- assessments_ou_acoc_dups[i, ]
        bar <-
          assessments[
            assessments$orgUnit == foo$orgUnit &
              assessments$attributeOptionCombo == foo$attributeOptionCombo,
          ]
        #Are there any duplicated dates?

        if (sum(duplicated(bar$period)) > 0) {
          dates <- as.Date(unique(strptime(bar$period, "%Y%m%d", tz = "UTC")))
          start_date <- min(dates)
          end_date <- max(dates)
          #We need a minumum pool of dates
          if ((start_date - end_date) < nrow(bar)) {
            end_date <- start_date + nrow(bar)
          }
          #make sure end date does not go beyond boundaries
          if (!is.na(period)) {
            if (end_date > period$endDate) {
              start_date <- start_date - (end_date - period$endDate)
            }
            if (start_date < period$startDate) {
              warning(paste("Shifting results in periods",
                            "outside of the defined isoPeriod"))
            }
          }

          possible_dates <- seq(start_date, end_date, by = "day")
          #Remove any dates which are already used
          possible_dates <- possible_dates[!(possible_dates %in% dates)]

          duplicated_dates <- which(duplicated(bar$period))
          for (j in seq_len(length(duplicated_dates))) {
            this_date <- as.Date(bar$period[duplicated_dates[j]], "%Y%m%d")
            #Which date is closest?
            date_distance <-
              methods::as(abs(possible_dates - this_date), "integer")
            replacement_date_n <- which(date_distance == min(date_distance))

            bar$period[duplicated_dates[j]] <-
              format(possible_dates[replacement_date_n], "%Y%m%d")
            #Remove it from the pool
            possible_dates <- possible_dates[-replacement_date_n]
          }
        }
        asessments_collisions <- rbind(asessments_collisions, bar)
      }
      #Non-collisions

      data_clear <- merge(data,
                          assessments_ou_acoc[assessments_ou_acoc$period == 1,
                                              c("orgUnit",
                                                "attributeOptionCombo")],
                          by = c("orgUnit", "attributeOptionCombo"))
      data_not_clear <- merge(data, asessments_collisions,
                              by = c("orgUnit",
                                     "attributeOptionCombo",
                                     "assessmentid"))
      data_not_clear <- data_not_clear[, c("orgUnit",
                                           "attributeOptionCombo",
                                           "dataElement",
                                           "period.y",
                                           "categoryOptionCombo",
                                           "value",
                                           "assessmentid")]
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
    data_shifted <-
      data_shifted[, header_final[header_final %in% names(data_shifted)]]
    return(data_shifted)
  }
