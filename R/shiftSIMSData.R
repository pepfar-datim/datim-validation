#' Title
#'
#' @param d datimvalidation object
#'
#' @return datimvalidation object with with dates deconflicted in 
#' d$data$import. 
#' @export
#'

shiftSIMSData <- function(d) {

  #if period is provided, use it for boundaries
  if (!is.na(d$info$isoPeriod)) {
    period <- getPeriodFromISO(isoPeriod)
  } else {
    period <- NA
  }

  #This is the
  data <- d$data$parsed

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
    for (i in seq_len(assessments_ou_acoc_dups)) {
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

        for (j in seq_len(duplicated_dates)) {
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

  d$data$import <- data_shifted
  d$sims$collisions <- assessments_ou_acoc_dups

  d

}
