#' @export
#' @title Function which checks the validity of mechanisms
#'
#' @description checkValidMechanisms should be supplied a d2Parser compliant data frame along with the operating unit UID
#' along with the other required paramaters. It will return a vector of non-valid mechanism UIDs which were part of the data
#'
#' @param data d2Parser data frame
#' @param organisationUnit UID of the operating unit.Defaults to the user organisation unit if left blank
#' @return Returns a data frame of invalid mechanisms
#' @note
#' getValidMechanisms(foo,"https://www.datim.org","admin","district","Ab12345678")
#' will remap organisation units specified as codes to UIDs
getInvalidMechanisms <-
  function(data, organisationUnit = NA) {
    if (class(data) != "data.frame") {
      print("Data must be a valid data frame")
      stop()
    }
    if (is.na(organisationUnit)) {
      organisationUnit = getOption("organisationUnit")
    }
    mechs <- getMechanismsMap(organisationUnit)
    data_mechs <- unique(data[, c("attributeOptionCombo", "period")])
    #Get the start and end dates
    mechs_dates <-
      do.call("rbind", lapply(data_mechs$period, getPeriodFromISO))
    mechs_dates <- mechs_dates[, c("startDate", "endDate")]
    names(mechs_dates) <- c("startDate_data", "endDate_data")
    #Merge
    data_mechs <- cbind(data_mechs, mechs_dates)
    #Merge the dates
    data_mechs <- merge(data_mechs,
                        mechs,
                        by.x = "attributeOptionCombo",
                        by.y = "id",
                        all.x = TRUE)
    valid_mechs_date <-
      data_mechs$startDate_data >= data_mechs$startDate &
      data_mechs$endDate_data <= data_mechs$endDate
    
    valid_mechs_ou <- data_mechs$attributeOptionCombo %in% mechs$id
    
    valid_mechs <- valid_mechs_date & valid_mechs_ou
    
    valid_mechs[is.na(valid_mechs)] <- FALSE
    
    data_mechs[!(valid_mechs), ]
  }
