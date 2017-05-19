#' @export
#' @title getPeriodType(iso)
#' 
#' @description Utility function return a DHIS2 period type from an ISO string
#'
#' @param iso String which identifies the period, such as 2016Q1 or 2016Q2
#' @return Returns a a character of the period, like "Monthly"
#' 
#' 
getPeriodType<-function(iso){
  if ( grepl("^\\d{8}$",iso,perl=TRUE)  ) {return("Daily") }
  else if ( grepl("^\\d{4}W\\d{1,2}$",iso,perl=TRUE) )  {return("Weekly") }
  else if ( grepl("^\\d{6}$",iso,perl=TRUE) )  {return("Monthly") }
  else if ( grepl("^\\d{4}Q\\d{1}$",iso,perl=TRUE) )  {return("Quarterly") }
  else if ( grepl("^\\d{4}$",iso,perl=TRUE) )  {return("Yearly") }
  else if ( grepl("^\\d{4}Oct$",iso,perl=TRUE) )  {return("FinancialOct") }
  else {return(NULL)}
}

#' @export
#' @title getPeriodFromISO(iso)
#' 
#' @description Utility function return a data frame of a periods start date, end date, ISO character and periodtype
#'
#' @param iso String which identifies the period, such as 2016Q1 or 2016Q2
#' @return Returns a data frame consisting of iso (character),startDate (Date),endDate (Date) and period type (character)
#' 
#' 
getPeriodFromISO <- function(iso) {
  pt <- getPeriodType(iso)
  if (is.null(pt)) {return(NULL)}
  assertthat::noNA(pt)
  startDate <- NA
  endDate <- NA
  if (pt == "Daily") {
    startDate <- as.Date(iso, "%Y%m%d")
    endDate <- as.Date(iso, "%Y%m%d")
  } else if (pt == "Weekly") {
    y <- substr(iso, 1, 4)
    wk<-as.numeric(gsub("W","",stringr::str_extract(iso,"W.+")))
    if (wk<=10) { wk<-paste0("0",as.character(wk)) }
    startDate <- ISOweek2date(paste0(y,"-W",wk, "-1"))
    endDate <- ISOweek2date(paste0(y,"-W",wk,"-7"))
  } else if (pt == "Monthly") {
    startDate <- as.Date(paste0(iso, "01"), "%Y%m%d")
    endDate <- startDate + months(1) - days(1)
  } else if (pt == "Quarterly") {
    y <- substr(iso, 1, 4)
    q <- substr(iso, 6, 6)
    if (q == "1") {
      m <- "01"
    } else if (q == "2") {
      m <- "04"
    } else if (q == "3") {
      m <- "07"
    } else if (q == 4) {
      m <- "10"
    }  else {
      (stop("Invalid quarter specified."))
    }
    add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
    startDate<-as.Date(paste0(y,m,"01"),"%Y%m%d")
    endDate<-add.months(startDate,3) - 1
  } else if (pt =="Yearly") {
    startDate<-as.Date(paste0(iso,"0101"),"%Y%m%d")
    endDate<-startDate + years(1) - days(1)
    
  } else if (pt == "FinancialOct") {
    y <- as.integer(substr(iso, 1, 4))
    startDate<-as.Date(paste0(y,"1001"),"%Y%m%d")
    endDate<-startDate + years(1) - days(1)
  }
  period<-data.frame(iso=iso,startDate=startDate,endDate=endDate,periodType=pt)
  return(period)
}

