#' @export
#' @title getDataElementsOrgunits(data,organisationUnit,datasets)
#' 
#' @description Returns a map of lists consisting of data elements and orgunits for a dataset (or datasets) for a given organisationUnit
#' @param organisationUnit Organisation unit. Defaults to user organisation unit if not supplied explicitly.
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the data is considered to be invalid.

getDataElementsOrgunits <- function(organisationUnit = NA,
                                    datasets = NA) {
  if (is.na(organisationUnit)) {
    organisationUnit = getOption("organisationUnit")
  }
  
  allDataSets <- getDataSets()
  
  if (is.na(datasets)) {
    datasets <- selectDataset()
  }
  
  dataSetValid <- Reduce("&", datasets %in% allDataSets$id)
  
  if (!dataSetValid) {
    stop("Invalid dataset")
  }
  
    for (i in seq_along(datasets)) {
  
      if (i == 1)  { des_ous.all <- list() }
      
      url <-
        paste0(
          getOption("baseurl"),
          "api/organisationUnits?fields=id&paging=false&filter=path:like:",
          organisationUnit,
          "&filter=dataSets.id:eq:",
          datasets[i]
        )
      
      sig <- digest::digest(paste0(url), algo = 'md5', serialize = FALSE)
      des_ous <- getCachedObject(sig)
      
      if (is.null(des_ous)) {
        r <- httr::GET(url, httr::timeout(60))
        r <- httr::content(r, "text")
        ous <-
          unique(jsonlite::fromJSON(r, flatten = TRUE)$organisationUnits$id)
        #OUs
        des <-
          unique(getValidDataElements(datasets[i])$dataelementuid)
        des_ous <- list(dataset = datasets[i], list(ous = ous, des = des))
        saveCachedObject(des_ous, sig)
      }
      
      des_ous.all <- rlist::list.append(des_ous.all, des_ous)
    }

  return(des_ous.all) }


#' @export
#' @title getInvalidOrgunitsFromDatasets(data,organisationUnit,datasets)
#' 
#' @description Returns a subset of data which exists in the data, but
#' which do not have corresponding organiation unit assignments
#' 
#' @param data D2 compliant data frame
#' @param organisationUnit Organisation unit. Defaults to user organisation unit if not supplied explicitly.
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame of "data" which was submitted along for a reason
#'  of why the data is considered to be invalid.
getInvalidOrgunitsFromDatasets<-function(data,organisationUnit=NA,datasets=NA) {
  if (is.na(organisationUnit)) { organisationUnit = getOption("organisationUnit") }
  des_ous<-getDataElementsOrgunits(organisationUnit,datasets)
  all_ous <- c()
  for(l in des_ous){
    all_ous <- c(all_ous, l[[2]]$ous)
  }
  return(subset(data,!(orgUnit %in% all_ous)))
}

#' @export
#' @title getInvalidOrgunitsFromDatasets(data,organisationUnit,datasets)
#' 
#' @description Returns a data frame invalid data elements which exist in the data 
#' but not the datasets
#'
#' @param data D2 compliant data frame
#' @param dataElement Should be the UID of the organisation unit ancestor.
#' @param datasets Should be a character vector of data set UIDs. Alternatively, if left missing, user will be promted.
#' @return Returns a data frame of "data" which was submitted along for a reason of why the 
#' data is considered to be invalid.
#' 

getInvalidDataElementFromDatasets<-function(data,dataElement,datasets) {
  if (is.na(organisationUnit)) { organisationUnit = getOption("organisationUnit") }
  des_ous<-getDataElementsOrgunits(organisationUnit,datasets)
  subset(data,!(dataElement %in% des_ous$des))
}

getInvalidDatasetMembers<-function(data,organisationUnit,datasets=NA){
  allDataSets<-getDataSets()
  dataSetValid<-Reduce("&",datasets %in% allDataSets$id)
  while(!dataSetValid || is.na(dataSetValid) ) {
    datasets<-selectDataset()
    if (length(datasets) == 0) {break;}
    dataSetValid <- Reduce("&",datasets %in% allDataSets$id) }
  if (length(datasets) == 0 || is.na(datasets)) { stop("Invalid dataset"); }
  foo=data.frame(dataElement=character(),period=character(),orgUnit=character(),categoryOptionCombo=character(),
                 attributeOptionCombo=character(),value=character(),type=character())
  ous<-getInvalidOrgunitsFromDatasets(data,organisationUnit,datasets)
  if (!is.null(ous) & nrow(ous) > 0) {
    ous$type="ORGUNIT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,ous)
  }
  
  des<-getInvalidDataElementFromDatasets(data,organisationUnit,datasets)
  if (!is.null(des) & nrow(des) > 0) {
    des$type="DATAELEMENT DOES NOT EXIST IN DATASETS"
    foo<-rbind(foo,des)
  }
  
  return(foo)
}
  
