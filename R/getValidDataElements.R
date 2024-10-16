#' @export
#' @title Get Invalid Data Elements
#'
#' @description Utility function to produce a data frame of valid data elements
#' based on current DATIM form specification.
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of "dataSet", "dataElementName",
#' "shortname", "code", "dataelementuid", "categoryOptionComboName"
#'
#'
getValidDataElements <- function(datasets = NA,
                                 d2session = dynGet("d2_default_session",
                                                    inherits = TRUE)) {
  allDataSets <- getDataSets(d2session = d2session)
  dataSetValid <- Reduce("&", datasets %in% allDataSets$id)
  while (!dataSetValid || is.na(dataSetValid)) {
    datasets <- selectDataset(d2session = d2session)
    if (length(datasets) == 0) {break;} #nolint
    dataSetValid <- Reduce("&", datasets %in% allDataSets$id)
  }
  if (length(datasets) == 0 || any(is.na(datasets))) { stop("Invalid dataset"); } #nolint
  #Valid data set assignments against the dataset
  #Custom forms
  des.all <-
    data.frame(
      dataset = character(),
      dataelement = character(),
      shortname = character(),
      code = character(),
      dataelementuid = character(),
      categoryoptioncombo = character(),
      categoryoptioncombouid = character()
    )


  for (i in seq_along(datasets)) {

    #We need to use these views here since they are based on
    #the actual HTML form. Certain disaggs may be excluded.
    if (allDataSets[allDataSets$id == datasets[i], "formType"] == "CUSTOM") {
      path <- paste0("sqlViews/DotdxKrNZxG/data.json?var=dataSets:",
               datasets[i],
               "&paging=false")
    } else {
      path <- paste0(
               "sqlViews/ZC8oyMiZVQD/data.json?var=dataSets:",
               datasets[i],
               "&paging=false")
    }


      r <- d2_api_get(path, d2session = d2session)

      if (!is.null(r)) {

        des <- as.data.frame(r$listGrid$rows, stringsAsFactors = FALSE)
        foo <- r$listGrid$headers
        names(des) <- as.character(foo$name)
        #Select only the columns we are interested in
        des <- des[, names(des.all)]

        if (nrow(des) > 0) {
          des.all <- rbind(des.all, des)
        }
      } else {
        stop("Could not get valid data elements")
      }

    }

  plyr::colwise(as.character)(des.all)
}
