#' @export
#' @title Get Data Element Map
#'
#' @description Utility function of extraction of data element ids, codes,
#' shortNames, and names
#'
#' @inheritParams datim_validation_params
#'
#' @return Returns a data frame  of id, code, shortName, and name
#' @examples \dontrun{
#' de_map <- getDataElementMap()
#' }

getDataElementMap <- function(d2session = dynGet("d2_default_session",
                                                 inherits = TRUE)) {
  url <- utils::URLencode(
    paste0(d2session$base_url,
           "api/", api_version(),
           "/dataElements?fields=id,code,shortName,name,",
           "valueType,optionSet[id],zeroIsSignificant&paging=false"))

  r <- httpcache::GET(url, httr::timeout(getHTTPTimeout()), handle = d2session$handle)
  if (r$status == 200L) {
    r <-  httr::content(r, "text")
    des <- jsonlite::fromJSON(r, flatten = TRUE)[[1]]
     } else {
      print(paste("Could not retreive data elements", httr::content(r, "text")))
      des <- NULL
    }

  des
}
