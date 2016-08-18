#' @export
#' @title loadSecrets(secrets)

#' @description Load a DHIS secrets file which should be a JSON file as below
#' {
#' dhis": {
#'  "baseurl": "http://localhost:8080",
#'  "username": "admin",
#'  "password": "district"
#'  }
#' }
#'
#'
#' @param secrets Location of the secrets file
#' @return Returns a secrets object.
#'
loadSecrets<-function(secrets) {
  s<-jsonlite::fromJSON(secrets)
  url<-URLencode(paste0(s$dhis$baseurl,"api/me"))
  r<-httr::GET(url ,
               httr::authenticate(s$dhis$username,s$dhis$password),
               httr::timeout(60))
  assertthat::assert_that(r$status == 200L)
  return(s) 
}
