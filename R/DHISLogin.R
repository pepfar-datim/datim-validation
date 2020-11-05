#' @title LoadConfig(config_path)
#'
#' @description Loads a JSON configuration file to access a DHIS2 instance
#' @param config_path Path to the DHIS2 credentials file
#' @return A list of baseurl, username and password
#'
LoadConfigFile <- function(config_path = NA) {
  #Load from a file
  if (!is.na(config_path)) {
    if (file.access(config_path, mode = 4) == -1) {
      stop(paste("Cannot read configuration located at",config_path))
    }
    jsonlite::fromJSON(config_path)
  } else {
    stop("You must specify a credentials file!") }
}


#' R6 Class which handles logging into to a DHIS instance. 
#' @importFrom R6 R6Class
#' @export
#' 
DHISLogin<-R6::R6Class("DHISLogin",
                   public=list(
                     #' @field  config_path Path to a JSON configuration file. 
                     config_path=NA,
                     #' @field  baseurl The URL of the server, e.g. https://www.datim.org/. 
                     baseurl = NULL,
                     #' @field  username Your user name. 
                     username = NULL,
                     #' @field  password Your password.
                     password = NULL,
                     #' @field user_orgunit UID of the users assigned organisation unit
                     user_orgunit = NA,
                     #' @field handle An httr handle used to communicate with the DHIS2 instance. 
                     handle = NULL,
                     #' @field maxCacheAge By default, set to 7 days. Used to control the valid 
                     #' age of objects which are cached locally. 
                     #' 
                     maxCacheAge = "7 days",
                     #' @field me Users me object from DHIS2
                     me  = NULL,
                     #' @description 
                     #' Create a new DHISLogin object
                     #' @param config_path Configuration file path
                     #' @param baseurl URL to the server. 
                     #' @param username Your username
                     #' @param password Your password.
                     
                     initialize = function(config_path=NA,baseurl = NA, username = NA, password = NA) {
                       if (!is.na(config_path)) {
                         config_file<-LoadConfigFile(config_path)
                         ValidateConfig(config_file)
                         self$config_path<-config_path
                         self$baseurl<-config_file$dhis$baseurl
                         self$username<-config_file$dhis$username
                         self$password<-config_file$dhis$password
                         
                       } else {
                         self$baseurl<-baseurl
                         self$username<-username
                         self$password<-password
                       }
                       self$login() }  ,
                     #' @description 
                     #' Login to a DHIS2 instance and set the httr handle for the specified server.
                     login = function() {
                       url <- URLencode(URL = paste0(self$baseurl, "api/",api_version(),"/me"))
                       httr_handle <-httr::handle("custom")
                       r<-httr::with_config(config = httr::config(httpauth = 1, userpwd = paste0(self$username,":",self$password)),
                                            httr::GET(url, handle = httr_handle),
                                            override = TRUE)
                       self$handle<-httr_handle
                       self$me<-jsonlite::fromJSON(httr::content(r,"text"))
                       self$user_orgunit<-self$me$organisationUnits$id
                       #Don't keep the password any longer than we need to
                       self$password<-NULL
                     }
                   ) )




#' @title ValidateConfig(dhis_config)
#'
#' @param dhis_config Path to a configuration file. 
#'
#' @return Returns nothing. Only errors if the file is not valid. 
#' @export
#'
#' @examples \dontrun{
#'      ValidatConfig("/home/littlebobbytables/creds.json")
#'  }
#' 
ValidateConfig <- function(dhis_config) {
  isBaseURL <- function(x) {
    grepl("^http(?:[s])?://.+/$", x)
  }
  isMissing <- function(x) {
    is.na(x) || missing(x) || x == ""
  }
  
  if (isMissing(dhis_config$dhis$username)) {
    stop("Username cannot by blank.")
  }
  if (isMissing(dhis_config$dhis$password)) {
    stop("Username cannot by blank.")
  }
  if (!isBaseURL(dhis_config$dhis$baseurl)) {
    stop("The base url does not appear to be valid. It should end in /")
  }
}

