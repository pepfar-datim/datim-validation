#' @title GetCredentialsFromConsole()
#' @description Obtains the credentials information from the console. 
#' @return A list of baseurl, username and password
#'
GetCredentialsFromConsole <- function() {

  s<-list(dhis=list())
  s$dhis$username<-readline("Username: ")
  s$dhis$password<-getPass::getPass()
  s$dhis$baseurl<-readline("Server URL (ends with /): ")
  return(s)
}

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
    dhis_config <- jsonlite::fromJSON(config_path)
    options("config" = config_path)
    return(dhis_config)
  } else {
    stop("You must specify a credentials file!") }
}


#' @title DHISLogin(config_path)
#'
#' @param dhis_config List of DHIS2 credentials
#'
#' @return TRUE if you are able to login to the server. 
#' 
DHISLogin<-function(dhis_config) {
  
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/",api_version(),"/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url ,
                 httr::authenticate(dhis_config$dhis$username, dhis_config$dhis$password),
                 httr::timeout(60))
  if(r$status != 200L){
    stop("Could not authenticate you with the server!")
  } else {
    me <- jsonlite::fromJSON(httr::content(r,as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}




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

#' @export
#' @importFrom utils URLencode
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
#' @param config_path Location of the configuration file to a DHIS2 server. 
#' @return Returns a boolean value indicating the user was able ot successfully 
#' login to the system
#' 
#' @examples 
#'  \dontrun{
#'      loadSecrets("/home/littlebobbytables/creds.json")
#'  }
loadSecrets <- function(config_path = NA) {
  #Load from a file
  if (is.na(config_path)) {
    s <- GetCredentialsFromConsole()
  } else {
    s <- LoadConfigFile(config_path)
  }
  
  ValidateConfig(s)
  options("baseurl" = s$dhis$baseurl)
  options("secrets" = config_path)
  options("maxCacheAge"="7 days")
  DHISLogin(s)
}
