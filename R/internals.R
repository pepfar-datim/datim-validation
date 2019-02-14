#' @export
#' @title isValidCachedObject(sig,wd)
#'
#' @description Internal utility function to determine whether a cached object is stale.
#' If its stale, it will be removed.
#'
#' @param x file path of the cached object
#' @return If the file exists and is not stale, returns TRUE, otherwise FALSE.
#'
isValidCachedObject <- function(x){
  
  if ( is.null(getOption("maxCacheAge")) ) { return(FALSE) } else {
  is_there<-file.exists(x)
  #Age of files in days
  is_fresh<-as.numeric(Sys.time()-file.info(x)$mtime ) < getOption("maxCacheAge")
  return (is_there && is_fresh) }
  
}

#' @export
#' @title getCachedObject(sig,wd)
#'
#' @description Internal utility function to retreive a cached object.
#'
#' @param sig MD5 hash of the objects URL reference
#' @param wd By default, the .R_Cache directory in the working directory.
#' @return Returns a cached data object.
#'
getCachedObject<-function(sig,wd=paste0(getwd(),"/.R_Cache/")) {
  if ( is.null(getOption("maxCacheAge")) ) { return(NULL) } else {
  this_obj = file=paste0(getwd(),"/.R_Cache/",sig)
  if (isValidCachedObject(this_obj)) { return(readRDS(this_obj)) }
  else {
    unlink(this_obj)
    return(NULL)}
  }
}



#' @export
#' @title saveCachedObject(toCache,sig,wd)
#'
#' @description Internal utility function to save an object to the cache.
#'
#' @param toCache Object which should be cached.
#' @param sig MD5 hash of the objects URL reference
#' @param wd By default, the .R_Cache directory in the working directory.
#' @return Nothing.
#'


saveCachedObject <-
  function(toCache, sig, wd = paste0(getwd(), "/.R_Cache/")) {
    if (!is.null(getOption("maxCacheAge"))) {
    if (!file.exists(wd)) {
      dir.create(wd)
    }
    saveRDS(toCache, file = paste0(wd, sig)) }
  }

#' @export
#' @title clearCache(wd)
#'
#' @description Internal utility function to clear the object cache.
#'
#' @param cache_dir Cache directory path. By default, working directory + ".R_Cache"
#' @param force Boolean (default FALSE) to clear cache without asking the user
#' @return Nothing.
#' @examples 
#'  \dontrun{
#'   clearCache(force=TRUE)
#' }
clearCache <-
  function(cache_dir = paste0(getwd(), "/.R_Cache/"),
           force = NA) {
    if (is.na(force)) {
      resp <-
        readline(paste(
          "Are you sure you want to delete the directory ",
          cache_dir,
          "[Y/N]"
        ))
    } else {
      resp <- ifelse(force, "Y", "N")
    }
    if (resp == "Y") {
      unlink(cache_dir, recursive = TRUE)
    } else {
      print("Aborting.")
    }
  }

#' @export
#' @title api_version()
#'
#' @description Internal utility function to get the current API version of the DATIM server
#' @return Version of the API. 
#'
#' 
api_version<-function() { "29" }

#' @export
#' @title api_version()
#'
#' @return Returns a password from a prompt. 
#'
#' 
get_password <- function() {
  cat("Password: ")
  system("stty -echo")
  a <- readline()
  system("stty echo")
  cat("\n")
  return(a)
}