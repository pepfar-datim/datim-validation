#' @export
#' @title getCachedObject(sig,wd)
#'
#' @description Internal utility function to retreive a cached object.
#'
#' @param sig MD5 hash of the objects URL reference
#' @param wd By default, the .R_Cache directory in the working directory.
#' @return Returns a cahced data object.
#'
getCachedObject<-function(sig,wd=paste0(getwd(),"/.R_Cache/")) {
  isCached<-file.exists(file=paste0(getwd(),"/.R_Cache/",sig))
  if (isCached) { foo<-readRDS(file=paste0(getwd(),"/.R_Cache/",sig));return(foo) }
  else {return(NULL)}
  
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


saveCachedObject<-function(toCache,sig,wd=paste0(getwd(),"/.R_Cache/")) {
  if (!file.exists(wd)) {dir.create(wd)}
  saveRDS(toCache,file=paste0(wd,sig))
}

#' @export
#' @title clearCache(wd)
#'
#' @description Internal utility function to clear the object cache.
#'
#' @param cache_dir Cache directory path. By default, working directory + ".R_Cache"
#' @param force Boolean (default FALSE) to clear cache without asking the user
#' @return Nothing.
#'
clearCache<-function(cache_dir=paste0(getwd(),"/.R_Cache/"),force=NA) {
  
  
  if ( is.na(force) ) {
resp<-readline(paste("Are you sure you want to delete the directory ", cache_dir,"[Y/N]")) } else {
  resp<-ifelse(force,"Y","N")
}
if (resp == "Y" ) { unlink(cache_dir,recursive=TRUE) } else { print("Aborting.") }
}

get_password <- function() {
  cat("Password: ")
  system("stty -echo")
  a <- readline()
  system("stty echo")
  cat("\n")
  return(a)
}