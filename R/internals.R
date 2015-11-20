
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