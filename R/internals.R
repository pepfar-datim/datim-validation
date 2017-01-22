#' @export
#' @title getOU3Map()
#'
#' @description Internal utility function to get a map of organisation unit IDs and their level 3 parent ID
#'
#' @return Returns data frame of id and ou3 UIDs
#'
getOU3Map<-function() {
  
  url<-paste0(getOption("baseurl"),"api/organisationUnits.json?fields=id,path&paging=false&filter=level:ge:3")
  url<-URLencode(url)
  sig<-digest::digest(paste0(url),algo='md5', serialize = FALSE)
  ou_ou3<-getCachedObject(sig)
  if (!is.null(ou_ou3))  { return(ou_ou3) } else {
    
    r<-httr::GET(url,httr::timeout(60))
    
    if (r$status == 200L ) {
      
      foo<- httr::content(r, "text") %>% 
        jsonlite::fromJSON(flatten = TRUE) %>%
        rlist::list.extract('organisationUnits') %>%
        subset(.,nchar(.$path) >= 36) 
      bar<-vapply(foo$path,function(x){unlist(strsplit(x,"[/]"))[[4]]},FUN.VALUE=character(1))
      baz<-data.frame(id=foo$id,ou3=bar,stringsAsFactors=FALSE,row.names = NULL)
      saveCachedObject(baz,sig)
      return(baz) }
    else {
      print(paste("Could not retreive mechanisms",httr::content(r,"text")))
      stop()
    }
  }
}

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
  is_there<-file.exists(x)
  #Age of files in days
  is_fresh<-as.numeric(Sys.time()-file.info(x)$mtime ) < getOption("maxCacheAge")
  return (is_there && is_fresh)
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
  this_obj = file=paste0(getwd(),"/.R_Cache/",sig)
  if (isValidCachedObject(this_obj)) { return(readRDS(this_obj)) }
  else {
    unlink(this_obj)
    return(NULL)}
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

##From https://github.com/sdoyen/r_password_crypt/blob/master/crypt.R
####Encryption functions
# write encrypted data frame to file
write.aes <- function(df,filename, key) {
  require(digest)
  zz <- textConnection("out","w")
  write.csv(df,zz, row.names=F)
  close(zz)
  out <- paste(out,collapse="\n")
  raw <- charToRaw(out)
  raw <- c(raw,as.raw(rep(0,16-length(raw)%%16)))
  aes <- AES(key,mode="ECB")
  aes$encrypt(raw)
  writeBin(aes$encrypt(raw),filename)  
}

# read encypted data frame from file
read.aes <- function(filename,key) {
  require(digest)
  dat <- readBin(filename,"raw",n=1000)
  aes <- digest::AES(key,mode="ECB")
  raw <- aes$decrypt(dat, raw=TRUE)
  txt <- rawToChar(raw[raw>0])
  read.csv(text=txt, stringsAsFactors = F)
}


