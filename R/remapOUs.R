#' Function for remapping of organisation units to UIDs
#'
#' @param ous_in A vector of organisation unit identifiers (codes, names or shortNames)
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param mode Should be one of code, name or shortName. This is the class we are mapping from to UIDs.
#' @param ou UID of the Operating Unit/Country
#' @return Returns a vector of organisation unit UIDs
#' @note
#' remapOUs(foo,"https://www.datim.org","admin","district","code","Ab12345678")
#' will remap organisation units specified as codes to UIDs
remapOUs<-function(ous_in,base.url,username,password,mode,ou) {
  is_valid_mode<-mode %in% c("code","name","shortName")
  if ( is_valid_mode == FALSE )  {break}
  r<-GET(URLencode(paste0(base.url,"api/organisationUnits/",ou,".json?includeDescendants=true&filter=level:ge:3&fields=id,code,name,shortName&paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  sites<-ldply(lapply(r$organisationUnits, function(x) t(unlist(x))))
  sites<-colwise(as.character)(sites)
  cmd<-paste0("mapvalues(ous_in,sites$",mode,",sites$id,warn_missing = FALSE)")
  as.character(eval(parse(text=cmd))) }