#' @title Function which converts mechanism codes, names or shortnames to UIDs
#' 
#' @description remapMechs should be supplied a vector of data mechanisms (names,codes or shortnames)
#' along with the other required paramaters. It will return a vector of UIDs.
#' 
#'
#' @param mechs_in A vector of data element identifiers (codes, names or shortNames)
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param mode Should be one of code, name or shortName. This is the class we are mapping from to UIDs.
#' @param ou The UID of the operating unit.
#' @return Returns a vector of mechanism UIDs
#' @note
#' remapMechs(foo,"https://www.datim.org","admin","district","code","Ab12345678")
#' will remap mechanisms specified as codes to UIDs
remapMechs<-function(mechs_in,base.url,username,password,mode,ou) {
  is_valid_mode<-mode %in% c("code","name")
  if ( is_valid_mode == FALSE )  {break}
  r<-GET(URLencode(paste0(base.url,"/api/categoryOptions?filter=organisationUnits.id:eq:",ou,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
         authenticate(username,password))
  r<- content(r, "parsed", "application/json")
  mechs<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
  mechs<-colwise(as.character)(mechs)
  cmd<-paste0("mapvalues(mechs_in,mechs$",mode,",mechs$categoryOptionCombos.id,warn_missing = FALSE)")
  as.character(eval(parse(text=cmd))) }