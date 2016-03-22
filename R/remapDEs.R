#' @export
#' @title Function which converts data element codes, names or shortnames to another class of identifiers
#' 
#' @description remapDes should be supplied a vector of data elements (names,codes or shortnames)
#' along with the other required paramaters. It will return a vector of identifiers of a different class.
#' 
#' 
#' @param des_in A vector of data element identifiers (codes, names or shortNames)
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param mode_in Should be one of code, name or shortName. This is the class we are mapping from to UIDs.
#' @param mode_out Should be one of code,name, shortName or id. This is the class we are mapping to.
#' @return Returns a vector of DATIM uids for each data element which needs to be remapped.
#' @note
#' remapDes(foo,"https://www.datim.org","admin","district","code")
#' will remap data elements specified as codes to UIDs
remapDEs<-function(des_in,base.url,username,password,mode_in="code",mode_out="id"){
  is_valid_mode<- (mode_in %in% c("code","name","shortName","id") )  & ( mode_out %in% c("code","name","shortName","id") )
  if ( is_valid_mode == FALSE )  {print("Not a valid mode. Must be one of code,name,shortName or id"); stop() } else {
  des<-getDataElementMap(base.url,username,password)
  cmd<-paste0("plyr::mapvalues(des_in,des$",mode_in,",des$",mode_out,",warn_missing = FALSE)")
  eval(parse(text=cmd)) } 
    
}
