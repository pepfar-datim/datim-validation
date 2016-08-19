#' @export
#' @title Function which converts category option combos from identifier to another 
#' 
#' @description remapCategoryOptionCombos should be supplied a vector of category option combos (names,codes,uids or shortnames)
#' It will return a vector of another class as specified with the mode_out paramater
#' 
#'
#' @param cocs_in A vector of category option combinations
#' @param mode_in Should be one of code, name,shortName or id. This is the class we are mapping from.
#' @param mode_out Should be one of code,name,shortName or id. This is the class we are mapping to..
#' @return Returns a vector of category option combos of the mode_out type.
#' @note
#' remapCategoryOptionCombos(foo,"https://www.datim.org","admin","district","code","name")
#' will remap categoryOptionCombos specified as codes to their corresponding names.

remapCategoryOptionCombos<-function(cocs_in,mode_in,mode_out) {
  is_valid_mode<- (mode_in %in% c("code","name","id","shortName") ) & (mode_out %in% c("code","name","id","shortName") )
  if ( is_valid_mode == FALSE )  { print("Not a valid mode. Must be one of code,name,shortName or id"); stop() }
  cocs<-getCategoryOptionCombosMap()
  cmd<-paste("plyr::mapvalues(cocs_in,cocs$",mode_in,",cocs$",mode_out,",warn_missing = FALSE)")
  as.character(eval(parse(text=cmd))) }
