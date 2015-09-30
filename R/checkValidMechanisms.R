#' @title Function which checks the validity of mechanisms
#' 
#' @description checkValidMechanisms should be supplied a d2Parser compliant data frame along with the operating unit UID
#' along with the other required paramaters. It will return a vector of non-valid mechanism UIDs which were part of the data
#'
#' @param data d2Parser data frame
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param organisationUnit UID of the operating unit
#' @return Returns a vector of non-valid mechanisms
#' @note
#' checkValidMechanisms(foo,"https://www.datim.org","admin","district","Ab12345678")
#' will remap organisation units specified as codes to UIDs
checkValidMechanisms <-
function(data,base.url,username,password,organisationUnit) {
r<-GET(URLencode(paste0(base.url,"api/categoryOptions?filter=organisationUnits.id:eq:",organisationUnit,"&fields=name,id,code,categoryOptionCombos[id]&filter=endDate:gt:2016-09-29&paging=false")),
       authenticate(username,password))
r<- content(r, "parsed", "application/json")
mechs.valid<-ldply(lapply(r$categoryOptions, function(x) t(unlist(x))))
mechs.valid<-colwise(as.character)(mechs.valid)
mechs<-unique(data$attributeOptionCombo)
mechs[!(mechs %in% mechs.valid$categoryOptionCombos.id)] }
