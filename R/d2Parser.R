#' @export
#' @title General purpose parsing function for different formats of DHIS2 data
#' 
#' @description d2Parser will parse a compliant DHIS2 XML,JSON or CSV file and transform it into a standard data
#' frame which can be used in subsequent DATIM validation routines
#'
#' @param filename Location of the payload to be imported. Should be a valid DHIS2 import file
#' @param type Type of the file. Should be either xml, json or csv
#' @param base.url Location of the server
#' @param username Server username
#' @param password Server password
#' @param organisationUnit Organisation unit UID of the operating unit
#' @param dataElementIdScheme Should be one of either code, name, shortName or id. If this paramater is "id", 
#' then the Data elements are assumed to be already specififed as UIDs.
#' @param orgUnitIdScheme Should be one of either code, name, shortName or id. If this paramater is "id", 
#' then the organisation units are assumed to be already specififed as UIDs
#' @param idScheme Remapping scheme for category option combos
#' @return Returns a data frame of at least "dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value"
#' 
#' @note function(filename="/home/me/foo.xml",type="xml",base.url="https://www.datim.org/",
#' username="admin",password="district",organisationUnit="Ab12345678",dataElementIdScheme="code",orgUnitIdScheme="code",idScheme="id")
#' Note that all values will be returned as characters.
#'
d2Parser<-function(filename,type,base.url,username,password,organisationUnit,dataElementIdScheme,orgUnitIdScheme,idScheme) {
  valid_type <- type %in% c("xml","json","csv")
if (!valid_type) { print("ERROR:Not a valid file type"); stop()} 

header<-c("dataElement","period","orgUnit","categoryOptionCombo","attributeOptionCombo","value","storedby","lastUpdated","comment")

if ( type == "xml") {
  d<-XML::xmlTreeParse(filename,useInternalNode=TRUE)
  data<-data.frame(t( sapply(XML::xmlRoot(d) [ "dataValue" ], XML::xmlAttrs) ),row.names=seq(1,XML::xmlSize(XML::xmlRoot(d))) )
  #Get all the attributes specified in the 
  data.attrs<-XML::xmlAttrs(XML::xmlRoot(d))
  #Period
  if (!is.na(data.attrs["period"])) { data$period<-data.attrs["period"] }
  if (!is.na(data.attrs["orgUnit"])) { data$orgUnit<-data.attrs["orgUnit"] }
  if (!is.na(data.attrs["attributeOptionCombo"])) { data$attributeOptionCombo<-data.attrs["attributeOptionCombo"] }
  }

if ( type == "csv") { 
  
  data<-read.csv(filename)
  not_empty<-which(plyr::colwise(sum)(plyr::colwise(is.na)(data)) == 0 )
  data<-data[,not_empty]
  names(data)<-header[not_empty]
  
}

if (type == "json") {
  
  j<-jsonlite::fromJSON(txt=filename)
  data<-j$dataValues
  if (!is.null(j[["period"]])) { data$period<-j$period }
  if (!is.null(j[["orgUnit"]])) { data$orgUnit<-j$orgUnit }
  if (!is.null(j[["attributeOptionCombo"]])) { data$attributeOptionCombo<-j$attributeOptionComboid }
}


data<-data[,header[ header %in% names(data)]]
data$value<-as.numeric(as.character(data$value))
if ( orgUnitIdScheme != "id" ) {
data$orgUnit<-remapOUs(data$orgUnit,base.url,username,password,organisationUnit,mode_in=orgUnitIdScheme,mode_out="id") }
if (dataElementIdScheme != "id" ) {
data$dataElement<-remapDEs(data$dataElement,base.url,username,password,mode_in=dataElementIdScheme,mode_out="id") }
if ( idScheme != "id" ) {
data$attributeOptionCombo<-remapMechs(data$attributeOptionCombo,base.url,username,password,organisationUnit=organisationUnit,mode_in=idScheme,mode_out="id") }

#Data frame needs to be completely flattened to characters
data<-plyr::colwise(as.character)(data)

return(data)
}
